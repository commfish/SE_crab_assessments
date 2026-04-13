### RTMB CSA Functions - Generalized for all Southeast Alaska RKC survey areas ###
## Alex Reich
## Originally created 7/31/25
## Revised/generalized 4/13/26
##
## This file contains:
##   1. basic_pop_model()  - the RTMB objective function (CSA population model)
##   2. run_csa_model()    - wrapper to build, optimize, and extract results
##   3. bootstrap_ci()     - parametric bootstrap for non-negative confidence intervals
##   4. extract_results()  - pull optimized results into a tidy data frame
##
## USAGE:
##   In each area-specific script (e.g., RTMB_CSA_Juneau_2025.R):
##     1. Prepare `data` list and `pars` list as before
##     2. source("RTMB_CSA_functions.R")
##     3. results <- run_csa_model(data, pars, map)
##     4. ci      <- bootstrap_ci(results$pop_mod, data, pars, map, n_boot = 1000)
##
## DEPENDENCIES: RTMB, TMBhelper, abind
###############################################################################

library(RTMB)
library(TMBhelper)
library(abind)


# =============================================================================
# 1. basic_pop_model  --  The RTMB objective function
# =============================================================================
#' @description 3-stage catch-survey analysis (CSA) population model for
#'   Southeast Alaska red king crab. Stages are pre-recruit, recruit, and
#'   post-recruit. The model predicts CPUE by stage, converts to biomass
#'   using stage-specific weights, and evaluates a weighted normal likelihood
#'   against observed survey CPUE.
#'
#' @details This function is meant to be passed to RTMB::MakeADFun(). It
#'   expects `data` to exist in the calling environment (standard RTMB pattern
#'   via getAll). The function is area-agnostic: all area-specific information
#'   enters through the `data` and `pars` lists prepared in each area script.
#'
#' @param pars Named list of parameters (see area scripts for structure).
#'
#' Required data list elements:
#'   YEARS             - integer vector, all survey years (including gap years)
#'   CATCH             - numeric vector, commercial catch in numbers (0 for no catch)
#'   survey_data       - 3D array [n_obs_years x 3 x 3]:
#'                         dim 1 = observed years (NA rows already removed)
#'                         dim 2 = (Year, CPUE, CV)
#'                         dim 3 = (Stage_1=prerec, Stage_2=rec, Stage_3=postrec)
#'   wt_prerec         - numeric vector (length n_yrs), pre-recruit weight (lbs)
#'   wt_legal          - numeric vector (length n_yrs), legal weight (lbs)
#'   wt_mature         - numeric vector (length n_yrs), mature weight (lbs)
#'   SURVEY_TAU        - numeric vector, fractional year between successive surveys
#'   CATCH_SURVEY_TAU  - numeric vector, fractional year between catch and next survey
#'
#' Required parameter list elements:
#'   ln_mean_rec           - log mean recruitment CPUE
#'   ln_Eps_R              - log recruitment deviates (length n_yrs)
#'   ln_q                  - log survey catchability
#'   ln_T12                - log transition rate (pre-recruit to recruit)
#'   S                     - natural mortality rate (fixed via map)
#'   ln_sigma_survey       - log observation error SD
#'   ln_init_rec_cpue      - log initial recruit CPUE
#'   ln_init_postrec_cpue  - log initial post-recruit CPUE

basic_pop_model <- function(pars) {
  
  # Pull parameters and data into local scope ---------------------------------
  RTMB::getAll(pars, data)
  
  # Dimensions ----------------------------------------------------------------
  n_stages <- 3L
  n_yrs    <- length(YEARS)
  
  # Containers ----------------------------------------------------------------
  PredSrvCPUE <- array(0, dim = c(n_yrs, n_stages))  # predicted CPUE by stage
  
  PredSrvIdx  <- array(0, dim = c(n_yrs, n_stages))  # predicted biomass indices
  SrvIdx_nLL  <- rep(0, n_stages)                     # neg-log-lik per stage
  jnLL        <- 0                                     # joint neg-log-lik
  
  # Parameter transformations (log -> natural scale) --------------------------
  mean_rec         <- exp(ln_mean_rec)
  q                <- exp(ln_q)
  T12              <- exp(ln_T12)
  sigma_survey     <- exp(ln_sigma_survey)
  init_rec_cpue    <- exp(ln_init_rec_cpue)
  init_postrec_cpue <- exp(ln_init_postrec_cpue)
  Eps_R            <- exp(ln_Eps_R)
  
  # Convert CV column in survey_data to likelihood weights --------------------
  # Weight = 1 / (2 * (sqrt(log(1 + CV^2)))^2)   i.e., inverse of log-normal variance
  WEIGHTS <- 1.0 / (2 * sqrt(log(1.0 + survey_data[, 3, ]^2))^2)
  survey_data <- abind(survey_data, WEIGHTS, along = 2)
  # survey_data is now [n_obs x 4 x 3]: (Year, CPUE, CV, Weight)
  
  # ===========================================================================
  # Population dynamics (CPUE prediction)
  # ===========================================================================
  
  # Year 1 initialization ----------------------------------------------------
  PredSrvCPUE[1, 1] <- Eps_R[1] * mean_rec          # pre-recruit
  PredSrvCPUE[1, 2] <- init_rec_cpue                 # recruit
  PredSrvCPUE[1, 3] <- init_postrec_cpue             # post-recruit
  
  # Forward projection --------------------------------------------------------
  for (t in 2:n_yrs) {
    # Pre-recruits: recruitment deviate * mean recruitment
    PredSrvCPUE[t, 1] <- Eps_R[t] * mean_rec
    
    # Recruits: transition from last year's pre-recruits
    PredSrvCPUE[t, 2] <- T12 * PredSrvCPUE[t - 1, 1]
    
    # Post-recruits: survivors of last year's recruits + post-recruits, minus catch
    # Floor at small positive value to prevent negative CPUE (important for
    # areas like Excursion Inlet where populations can be very low)
    PredSrvCPUE[t, 3] <- max(
      (PredSrvCPUE[t - 1, 2] + PredSrvCPUE[t - 1, 3]) *
        exp(-S * SURVEY_TAU[t]) -
        (q * CATCH[t - 1] * exp(CATCH_SURVEY_TAU[t] * -S)),
      0.0001
    )
  }
  
  # ===========================================================================
  # Biomass indices
  # ===========================================================================
  # Pre-recruit biomass
  PredSrvIdx[, 1] <- (PredSrvCPUE[, 1] / q) * wt_prerec
  
  # Legal biomass (recruit + post-recruit)
  PredSrvIdx[, 2] <- ((PredSrvCPUE[, 2] + PredSrvCPUE[, 3]) / q) * wt_legal
  
  # Mature biomass = pre-recruit + legal
  PredSrvIdx[, 3] <- PredSrvIdx[, 1] + PredSrvIdx[, 2]
  
  # ===========================================================================
  # Likelihood
  # ===========================================================================
  # For each stage, match observed survey years to the full year index,
  # then evaluate a weighted normal log-likelihood.
  pred <- numeric(nrow(survey_data[, , 1]))
  
  for (h in 1:n_stages) {
    for (y in 1:nrow(survey_data[, , h])) {
      y_row   <- which(YEARS == survey_data[y, 1, h])
      pred[y] <- PredSrvCPUE[y_row, h]
    }
    # Weighted negative log-likelihood (normal)
    SrvIdx_nLL[h] <- -sum(
      dnorm(survey_data[, 2, h], pred, sigma_survey, TRUE) * survey_data[, 4, h]
    )
  }
  
  # Joint negative log-likelihood
  jnLL <- sum(SrvIdx_nLL)
  
  # ===========================================================================
  # Report section
  # ===========================================================================
  RTMB::REPORT(sigma_survey)
  RTMB::REPORT(PredSrvIdx)
  RTMB::REPORT(PredSrvCPUE)
  RTMB::REPORT(jnLL)
  RTMB::REPORT(q)
  RTMB::REPORT(T12)
  RTMB::REPORT(Eps_R)
  RTMB::REPORT(survey_data)
  
  return(jnLL)
}
# END basic_pop_model


# =============================================================================
# 2. run_csa_model  --  Convenience wrapper: build AD object, optimize, extract
# =============================================================================
#' @param data  List of data (see basic_pop_model docs)
#' @param pars  List of starting parameter values
#' @param map   List of factors for fixing parameters (e.g., map$S = factor(NA))
#' @param newtonsteps Integer, number of Newton steps after optimization (default 1)
#' @param silent Logical, suppress RTMB printing (default TRUE)
#'
#' @return List with components:
#'   pop_mod   - the MakeADFun object
#'   opt       - optimization result from fit_tmb
#'   sdrep     - sdreport object
#'   report    - report list at optimized parameters
#'   results   - tidy data frame of point estimates (from extract_results)

run_csa_model <- function(data, pars, map,
                          newtonsteps = 1, silent = TRUE) {
  
  # Make the data available in the environment that basic_pop_model will search
  # RTMB::getAll looks in the parent environment, so we assign data there.
  # To keep things clean, we use an environment trick:
  model_env <- new.env(parent = globalenv())
  model_env$data <- data
  
  # Create a version of basic_pop_model that has access to `data`
  model_fn <- basic_pop_model
  environment(model_fn) <- model_env
  
  # Build AD function
  pop_mod <- RTMB::MakeADFun(model_fn, parameters = pars, map = map,
                             silent = silent)
  
  # Quick sanity checks
  init_fn <- pop_mod$fn(pop_mod$par)
  init_gr <- pop_mod$gr(pop_mod$par)
  
  if (any(is.na(init_fn)) || any(is.nan(init_fn))) {
    warning("Objective function returns NA/NaN at initial parameters. Check data and starting values.")
  }
  if (any(is.na(init_gr)) || any(is.nan(init_gr))) {
    warning("Gradient returns NA/NaN at initial parameters. Check data and starting values.")
  }
  
  # Optimize
  opt <- TMBhelper::fit_tmb(
    obj          = pop_mod,
    fn           = pop_mod$fn,
    gr           = pop_mod$gr,
    newtonsteps  = newtonsteps,
    getsd        = TRUE
  )
  
  # SD report
  invisible(capture.output(sdrep <- sdreport(pop_mod))) #AGR recent change
  
  # Extract report at optimized parameters
  report <- pop_mod$report(pop_mod$env$last.par.best)
  
  # Build tidy results
  results <- extract_results(report, data)
  
  return(list(
    pop_mod = pop_mod,
    opt     = opt,
    sdrep   = sdrep,
    report  = report,
    results = results
  ))
}


# =============================================================================
# 3. extract_results  --  Pull model report into a tidy data frame
# =============================================================================
#' @param report  List from pop_mod$report(pop_mod$env$last.par.best)
#' @param data    The data list used in model fitting
#'
#' @return Data frame with columns:
#'   year, sigma_survey, q, T12,
#'   prerec_cpue, rec_cpue, postrec_cpue,
#'   prerec_biomass, legal_biomass, mature_biomass,
#'   Eps_R

extract_results <- function(report, data) {
  
  n_yrs <- length(data$YEARS)
  
  df <- data.frame(
    year              = data$YEARS,
    sigma_survey      = rep(report$sigma_survey, n_yrs),
    q                 = rep(report$q, n_yrs),
    T12               = rep(report$T12, n_yrs),
    prerec_cpue       = report$PredSrvCPUE[, 1],
    rec_cpue          = report$PredSrvCPUE[, 2],
    postrec_cpue      = report$PredSrvCPUE[, 3],
    prerec_biomass    = report$PredSrvIdx[, 1],
    legal_biomass     = report$PredSrvIdx[, 2],
    mature_biomass    = report$PredSrvIdx[, 3],
    Eps_R             = report$Eps_R
  )
  
  return(df)
}


# =============================================================================
# 4. bootstrap_ci  --  Parametric bootstrap for CPUE and biomass CIs
# =============================================================================
#' @description Generates bootstrap confidence intervals on predicted CPUE and
#'   biomass that are guaranteed non-negative (by construction, since each
#'   bootstrap replicate runs the full model which floors post-recruit CPUE
#'   at 0.0001). This replaces the +/- 1.96*SE approach that could produce
#'   negative lower bounds.
#'
#' @details The bootstrap works as follows:
#'   1. At the MLE, extract the estimated sigma_survey and predicted CPUE.
#'   2. For each bootstrap replicate:
#'      a. Simulate new observed CPUE data by adding normal noise (sigma_survey)
#'         to the predicted CPUE, at only the observed-year positions.
#'      b. Re-fit the model to the simulated data.
#'      c. Store the resulting predicted CPUE and biomass.
#'   3. Compute percentile-based confidence intervals across replicates.
#'
#' @param pop_mod    The fitted MakeADFun object (after optimization)
#' @param data       The original data list
#' @param pars       The original starting parameter list
#' @param map        The map list for fixed parameters
#' @param n_boot     Number of bootstrap replicates (default 1000)
#' @param ci_level   Confidence level (default 0.95 for 95% CI)
#' @param seed       Random seed for reproducibility (default 42)
#' @param newtonsteps Newton steps per bootstrap fit (default 1)
#' @param verbose    Print progress? (default TRUE)
#'
#' @return List with:
#'   ci_df       - data frame with year and lower/upper bounds for each quantity
#'   boot_cpue   - array [n_yrs x 3 x n_boot] of bootstrapped CPUE
#'   boot_bio    - array [n_yrs x 3 x n_boot] of bootstrapped biomass
#'   n_success   - number of successful bootstrap fits

bootstrap_ci <- function(pop_mod, data, pars, map,
                         n_boot = 1000, ci_level = 0.95,
                         seed = 42, newtonsteps = 1,
                         verbose = TRUE) {
  
  set.seed(seed)
  
  n_yrs    <- length(data$YEARS)
  n_stages <- 3L
  
  # Get MLE report for the "true" predicted values
  mle_report <- pop_mod$report(pop_mod$env$last.par.best)
  sigma_mle  <- mle_report$sigma_survey
  pred_cpue  <- mle_report$PredSrvCPUE  # [n_yrs x 3]
  
  # Identify which years have observed data (from the survey_data array)
  obs_years <- data$survey_data[, 1, 1]  # observed years (same across stages after NA removal)
  
  # Storage
  boot_cpue <- array(NA, dim = c(n_yrs, n_stages, n_boot))
  boot_bio  <- array(NA, dim = c(n_yrs, n_stages, n_boot))
  n_success <- 0
  n_fail    <- 0
  
  # Use the MLE parameters as starting values for bootstrap fits
  # (much faster convergence than original starting values)
  mle_pars <- pars
  best_par <- pop_mod$env$last.par.best
  par_names <- names(pop_mod$par)
  
  # Reconstruct the full parameter list from the optimized vector
  # This handles mapped (fixed) parameters correctly
  for (nm in names(pars)) {
    if (nm %in% names(map) && all(is.na(map[[nm]]))) {
      # Fixed parameter - keep original
      mle_pars[[nm]] <- pars[[nm]]
    }
  }
  # For estimated parameters, update from the optimized values
  # We'll use the environment's last.par.best which includes all params
  mle_fullpar <- pop_mod$env$last.par.best
  # Map the full par vector back to the list structure
  # This is a bit involved; simpler approach: just use pars as starting values
  
  # and let the optimizer do its thing (it's fast from near the MLE).
  
  for (b in 1:n_boot) {
    if (verbose && b %% 100 == 0) {
      cat(sprintf("Bootstrap replicate %d / %d  (successes: %d, failures: %d)\n",
                  b, n_boot, n_success, n_fail))
    }
    
    # Simulate new survey data ------------------------------------------------
    sim_survey <- data$survey_data  # copy the original [n_obs x 3 x 3]
    
    for (h in 1:n_stages) {
      for (y in 1:nrow(sim_survey[, , h])) {
        yr_idx <- which(data$YEARS == sim_survey[y, 1, h])
        # Simulate observed CPUE: predicted + normal error, floored at 0
        sim_val <- rnorm(1, mean = pred_cpue[yr_idx, h], sd = sigma_mle)
        sim_survey[y, 2, h] <- max(sim_val, 0.001)  # floor at small positive
      }
    }
    
    # Build simulated data list
    sim_data <- data
    sim_data$survey_data <- sim_survey
    
    # Fit model to simulated data
    tryCatch({
      sim_result <- run_csa_model(
        data        = sim_data,
        pars        = pars,      # use original starting values (robust)
        map         = map,
        newtonsteps = newtonsteps,
        silent      = TRUE
      )
      
      # Check convergence (max gradient < 0.01 is a loose check for bootstrap)
      max_grad <- max(abs(sim_result$pop_mod$gr(sim_result$pop_mod$env$last.par.best)))
      if (max_grad < 0.01 && !is.na(max_grad)) {
        n_success <- n_success + 1
        boot_cpue[, , n_success] <- sim_result$report$PredSrvCPUE
        boot_bio[, , n_success]  <- sim_result$report$PredSrvIdx
      } else {
        n_fail <- n_fail + 1
      }
    }, error = function(e) {
      n_fail <<- n_fail + 1
    })
  }
  
  if (verbose) {
    cat(sprintf("\nBootstrap complete: %d / %d replicates converged.\n",
                n_success, n_boot))
  }
  
  if (n_success < 10) {
    warning("Fewer than 10 bootstrap replicates converged. CIs will be unreliable.")
  }
  
  # Trim arrays to successful replicates
  boot_cpue <- boot_cpue[, , 1:n_success, drop = FALSE]
  boot_bio  <- boot_bio[, , 1:n_success, drop = FALSE]
  
  # Compute percentile CIs ---------------------------------------------------
  alpha <- (1 - ci_level) / 2
  probs <- c(alpha, 1 - alpha)
  
  ci_df <- data.frame(
    year = data$YEARS,
    # CPUE CIs
    prerec_cpue_lower  = apply(boot_cpue[, 1, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    prerec_cpue_upper  = apply(boot_cpue[, 1, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE),
    rec_cpue_lower     = apply(boot_cpue[, 2, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    rec_cpue_upper     = apply(boot_cpue[, 2, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE),
    postrec_cpue_lower = apply(boot_cpue[, 3, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    postrec_cpue_upper = apply(boot_cpue[, 3, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE),
    # Biomass CIs
    prerec_bio_lower   = apply(boot_bio[, 1, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    prerec_bio_upper   = apply(boot_bio[, 1, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE),
    legal_bio_lower    = apply(boot_bio[, 2, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    legal_bio_upper    = apply(boot_bio[, 2, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE),
    mature_bio_lower   = apply(boot_bio[, 3, , drop = FALSE], 1, quantile, probs = probs[1], na.rm = TRUE),
    mature_bio_upper   = apply(boot_bio[, 3, , drop = FALSE], 1, quantile, probs = probs[2], na.rm = TRUE)
  )
  
  return(list(
    ci_df     = ci_df,
    boot_cpue = boot_cpue,
    boot_bio  = boot_bio,
    n_success = n_success
  ))
}


# =============================================================================
# 5. merge_results_ci  --  Combine point estimates with bootstrap CIs
# =============================================================================
#' @param results  Data frame from extract_results() or run_csa_model()$results
#' @param ci       List from bootstrap_ci()
#'
#' @return Data frame with point estimates and lower/upper CI columns

merge_results_ci <- function(results, ci) {
  merged <- merge(results, ci$ci_df, by = "year", all.x = TRUE)
  return(merged)
}

#AGR flag- QC the bootstrap, make sure it calls the population model