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

#flag - q estimated differently from excel because RTMB model handles the pre-recruits differently
###############################################################################

library(RTMB)
library(TMBhelper)
library(abind)


# =============================================================================
# 1. basic_pop_model  --  The RTMB objective function
# =============================================================================

basic_pop_model <- function(pars) {
  
  # Pull parameters and data into local scope ---------------------------------
  RTMB::getAll(pars, data)
  
  # Dimensions ----------------------------------------------------------------
  n_stages <- 3
  n_yrs    <- length(YEARS)
  
  # Containers ----------------------------------------------------------------
  PredSrvCPUE <- array(0, dim = c(n_yrs, n_stages))  # predicted CPUE by stage
  PredSrvIdx  <- array(0, dim = c(n_yrs, n_stages))  # predicted biomass indices
  SrvIdx_nLL  <- rep(0, n_stages)  # 3 stages                   # neg-log-lik per stage
  jnLL        <- 0                                     # joint neg-log-lik
  
  # Parameter transformations (log -> natural scale) --------------------------
  #mean_rec         <- exp(ln_mean_rec) #turn off for excel loyal
  q                <- exp(ln_q)
  T12              <- exp(ln_T12)
  #sigma_survey     <- exp(ln_sigma_survey) #off for excel loyal
  init_rec_cpue    <- exp(ln_init_rec_cpue)
  init_postrec_cpue <- exp(ln_init_postrec_cpue)
#  Eps_R             <- exp(ln_Eps_R)          # turn off in excel loyal
  prerec_cpue       <- exp(ln_prerec_cpue)    # NEW: free pre-recruit CPUE per year
  
  
  # Convert CV column in survey_data to likelihood weights --------------------
  # WEIGHTS = 1 / (2 * (sqrt(log(1 + CV^2)))^2)   i.e., inverse of log-normal variance
  WEIGHTS <- 1.0 / (2 * sqrt(log(1.0 + survey_data[, 3, ]^2))^2)   #turn CV into weights - I made it so years formerly weighted 0 are now weighted 0.1- agr 25
  survey_data <- abind(survey_data, WEIGHTS, along = 2)

  # ===========================================================================
  # Population dynamics (CPUE prediction)
  # ===========================================================================
  
  # Year 1 initialization ----------------------------------------------------
  PredSrvCPUE[1, 1] <-   prerec_cpue[1]          # CHANGED: free parameter exp(ln_prerec_cpue[1]) #CHANGED  # free parameter year 1 # Eps_R[1] * mean_rec          # pre-recruit
  PredSrvCPUE[1, 2] <- init_rec_cpue                 # recruit
  PredSrvCPUE[1, 3] <- init_postrec_cpue             # post-recruit
  
  # Forward projection --------------------------------------------------------
  for (t in 2:n_yrs) {
    
    # Pre-recruits: free parameter per year (matches Excel's optimized column)
    PredSrvCPUE[t, 1] <- prerec_cpue[t]        # CHANGED: was Eps_R[t] * mean_rec
    
    # Recruits: transition from last year's pre-recruits
    PredSrvCPUE[t, 2] <- T12 * PredSrvCPUE[t - 1, 1]
    
    # Post-recruits: survivors of last year's recruits + post-recruits, minus catch
    # Floor at small positive value to prevent negative CPUE (important for
    # areas like Excursion Inlet where populations can be very low)
    PredSrvCPUE[t, 3] <- max( #FLAG-- CHECK THAT THIS PART IS DONE OK - AGR, OR IF I NEED SOMETHING ELSE
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
  
  #turn weights of 0.1 into 0 
  for (s in 1:3) { #band-aid
    survey_data[survey_data[, 4, s] < 0.2, 4, s] <- 0
  }
  
  for (h in 1:n_stages) {
    for (y in 1:nrow(survey_data[, , h])) {
      y_row   <- which(YEARS == survey_data[y, 1, h])
      pred[y] <- PredSrvCPUE[y_row, h]
    }
    # Weighted negative log-likelihood (normal)
    #SrvIdx_nLL[h] <- -sum(
     # dnorm(survey_data[, 2, h], pred, sigma_survey, TRUE) * survey_data[, 4, h] #might want a way to make weights of 0.1 actually 0
      #dnorm(survey_data[, 2, h], pred, sigma_survey*survey_data[, 3, h], TRUE)
    #) #agr turned off in excel loyal sumsq version
    
    SrvIdx_nLL[h] <- sum(survey_data[, 4, h] * (survey_data[, 2, h] - pred)^2)
    
    #log liklelihood formula
    #SrvIdx_nLL[h] <- -sum(dnorm(log(obs), log(pred), sqrt(log(1 + CV^2)), log = TRUE)
    
    #weighted sum of squares
  #  SrvIdx_nLL[h] <- sum( #removed the negative sign
      #dnorm(survey_data[, 2, h], pred, sigma_survey, TRUE) * survey_data[, 4, h] AGR off- will be the CV version if I incorporate that later
   #   survey_data[, 4, h] * (survey_data[, 2, h] - pred)^2 #weights times squared residuals. ln_sigma_survey needs to be mapped for this to make sense
    #)
    
  }
  
  #note- can add recruitment in here later (see RTMB_CSA_Juneau_2025.R for draft)
  
  # Joint negative log-likelihood (or positive weighted sum of squares, pending what is turned on above)
  jnLL <- sum(SrvIdx_nLL)
  
  # ===========================================================================
  # Report section
  # ===========================================================================
  #RTMB::REPORT(sigma_survey) #off for excel loyal
  RTMB::REPORT(PredSrvIdx)
  RTMB::REPORT(PredSrvCPUE)
  RTMB::REPORT(jnLL)
  RTMB::REPORT(q)
  RTMB::REPORT(T12)
  #RTMB::REPORT(Eps_R) #turn off for excel loyal
  RTMB::REPORT(prerec_cpue)     # NEW: report the free pre-recruit estimates
  RTMB::REPORT(survey_data)
  
  return(jnLL)
}


# =============================================================================
# 3. extract_results  --  Pull model report into a tidy data frame. Yes it is out of numerical order
# =============================================================================


extract_results <- function(report, data) {
  
  n_yrs <- length(data$YEARS)
  
  df <- data.frame(
    year              = data$YEARS,
    #sigma_survey      = rep(report$sigma_survey, n_yrs), #off for excel loyal
    q                 = rep(report$q, n_yrs),
    T12               = rep(report$T12, n_yrs),
    prerec_cpue       = report$PredSrvCPUE[, 1],
    rec_cpue          = report$PredSrvCPUE[, 2],
    postrec_cpue      = report$PredSrvCPUE[, 3],
    prerec_biomass    = report$PredSrvIdx[, 1],
    legal_biomass     = report$PredSrvIdx[, 2],
    mature_biomass    = report$PredSrvIdx[, 3]#, #comma off for excel-loyal
    #Eps_R             = report$Eps_R #turn off for excel loyal
  )
  
  return(df)
}


# =============================================================================
# 2. run_csa_model  --  Convenience wrapper: build AD object, optimize, extract
##QC'ed and looks good
# =============================================================================

run_csa_model <- function(data, pars, map,
                          newtonsteps = 1, silent = TRUE) {
  
  # Make the data available in the environment that basic_pop_model will search
  # RTMB::getAll looks in the parent environment, so we assign data there.
  # To keep things clean, we use an environment trick:
  #model_env <- new.env(parent = globalenv()) #agr off test
  #model_env$data <- data #AGR off test
  
  # Create a version of basic_pop_model that has access to `data`
  #model_fn <- basic_pop_model
  #environment(model_fn) <- model_envAGR off test
  
  # Build AD function
  pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map = map, #AGR switch pop dy model name
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
 suppressMessages( #agr add
  opt <- TMBhelper::fit_tmb(
    obj          = pop_mod,
    fn           = pop_mod$fn,
    gr           = pop_mod$gr,
    newtonsteps  = newtonsteps,
    getsd        = FALSE, #AGR recent change
    control = list(trace = 0) #AGR off
  )
 )#agr add

  
  # SD report
 # invisible(capture.output(sdrep <- sdreport(pop_mod, getReportCovariance=TRUE))) #AGR recent change
  
  # Extract report at optimized parameters
  report <- pop_mod$report(pop_mod$env$last.par.best)
  
  sdrep <- sdreport(pop_mod, getReportCovariance=TRUE)
  
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
# 4. bootstrap_ci  --  Parametric bootstrap for CPUE and biomass CIs
# =============================================================================
bootstrap_ci <- function(pop_mod, data, pars, map,
                         n_boot = 1000, ci_level = 0.95,
                         seed = 42, newtonsteps = 1,
                         verbose = TRUE, sigma_manual = 0.5) { #added sigma_manual for the excel-loyal version
  
  set.seed(seed)
  
  n_yrs    <- length(data$YEARS)
  n_stages <- 3
  
  # Get MLE report for the "true" predicted values
  mle_report <- pop_mod$report(pop_mod$env$last.par.best)
  #sigma_mle  <- mle_report$sigma_survey #off for excel loyal
  sigma_mle  <- sigma_manual #added for excel loyal
  pred_cpue  <- mle_report$PredSrvCPUE  # [n_yrs x 3]
  
  # Identify which years have observed data (from the survey_data array)
  obs_years <- data$survey_data[, 1, 1]  # observed years (same across stages after NA removal)
  
  # Storage
  boot_cpue <- array(NA, dim = c(n_yrs, n_stages, n_boot))
  boot_bio  <- array(NA, dim = c(n_yrs, n_stages, n_boot))
  n_success <- 0
  n_fail    <- 0
  
  
  # optimizer
  
  mle_pars <- pop_mod$env$parList(pop_mod$env$last.par.best) #RECENT ADD
  
  for (b in 1:n_boot) {
    if (verbose && (b %% 100 == 0 || b == 1)) { #print progress every 100 iterations
      cat(sprintf("Bootstrap replicate %d / %d  (successes: %d, failures: %d)\n", #part of the print progress
                  b, n_boot, n_success, n_fail))
    }
    
    # Simulate new survey data ------------------------------------------------
    sim_survey <- data$survey_data  # copy the original [n_obs x 3 x 3]
    
    for (h in 1:n_stages) {
      for (y in 1:nrow(sim_survey[, , h])) {
        yr_idx <- which(data$YEARS == sim_survey[y, 1, h])
        # Simulate observed CPUE: predicted + normal error, floored at 0
        sim_val <- rnorm(1, mean = pred_cpue[yr_idx, h], sd = sigma_mle) #draws one random obs from normal(pred CPUE, sd pred CPUE)
        sim_survey[y, 2, h] <- max(sim_val, 0.001)  # floor at small positive ; makes simulated values in same strucuture as survey
      }
    }
    
    # Build simulated data list
    sim_data <- data
    sim_data$survey_data <- sim_survey
    
    # Fit model to simulated data (lightweight — no sdreport or sanity checks) 
    tryCatch({#try turning off?? AGR- ok I'll keep it.
      # Set up environment for data scoping
      sim_env <- new.env(parent = globalenv())
      sim_env$data <- sim_data
      sim_fn <- basic_pop_model
      environment(sim_fn) <- sim_env
      
      # Build AD object
      sim_mod <- RTMB::MakeADFun(sim_fn, parameters = mle_pars, map = map, #mle_pars changed from pars recently
                                 silent = TRUE)
      
      # Optimize (skip sdreport — we only need the report)
      sim_opt <- suppressMessages(
        TMBhelper::fit_tmb(
        obj         = sim_mod,
       fn          = sim_mod$fn,
        gr          = sim_mod$gr,
        newtonsteps = newtonsteps,
        getsd       = FALSE ,  # no sdreport needed for bootstrap
        control = list(trace = 0)
      )
      )
      
      # Check convergence (max gradient < 0.01 is a loose check for bootstrap)
      # NOTE: $gr() with no args evaluates at the last optimized point.
      # Do NOT pass $env$last.par.best — it includes fixed params and can
      # cause dimension mismatches or silently return wrong gradients.
      max_grad <- max(abs(sim_mod$gr()))
      if (max_grad < 0.01 && !is.na(max_grad)) {
        sim_report <- sim_mod$report(sim_mod$env$last.par.best)
        n_success <- n_success + 1
        boot_cpue[, , n_success] <- sim_report$PredSrvCPUE
        boot_bio[, , n_success]  <- sim_report$PredSrvIdx
      } else {
        n_fail <- n_fail + 1
      }
    }, error = function(e) {
      n_fail <<- n_fail + 1
      if (verbose) cat("Replicate", b, "error:", conditionMessage(e), "\n") #print error message if it fails
    })
  } #trycatch end 
  
  if (verbose) {
    cat(sprintf("\nBootstrap complete: %d / %d replicates converged.\n",
                n_success, n_boot))
  }
  
  if (n_success < 10) {
    warning("Fewer than 10 bootstrap replicates converged. CIs will be unreliable.")
  }
  
  if (n_success == 0) stop("No bootstrap replicates converged.")  # ADDed
  
  
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

merge_results_ci <- function(results, ci) {
  merged <- merge(results, ci$ci_df, by = "year", all.x = TRUE)
  return(merged)
}

