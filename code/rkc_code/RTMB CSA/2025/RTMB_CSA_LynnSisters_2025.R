### Lynn Sisters RTMB CSA 2025 ###
## Alex Reich
### 4/20/26 - building the base off excursion for future years
##


##############################################################################

#BASIC STRUCTURE
##1. Setup and data wrangling (area-specific)
##2. Build data and pars lists
##3. Source generalized functions and run model
##4. Bootstrap CIs (non-negative)
##5. Comparison plots (RTMB vs Excel vs Observed)
##6. Outputs (Save them!!)

##################################################################################

#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)
library(abind)
library(patchwork)

set.seed(100)

# Source the generalized CSA functions
source("code/rkc_code/RTMB CSA/RTMB_CSA_generalize_4.R")

# ============================================================================
# AREA-SPECIFIC SETTINGS
# ============================================================================
area_name <- "LS"            # short label for filenames and plot titles
cur_yr    <- 2025            # current analysis year

#flag for lynn 2025 shakedown- I used the 2025 starting values. I think this is ok

# ============================================================================
# DATA WRANGLING (area-specific -- will be streamlined in the future)
# ============================================================================

df <- read.csv("CSA_excel/Lynn sisters 2025 input data RTMB.csv")

this_year_cpue <- read.csv("results/rkc/LynnSisters/2025/LS_CPUE_2025.csv") %>% #results from the CPUE standardization and summary script
  filter(Year == cur_yr)
this_year_crab_weights <- read.csv("results/rkc/LynnSisters/2025/maleweights.csv") %>% #from that same R summary
  filter(Year == cur_yr)

df$Catch..Number.[46] <- 0       # No commercial catch in Lynn sisters in 24/25(fishery closed)
df$Catch.Mid.Date[46] <- NA #AGR HERE- Need to change Lynn sisters blanks to match excursion blanks. Compare CSV;s

df <- df %>% select(Survey.Year, Catch.Season, Pre.recruit, Recruit, Post.recruit,
                    Catch..Number., Catch.Mid.Date, Survey.Mid.Date,
                    Mature.Weight, Legal.Weight, Prerecruit.Weight, Weight,
                    Estimated.Recruits, Estimated.Postrecruits)

# Add current year row 
new_line <- data.frame(
  Survey.Year        = cur_yr,
  Catch.Season       = paste0(cur_yr, "/", cur_yr + 1),
  Pre.recruit        = this_year_cpue$Pre_Recruit_wt,
  Recruit            = this_year_cpue$Recruit_wt,
  Post.recruit       = this_year_cpue$Post_Recruit_wt,
  Catch..Number.     = NA,
  Catch.Mid.Date     = NA,
  Survey.Mid.Date    = "19-Jul-25",  # LS-specific survey date (automate later)
  Mature.Weight      = this_year_crab_weights$mature_lbs,
  Legal.Weight       = this_year_crab_weights$legal_lbs,
  Prerecruit.Weight  = this_year_crab_weights$prer_lbs,
  Estimated.Recruits     = 0,
  Estimated.Postrecruits = 0,
  Weight             = 12
)

df <- rbind(df, new_line)

# --- Process vectors for RTMB ---
YEARS   <- df$Survey.Year
WEIGHTS <- df$Weight
WEIGHTS[WEIGHTS == 0] <- 0.1 

# Convert weights to CV
CV <- sqrt(exp(1 / (2 * WEIGHTS)) - 1)

CATCH <- as.numeric(gsub(",", "", df$Catch..Number.))
CATCH[is.na(CATCH)] <- 0 #ok here?? AGR FLAG

CATCH_MIDDATE  <- as.Date(df$Catch.Mid.Date, format = "%d-%b-%y")
REF_DATE       <- CATCH_MIDDATE[1]
CATCH_MIDDATE  <- as.numeric(CATCH_MIDDATE - REF_DATE)

SURVEY_MIDDATE <- as.Date(df$Survey.Mid.Date, format = "%d-%b-%y")
SURVEY_MIDDATE <- as.numeric(SURVEY_MIDDATE - REF_DATE)
for (i in 2:length(SURVEY_MIDDATE)) {
  if (is.na(SURVEY_MIDDATE[i])) SURVEY_MIDDATE[i] <- SURVEY_MIDDATE[i - 1]
}

# EI-specific fix: year 34 has missing catch mid-date (see original notes)
# "Year 2012 is missing a catch mid-year date"
#CATCH_MIDDATE[34] <- CATCH_MIDDATE[33]
#hmm, lots of LS is missing a catch middate. Not sure what to do about that. AGR FLAG.

# TAUs - 
N2 <- 0.6443  # fallback value from Excel $N$3- of Lynn specifically, from Excel

CATCH_SURVEY_TAU    <- rep(0, length(CATCH_MIDDATE))
CATCH_SURVEY_TAU[1] <- 0#0.6499   #AGR HERE put LS starting value from excel- FLAG, this is hand wavey
for (i in 2:length(CATCH_MIDDATE)) {
  if (CATCH[i - 1] == 0) {
    CATCH_SURVEY_TAU[i] <- N2
  } else {
    CATCH_SURVEY_TAU[i] <- abs(SURVEY_MIDDATE[i] - CATCH_MIDDATE[i - 1]) / 365
  }
}

SURVEY_TAU    <- rep(0, length(SURVEY_MIDDATE))
SURVEY_TAU[1] <- 0
for (i in 2:length(SURVEY_MIDDATE)) {
  SURVEY_TAU[i] <- abs(SURVEY_MIDDATE[i] - SURVEY_MIDDATE[i - 1]) / 365
}

# Survey CPUE vectors
CPUE_prerec  <- df$Pre.recruit;   CPUE_prerec[is.na(CPUE_prerec)]   <- 0
CPUE_rec     <- df$Recruit;       CPUE_rec[is.na(CPUE_rec)]         <- 0
CPUE_postrec <- df$Post.recruit;  CPUE_postrec[is.na(CPUE_postrec)] <- 0

# Starting values from previous analysis (for initial pars)
pred_CPUE_rec     <- df$Estimated.Recruits
pred_CPUE_postrec <- df$Estimated.Postrecruits

# 3D survey data array [n_obs x 3 x 3]: (Year, CPUE, CV) x 3 stages
array_prerec_cpue  <- array(c(YEARS, CPUE_prerec,  CV), dim = c(nrow(df), 3),
                            dimnames = list(NULL, c("YEARS", "CPUE_prerec",  "CV")))
array_rec_cpue     <- array(c(YEARS, CPUE_rec,     CV), dim = c(nrow(df), 3),
                            dimnames = list(NULL, c("YEARS", "CPUE_rec",     "CV")))
array_postrec_cpue <- array(c(YEARS, CPUE_postrec, CV), dim = c(nrow(df), 3),
                            dimnames = list(NULL, c("YEARS", "CPUE_postrec", "CV")))

array_all_stages <- abind::abind(array_prerec_cpue, array_rec_cpue, array_postrec_cpue, along = 3)
dimnames(array_all_stages)[[3]] <- c("Stage_1", "Stage_2", "Stage_3")
array_all_stages <- array_all_stages[!is.na(array_all_stages[, 2, 1]), , ]  # drop NA years

# ============================================================================
# AREA-SPECIFIC STARTING VALUES
# ============================================================================
T12_start <- 4.6443533917851 / 10       # from 2024 Lynn sisters Excel CSA (Q2)
q_start   <- 76.1472017408894 / 1e5       # from 2024 LYnn sisters Excel CSA (Q3)
S_fixed   <- 0.32                     # natural mortality (fixed, same across areas)

# ============================================================================
# BUILD DATA AND PARAMETER LISTS
# ============================================================================
data <- list(
  YEARS            = YEARS,
  CATCH            = CATCH,
  survey_data      = array_all_stages,
  wt_mature        = df$Mature.Weight,
  wt_legal         = df$Legal.Weight,
  wt_prerec        = df$Prerecruit.Weight,
  SURVEY_TAU       = SURVEY_TAU,
  CATCH_SURVEY_TAU = CATCH_SURVEY_TAU
)

pars <- list(
  ln_mean_rec          = log(1.6),     #about 1.6 for lynn sisters, the prerecruits each year (ie recruitment)
  ln_Eps_R             = log(rep(1, length(YEARS))),
  ln_q                 = log(q_start),
  ln_T12               = log(T12_start),
  S                    = S_fixed,
  ln_sigma_survey      = log(0.25),
  ln_init_rec_cpue     = log(pred_CPUE_rec[1]),
  ln_init_postrec_cpue = log(pred_CPUE_postrec[1])
)

# Map: fix S and mean recruitment
map <- list()
map$S             <- factor(NA)
map$ln_mean_rec   <- factor(NA)
#consider fixing q if issues

# Save weight vectors for plotting before rm (used by plots and GHL tables below)
wt_df <- data.frame(
  wt_prerec = data$wt_prerec,
  wt_legal  = data$wt_legal,
  wt_mature = data$wt_mature
)

# ============================================================================
# RUN MODEL (using generalized function)
# ============================================================================
mod <- run_csa_model(data, pars, map, newtonsteps = 0)

# Quick convergence check
# Quick convergence check
best_par  <- mod$pop_mod$env$last.par.best
max_grad  <- max(abs(mod$pop_mod$gr(best_par)))

cat("Max gradient (at last.par.best):", max_grad, "\n") 
cat("Converged (< 0.001)?", max_grad < 0.001, "\n")
cat("Optimized jnLL:", mod$report$jnLL, "\n")
cat("Estimated q:", mod$report$q, "\n")
cat("Estimated T12:", mod$report$T12, "\n")
cat("Estimated sigma_survey:", mod$report$sigma_survey, "\n")

# Point estimates (tidy data frame)
results <- mod$results
head(results)

# ============================================================================
# BOOTSTRAP CONFIDENCE INTERVALS (non-negative by construction)
# ============================================================================
# NOTE: This is especially important for Excursion Inlet where the old
# +/- 1.96*SE method produced negative CI lower bounds.
# n_boot = 500 is a reasonable starting point. For quick testing try n_boot = 50.
boot <- bootstrap_ci(mod$pop_mod, data, pars, map, #convergence issues on 20/500 for lynn sisters
                     n_boot = 500, ci_level = 0.95,
                     seed = 42, verbose = TRUE, newtonsteps=0) #EI - some iterations have nonconvergence. Try more newtonsteps? Most converge tho

# Merge point estimates with bootstrap CIs
results_df <- merge_results_ci(results, boot)

# ============================================================================
# LOAD EXCEL COMPARISON DATA
# ============================================================================
##AGR HERE- get lynn data for comparison
df_excel <- read.csv("CSA_excel/Lynn sisters 2025 Excel results comparison.csv")
df_excel$Mature.Biomass     <- as.numeric(gsub(",", "", df_excel$Mature.Biomass))
df_excel$Legal.Biomass      <- as.numeric(gsub(",", "", df_excel$Legal.Biomass))
df_excel$Prerecruit.Biomass <- as.numeric(gsub(",", "", df_excel$Prerecruit.Biomass))

# Add CV from the survey_data report for observed error bars
report_survey  <- mod$report$survey_data  # [n_obs x 4 x 3] with weights appended
cv_df <- data.frame(
  year = report_survey[, 1, 1],
  CV   = report_survey[, 3, 1]
)

df_excel <- df_excel %>%
  mutate(year = Survey.Year) %>%
  left_join(cv_df, by = "year") %>%
  mutate(
    cv_upper_prerec  = Pre.recruit  + CV,
    cv_lower_prerec  = Pre.recruit  - CV,
    cv_upper_rec     = Recruit      + CV,
    cv_lower_rec     = Recruit      - CV,
    cv_upper_postrec = Post.recruit + CV,
    cv_lower_postrec = Post.recruit - CV
  )

# ============================================================================
# CPUE COMPARISON PLOTS
# ============================================================================
color_levels <- c("RTMB", "Excel", "Observed")

# Pre-recruit CPUE
p1 <- ggplot(results_df, aes(x = year, y = prerec_cpue)) +
  geom_ribbon(aes(ymin = prerec_cpue_lower, ymax = prerec_cpue_upper),
              alpha = 0.3, fill = "lightblue") +
  geom_line(aes(color = factor("RTMB", levels = color_levels)), linewidth = 1) +
  geom_point(aes(color = factor("RTMB", levels = color_levels))) +
  geom_line(data = df_excel,
            aes(y = Estimated.Prerecruits, x = Survey.Year,
                color = factor("Excel", levels = color_levels))) +
  geom_point(data = df_excel,
             aes(y = Pre.recruit, x = Survey.Year,
                 color = factor("Observed", levels = color_levels))) +
  geom_errorbar(data = df_excel,
                aes(ymin = cv_lower_prerec, ymax = cv_upper_prerec,
                    x = Survey.Year, y = NULL)) +
  labs(title = paste(area_name, "Pre-recruit CPUE"), x = "Year", y = "CPUE",
       subtitle = "Observed CPUE as black points with CV error bars") +
  scale_color_manual(name = NULL,
                     values = c("RTMB" = "lightblue", "Excel" = "darkgreen", "Observed" = "black")) +
  theme_minimal() +
  theme(legend.position = c(0.95, 0.95),
        legend.box.background = element_rect(color = "gray80"))

# Recruit CPUE
p2 <- ggplot(results_df, aes(x = year, y = rec_cpue)) +
  geom_ribbon(aes(ymin = rec_cpue_lower, ymax = rec_cpue_upper),
              alpha = 0.3, fill = "lightblue") +
  geom_line(color = "lightblue", linewidth = 1) +
  geom_point(color = "lightblue") +
  geom_point(data = df_excel, aes(y = Recruit, x = Survey.Year)) +
  geom_errorbar(data = df_excel,
                aes(ymin = cv_lower_rec, ymax = cv_upper_rec, x = Survey.Year, y = NULL)) +
  geom_line(data = df_excel,
            aes(y = Estimated.Recruits, x = Survey.Year), color = "darkgreen") +
  labs(title = paste(area_name, "Recruit CPUE"), x = "Year", y = "CPUE") +
  theme_minimal()

# Post-recruit CPUE
p3 <- ggplot(results_df, aes(x = year, y = postrec_cpue)) +
  geom_ribbon(aes(ymin = postrec_cpue_lower, ymax = postrec_cpue_upper),
              alpha = 0.3, fill = "lightblue") +
  geom_line(color = "lightblue", linewidth = 1) +
  geom_point(color = "lightblue") +
  geom_point(data = df_excel, aes(y = Post.recruit, x = Survey.Year)) +
  geom_errorbar(data = df_excel,
                aes(ymin = cv_lower_postrec, ymax = cv_upper_postrec, x = Survey.Year, y = NULL)) +
  geom_line(data = df_excel,
            aes(y = Estimated.Postrecruits, x = Survey.Year), color = "darkgreen") +
  labs(title = paste(area_name, "Post-recruit CPUE"), x = "Year", y = "CPUE") +
  theme_minimal()

(p123 <- p1 / p2 / p3)
ggsave(paste0("figures/rkc/", cur_yr, "/CSA_", area_name, "_CPUE.png"),
       plot = p123, width = 8, height = 10, dpi = 300)

# ============================================================================
# BIOMASS COMPARISON PLOT
# ============================================================================
color_levels_2 <- c("Mature RTMB", "Mature Excel", "Legal RTMB", "Legal Excel",
                    "Pre-recruit RTMB", "Pre-recruit Excel")

p4 <- ggplot(results_df, aes(x = year)) +
  # Mature biomass
  geom_ribbon(aes(ymin = mature_bio_lower, ymax = mature_bio_upper),
              alpha = 0.3, fill = "#56B4E9") +
  geom_line(aes(y = mature_biomass, color = factor("Mature RTMB", levels = color_levels_2)),
            linewidth = 1) +
  geom_line(data = df_excel,
            aes(y = Mature.Biomass, x = Survey.Year,
                color = factor("Mature Excel", levels = color_levels_2))) +
  # Legal biomass
  geom_ribbon(aes(ymin = legal_bio_lower, ymax = legal_bio_upper),
              alpha = 0.2, fill = "#009E73") +
  geom_line(aes(y = legal_biomass, color = factor("Legal RTMB", levels = color_levels_2)),
            linewidth = 1) +
  geom_line(data = df_excel,
            aes(y = Legal.Biomass, x = Survey.Year,
                color = factor("Legal Excel", levels = color_levels_2))) +
  # Pre-recruit biomass
  geom_ribbon(aes(ymin = prerec_bio_lower, ymax = prerec_bio_upper),
              alpha = 0.2, fill = "#CC79A7") +
  geom_line(aes(y = prerec_biomass, color = factor("Pre-recruit RTMB", levels = color_levels_2)),
            linewidth = 1) +
  geom_line(data = df_excel,
            aes(y = Prerecruit.Biomass, x = Survey.Year,
                color = factor("Pre-recruit Excel", levels = color_levels_2))) +
  labs(title = paste(area_name, "Mature, Legal, and Pre-recruit Biomass"),
       x = "Year", y = "Biomass (lbs)") +
  scale_color_manual(name = NULL,
                     values = c("Mature RTMB" = "#56B4E9", "Mature Excel" = "darkblue",
                                "Legal RTMB" = "#009E73",  "Legal Excel" = "darkgreen",
                                "Pre-recruit RTMB" = "#CC79A7", "Pre-recruit Excel" = "#542788")) +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.8),
        legend.background = element_rect(fill = alpha("white", 0.7), color = "gray80"),
        legend.box.background = element_rect(color = "gray80"))

p4
ggsave(paste0("figures/rkc/", cur_yr, "/CSA_", area_name, "_Biomass.png"),
       plot = p4, width = 8, height = 5, dpi = 300)

# Extra zoom view
p5 <- p4 + ylim(0, 500000)
ggsave(paste0("figures/rkc/", cur_yr, "/CSA_", area_name, "_Biomass_zoom.png"),
       plot = p5, width = 8, height = 5, dpi = 300)


# ============================================================================
# OUTPUT TABLE CONSTRUCTION
# ============================================================================

# Build output_df (analogous to the old Excel-format output)
output_df <- df_excel %>%
  mutate(
    Estimated.Prerecruits = results_df$prerec_cpue[match(Survey.Year, results_df$year)],
    Estimated.Recruits    = results_df$rec_cpue[match(Survey.Year, results_df$year)],
    Estimated.Postrecruits = results_df$postrec_cpue[match(Survey.Year, results_df$year)],
    Prerecruit.Biomass    = results_df$prerec_biomass[match(Survey.Year, results_df$year)],
    Legal.Biomass         = results_df$legal_biomass[match(Survey.Year, results_df$year)],
    Mature.Biomass        = results_df$mature_biomass[match(Survey.Year, results_df$year)]
  )

#SAVE output csv
write.csv(output_df, paste0("results/rkc/LynnSisters/", cur_yr, "/CSA_output_2", cur_yr, ".csv"), row.names = FALSE) #fkag- output date is bad

#output biomass only into a biomass csv:
