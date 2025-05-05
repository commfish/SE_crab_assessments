# ****************************************************************************************************
# Purpose: create .dat files for the CSA model in TMB
# Author: Caitlin Stern
# Date: June 2023


# ****************************************************************************************************
# libraries and parameter values ----
# ****************************************************************************************************

library(tidyverse)

# set year and survey location
cur_yr <- 2022 
pr_yr <- cur_yr - 1
survey.location <- 'Juneau'



# ****************************************************************************************************
# Workflow ----
# ****************************************************************************************************

# Before running this script, run 
# 1) [SurveyLocation].R (e.g., Juneau.R)
#     - this will create the updated .csv files to read in for sur.dat, wts.dat, and avg_wts.dat       
# 2) personal_use.R (if running for the Juneau area)
#     - this will create the updated .csv file to read in for catch.dat


# ****************************************************************************************************
# create data input file ----
# ****************************************************************************************************

# load catch data 
catch.dat <- read_csv(paste0(here::here(), "/data/harvest/", survey.location, "_catch_all_", cur_yr, ".csv")) %>%
  as.data.frame()
# this is what I did to format the catch data from the CSA excel spreadsheet
# shouldn't need to repeat this process once the base file is created - just append each year
  #mutate(Date = as.Date(Catch.Middate,format="%Y-%m-%d")) %>%
  #mutate(doy_mid_date = yday(Date)) %>%
  #mutate(year = Survey.Year) %>%
  #mutate(yield = Catch.Number) %>%
  #dplyr::select(year, yield, doy_mid_date)

# load survey data
sur.dat <- read_csv(paste0(here::here(), "/results/rkc/", survey.location, "/", cur_yr, "/", survey.location, "_cpue_for_model_", cur_yr, ".csv")) %>%
  as.data.frame()
# this is what I did to format the survey data from the CSA excel spreadsheet
# shouldn't need to repeat this process once the base file is created - just append each year
#mutate(Date = as.Date(survey_mid_date, format="%Y-%m-%d")) %>%
#mutate(doy_mid_date = yday(Date)) %>%
#dplyr::select(-c(survey_mid_date, Date))

# load survey weights data
wts.dat <- read_csv(paste0(here::here(), "/results/rkc/", survey.location, "/", cur_yr, "/", survey.location, "_survey_weights_", cur_yr, ".csv")) %>%
  as.data.frame() 
  
names(wts.dat) <- NULL

# load average weights data
avg_wts.dat <- read_csv(paste0(here::here(), "/results/rkc/", survey.location, "/", cur_yr, "/", survey.location, "_avg_weights_", cur_yr, ".csv")) %>%
  as.data.frame()

# I converted the historical average weights data into kilograms as required by the model.
# In future, this conversion will be performed before the current year's data are appended,
# in the [SurveyLocation].R script.
#
#wts_kg <- read_csv(paste0(here::here(), "/results/rkc/", survey.location, "/2021/", survey.location, "_avg_weights_2021.csv")) %>% 
#  mutate(pre_recruit_lbs = pre_recruit, legal_lbs = legal) %>%
#  mutate(pre_recruit = pre_recruit_lbs * 0.45359237, legal = legal_lbs * 0.45359237) %>%
#  dplyr::select(year, pre_recruit, legal)
#write_csv(wts_kg, paste0(here::here(), "/results/rkc/", survey.location, "/2021/", survey.location, "_avg_weights_2021.csv"))

names(avg_wts.dat) <- NULL

# model inputs
input.dat <- list("start year, end year" = c(1979, cur_yr), "nstage" = 3, "natural mortality of recruits" = 0.3, "catch in directed fishery" = catch.dat, "observed survey index" = sur.dat, "weights" = wts.dat, "avg weight by group" = avg_wts.dat)

# visual check of inputs
input.dat

# write inputs to .rds file
write_rds(input.dat, paste0(here::here(), "/data/rkc/", survey.location, "/", survey.location, "_model_input_", cur_yr, ".rds"))


# ****************************************************************************************************
# create starting values for model in .pin file ----
# ****************************************************************************************************

# sources of values for PIN file:
# ln_init_index = 
# trans_probs = 0 	0 	1 	1. These are the diagonal elements of the transition matrix.
# ln_Rbar = 0. Mean recruitment in log space; is 0 if fitting without recruitment as a random effect.
# Eps_R = 
# ln_sigmaR = 
# preM = 
# ln_q = log(0.00010556), the scaled estimated catchability for 2022
# ln_mu = 

pin.dat <- c("# RKC csa parameters starting values", "# ln_init_index", "-0.1085670  0.2112000", "# trans_probs", "0 	0 	1 	1", "# ln_Rbar", "0", "# Eps_R", "0.8454125	1.284120104 1.462321602	1.610675356	1.495114956	1.707739803	2.173688037	1.498484417	1.652566142	1.572296267	1.312914724	1.476313009	0.924074388	1.426210962	1.362561846	1.670851699	1.677917689	2.141306631	2.270968773	2.316513342	1.758259574	1.020472038", "# ln_sigmaR", "-1.7", "# preM", "-1.314766", "# ln_q", "-9.156231", "# ln_mu", "0")

writeLines(pin.dat, paste0(here::here(), "/data/rkc/", survey.location, "/", survey.location, "_initial_values_", cur_yr, ".pin"))


