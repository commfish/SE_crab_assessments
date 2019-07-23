# K.Palof    katie.palof@alaska.gov
# ADF&G 7-3-19/ updated for JUNEAU area

# R script contains code to process data from Ocean AK to use in crab CSA models, code to run CSA model, and calls to create 
#     output and figures for annual stock health report.


# Read me:
#     update code with date updated (top), change global year, and pull new survey data (see below)


## load -------------------------
source('./code/functions.R')

## setup global ---------------
cur_yr <- 2019
pr_yr <- cur_yr -1
survey.location <- 'Juneau'

## data -------------------
dat <- read.csv("./data/rkc/Juneau/jnu_18_19_oceanAK_out_RAW.csv") # file name will change annually
# this is input from OceanAK - set up as red crab survey data for CSA
#   survey area should match that in the name of this script file
#   Juneau area includes Juneau and Barlow
area <- read.csv("./data/rkc/Juneau/Juneau_Barlow_strata_area.csv") # same every year

histdat <- read.csv(paste0('./results/rkc/', survey.location, 
                           '/', pr_yr, '/JNU_perpot_all_', pr_yr,'.csv'))
## !!!!  this file will be 'JNU_perpot_all_pr_yr' and just get updated with current years data.
females <- read.csv(paste0('./results/rkc/', survey.location, 
                           '/', pr_yr, '/largef_all.csv'))

baseline <- read.csv("./data/rkc/longterm_means.csv") # same every year
biomass <- read.csv("./data/rkc/biomass.csv") # ** update ** from CSA model
#   file for all locations. Has biomass estimates from CSA,
#   must be updated after CSA model is run for current year USING current year's model
#             NOT historic forecast!

## CPUE calc --------------






