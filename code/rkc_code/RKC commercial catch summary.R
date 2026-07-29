###RKC commercial catch ###
##Alex Reich
## alex.reich@alaska.gov
## 7/13/26
## 7/29/26 Lynn Sisters Pull

## A file that will read in commercial catch and summarize it by area ##

library(tidyverse)
library(ggplot2)

cur_yr <- 2026

#read in data
##get data from OceanAK - https://oceanak.adfg.alaska.gov/analytics/saw.dll?Answers#resultsTab19f5da9bb75 or Shared Folders/Commercial Fisheries/Region I/Invertebrates/User Reports/agreich/SE RKC
###pull just the most recent year/season in OceanAK

comm_catch <- read.csv("data/harvest/RKC commercial fish tickets 25 26.csv")

comm_catch_by_area <- comm_catch %>%
  dplyr::filter(Fishery == "Juneau area RKC"|Fishery=="Seymour Canal RKC"|Fishery=="Gambier Bay RKC") %>%
  group_by(Fishery) %>%
  summarise(Whole_weight_total = sum(Whole.Weight..sum.),
            Landed_weight_total = sum(Landed.Weight..sum.)
            ) %>%#summarized by biomass.
    ungroup() #whole weight is the same as landed weight

#Do we want it summarized by number? See the personal use fishery code.
  
## can use legal weight from last years to extrapolate this into pounds 
male_weights_JNU <- read.csv(paste0('./results/rkc/Juneau', 
                                '/', cur_yr, '/maleweights.csv'))
legal_wt_JNU_pryr <- male_weights_JNU[1,3]
#male_weights_GB <- read.csv(paste0('./results/rkc/Gambier Bay', 
 #                                   '/', cur_yr, '/maleweights.csv'))
#male_weights_SC <- read.csv(paste0('./results/rkc/Seymour', 
 #                                   '/', cur_yr, '/maleweights.csv'))

comm_ctach_JNU_table <- comm_catch_by_area %>% filter(Fishery == "Juneau area RKC") %>%
  mutate(Numbers_crab = Whole_weight_total/legal_wt_JNU_pryr)

write.csv(comm_ctach_JNU_table, file="results/rkc/Juneau/2026/comm_catch_JNU.csv")


######################################################################################
#Lynn Sisters
#######################################################################################
#View(comm_catch)

#stat area 115-14 is adjacent to Lynn Sisters. We have reason to believe that the Lynn Sisters stock overflows/moves into 115-14
## so even though the 115-14 is not in Lynn Sisters, the catch from that area will be included as the Lynn Sisters commercial catch
## Lynn Sisters was closed to commercial fishing in 2026. If it opens, include the catch from Lynn Sisters proper + catch in 115-14 as the Lynn Sisters comemrcial catch

comm_catch_LS <- comm_catch %>%
  dplyr::filter(Fishery == "Lynn Sisters RKC"|Stat.Area=="11514") %>%
  group_by(Fishery) %>%
  summarise(Whole_weight_total = sum(Whole.Weight..sum.),
            Landed_weight_total = sum(Landed.Weight..sum.)
  ) %>%#summarized by biomass.
  ungroup() #whole weight is the same as landed weight

#how many #'s of crab is that?
male_weights_LS <- read.csv(paste0('./results/rkc/LynnSisters', 
                                    '/', cur_yr, '/maleweights.csv'))
legal_wt_LS_pryr <- male_weights_LS[1,3]

comm_ctach_LS_table <- comm_catch_LS %>%
  mutate(Numbers_crab = Whole_weight_total/legal_wt_LS_pryr)

write.csv(comm_ctach_LS_table, file="results/rkc/LynnSisters/2026/comm_catch_LS.csv")


##############################
###comm catch dates
#######################
#### survey mid date Lynn Sisters-----  
# list of unique dates (day only, excluding time)
dates_LS_temp <- comm_catch %>%
  dplyr::filter(Fishery == "Lynn Sisters RKC"|Stat.Area=="11514")  #select for Lynn Sisters
dates.LS <- unique(round_date(ymd_hms(dates_LS_temp$Date.of.Landing), unit="day"))


# interval of minimum and maximum survey dates
date.int <- interval(min(dates.LS, na.rm=TRUE), max(dates.LS, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
source('./code/functions.R')
sur.midpoint <- int_midpoint(date.int)

# convert to Julian day
sur.midpoint.jul <- yday(sur.midpoint)
#save the survey midpoint in results
#write.csv(data.frame(sur.midpoint, sur.midpoint.jul), 
 #         paste0('./results/rkc/', survey.location, '/', cur_yr, '/survey_midpoint_', cur_yr, '.csv'))

#AGR TK- write for all areas then combine - Juneau, Gambier, Seymour, LS


########################################################################################
#summary
#####################################################################################
#perhaps graph the comm catch by area?? Nah?






