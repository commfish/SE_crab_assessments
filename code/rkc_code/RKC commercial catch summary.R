###RKC commercial catch ###
##Alex Reich
## alex.reich@alaska.gov
## 7/13/26
## 7/29/26 Lynn Sisters Pull

## A file that will read in commercial catch and summarize it by area ##
source("code/functions.R")

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
  
## can use legal weight from last years to extrapolate this into pounds - need to run up to maleweights in the area.R file first.
male_weights_JNU <- read.csv(paste0('./results/rkc/Juneau', 
                                '/', cur_yr, '/maleweights.csv'))
legal_wt_JNU_pryr <- male_weights_JNU[1,3]

male_weights_GB <- read.csv(paste0('./results/rkc/Gambier', 
                                    '/', cur_yr, '/maleweights.csv'))
legal_wt_GB_pryr <- male_weights_GB[1,3]

male_weights_SC <- read.csv(paste0('./results/rkc/Seymour', 
                                    '/', cur_yr, '/maleweights.csv'))

legal_wt_SC_pryr <- male_weights_SC[1,3]

comm_ctach_JNU_table <- comm_catch_by_area %>% filter(Fishery == "Juneau area RKC") %>%
  mutate(Numbers_crab = Whole_weight_total/legal_wt_JNU_pryr)

write.csv(comm_ctach_JNU_table, file="results/rkc/Juneau/2026/comm_catch_JNU.csv")


comm_ctach_GB_table <- comm_catch_by_area %>% filter(Fishery == "Gambier Bay RKC") %>%
  mutate(Numbers_crab = Whole_weight_total/legal_wt_GB_pryr)

comm_ctach_SC_table <- comm_catch_by_area %>% filter(Fishery == "Seymour Canal RKC") %>%
  mutate(Numbers_crab = Whole_weight_total/legal_wt_SC_pryr)

write.csv(comm_ctach_GB_table, file="results/rkc/Gambier/2026/comm_catch_GB.csv")
write.csv(comm_ctach_SC_table, file="results/rkc/Seymour/2026/comm_catch_SC.csv")




##Gambier

##Seymour




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
#Lynn Sisters comm midpoint
dates_LS_temp <- comm_catch %>%
  dplyr::filter(Fishery == "Lynn Sisters RKC"|Stat.Area=="11514")  #select for Lynn Sisters
dates.LS <- unique(round_date(ymd_hms(dates_LS_temp$Date.of.Landing), unit="day"))


# interval of minimum and maximum survey dates
date.int.LS <- interval(min(dates.LS, na.rm=TRUE), max(dates.LS, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
source('./code/functions.R')
sur.midpoint.LS <- int_midpoint(date.int.LS)

# convert to Julian day
sur.midpoint.jul.LS <- yday(sur.midpoint.LS)



#Juneau comm midpoint - TK question - when there's a PU and Comm fishery, what's the catch date? 60/40 between PU and comm fish catch?? 
##doesn't really matter but maybe correct it for next year
dates_JNU_temp <- comm_catch %>%
  dplyr::filter(Fishery == "Juneau area RKC")  #select for JNU; perhaps use this in the future: (Fishery == "Juneau area RKC"|Stat.Area=="11513") to include "postage stamp"
dates.JNU <- unique(round_date(ymd_hms(dates_LS_temp$Date.of.Landing), unit="day"))

# interval of minimum and maximum survey dates
date.int.JNU <- interval(min(dates.JNU, na.rm=TRUE), max(dates.JNU, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
sur.midpoint.JNU <- int_midpoint(date.int.JNU)

# convert to Julian day
sur.midpoint.jul.JNU <- yday(sur.midpoint.JNU)



#Seymour Canal comm catch midpoint
dates_SC_temp <- comm_catch %>%
  dplyr::filter(Fishery == "Seymour Canal RKC")  #select for Seymour
dates.SC <- unique(round_date(ymd_hms(dates_SC_temp$Date.of.Landing), unit="day"))

# interval of minimum and maximum survey dates
date.int.SC <- interval(min(dates.SC, na.rm=TRUE), max(dates.SC, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
sur.midpoint.SC <- int_midpoint(date.int.SC)

# convert to Julian day
sur.midpoint.jul.SC <- yday(sur.midpoint.SC)


#Gambier Bay comm catch midpoint
dates_GB_temp <- comm_catch %>%
  dplyr::filter(Fishery == "Gambier Bay RKC")  #select for Seymour
dates.GB <- unique(round_date(ymd_hms(dates_GB_temp$Date.of.Landing), unit="day"))

# interval of minimum and maximum survey dates
date.int.GB <- interval(min(dates.GB, na.rm=TRUE), max(dates.GB, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
sur.midpoint.GB <- int_midpoint(date.int.GB)

# convert to Julian day
sur.midpoint.jul.GB <- yday(sur.midpoint.GB)


#ADD HERE if the comm fishery opens for other areas in future years


#combine and write out the CSV - TK TK AGR
combined_midpoint <- data.frame(sur.midpoint.LS, sur.midpoint.JNU, sur.midpoint.SC, sur.midpoint.GB)

#save the survey midpoint in results
write.csv(combined_midpoint, 
          paste0('./results/rkc/Region1/', cur_yr, '/survey_midpoint_combined', cur_yr, '.csv'))

#AGR TK- write for all areas then combine - Juneau, Gambier, Seymour, LS


########################################################################################
#summary
#####################################################################################
#perhaps graph the comm catch by area?? Nah?






