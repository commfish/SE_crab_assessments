###RKC commercial catch ###
##Alex Reich
## alex.reich@alaska.gov
## 7/13/26

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
    ungroup() #so whole weight is the same as landed weight

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


