# K.Palof  ADF&G
# 7-11-18, updated 7-8-19

# personal use summary for 11-A
# all years that have permit data available


#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)
cur_yr = 2019 # fsurvey year

prv_yr = cur_yr-1 # fishery year NOT survey year

#####Load Data ---------------------------------------------------
personal_use <- read.csv("./data/redcrab/11-A rkc pu_catch.csv")
permit_type <- read.csv("./data/redcrab/PU RKC Juneau 2018 permit status summary.csv")

## reported number ----
# ** in order to get permits not returned that do NOT have catch need to click on "xyz" and select "include rows with only null values"
# this does NOT translate to .csv output..... **FIX**
personal_use %>% # *
  filter(Year == cur_yr | Year == prv_yr) %>%  # remove this to do all years, currently just want current 18/19 season
  group_by(Area, Year, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number)), 
            number = sum(Number.of.Crab, na.rm = TRUE), 
            pots = sum(Number.of.Pots.or.Tows)) -> by_status

permit_type %>%  # need this data set to get all the permit status...not returned and did not fish are not accounted above due to lack of crab.
  group_by(Area, Year, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number))) ->permit_status

by_status %>% 
  select(Area, Year, Permit.Returned.Status, number, pots) %>% 
  right_join(permit_status) %>% 
  as.data.frame()-> number_crab_by_status

### summary and calcs ---------

number_crab_by_status %>% 
  mutate(status = ifelse(Permit.Returned.Status == "Permit entered online", 1, 
                         ifelse(Permit.Returned.Status == "Permit phoned in", 1, 
                                ifelse(Permit.Returned.Status == "Permit returned", 1, 
                                       ifelse(Permit.Returned.Status == "Permit returned - did not fish", 2, 
                                              0))))) %>% 
  mutate(cpue = number/pots, cpue_permits = number/n) -> by_status_current
write.csv(by_status_current, paste0('./results/redcrab/Juneau/personal_use_raw_summary_', cur_yr,'.csv'), row.names = FALSE)

by_status_current %>% 
  summarise(sum(number, na.rm = TRUE)) -> total_c
# 0 = permit not returned
# 1 = permit returned and fished
# 2 = permit returned but NOT fished

## estimated number ----
# pervious notes on personal use suggest that an equation was used to estimate harvest from those permits
#   that were not returned
#     
# percent not returned 
by_status_current %>% 
  group_by(status) %>% # only looking at one season here 2018/2019...or current
  summarise(n = sum(n, na.rm = TRUE), 
            number = sum(number, na.rm = TRUE), 
            pots = sum(pots, na.rm = TRUE)) %>% 
  select(status, n) %>% 
  spread(status, n) %>% 
  mutate(pct.r.that.fished = (`1`) / (`1` + `2`), 
         pnr = (`0`) / (`1` + `2` +`0`), 
         total_permits = sum(`1` + `2` +`0`), # **FIX ** this is not matching....
         adjustment = (total_permits / (total_permits - 0.762*(`0`))), 
         est.total.catch.numbers = adjustment*as.numeric(total_c[1])) -> summary_current
write.csv(summary_current, paste0('./results/redcrab/Juneau/personal_use_estimate_total_', cur_yr, '.csv'), row.names = FALSE)



## can use legal weight from last years to extrapolate this into pounds ***need to have run current survey year data
          #   in JNUprocessingCODE.R 
          # use weight that matches fishery timing i.e. 2018 for 2018 summer
## only works IF the male_weights is loaded from the processing code - if not need to bring it in from
###     results folder
male_weights <- read.csv(paste0('./results/redcrab/Juneau', 
                                '/', cur_yr, '/maleweights.csv'))
summary_current %>% 
  mutate(est.catch.lbs = est.total.catch.numbers*male_weights$legal_lbs[1]) -> summary_current

write.csv(summary_current, paste0('./results/redcrab/Juneau/personal_use_estimate_total_', cur_yr, '.csv'), row.names = FALSE)


