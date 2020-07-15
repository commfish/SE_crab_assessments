# K.Palof  ADF&G
# 7-11-18, updated 7-8-19 / 7-14-20

# personal use summary for 11-A
# all years that have permit data available


#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)
cur_yr = 2020 # survey year

prv_yr = cur_yr-1 # fishery year NOT survey year

#####Load Data ---------------------------------------------------
personal_use <- read.csv(paste0(here::here(), "/data/harvest/11-A rkc pu_catch_updated2020.csv"))
permit_type <- read.csv(paste0(here::here(), "/data/harvest/PU RKC Juneau 2019 2020 permit status summary.csv"))
# **FIX** get biologist to help summarize this in future...way to time consuming for me and they have to do it already.

## reported number ----
# ** in order to get permits not returned that do NOT have catch need to click on "xyz" and select "include rows with only null values"
# this does NOT translate to .csv output..... **FIX**
personal_use %>% # *
  filter(Year == cur_yr | Year == prv_yr | Year == prv_yr-1) %>%  # remove this to do all years, currently just want current 18/19 season
  group_by(Personal.Use.Area, Year, Season, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number)), 
            number = sum(Number.of.Crab, na.rm = TRUE), 
            pots = sum(Number.of.Pots.or.Tows)) -> by_status

permit_type %>%  # need this data set to get all the permit status...not returned and did not fish are not accounted above due to lack of crab.
  group_by(Personal.Use.Area, Year, Season, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number))) -> permit_status

by_status %>% 
  select(Personal.Use.Area, Year, Season, Permit.Returned.Status, number, pots) %>% 
  right_join(permit_status) %>% 
  as.data.frame()-> number_crab_by_status

### summary and calcs ---------

number_crab_by_status %>% 
  mutate(status = ifelse(Permit.Returned.Status == "Permit entered online", 1, 
                         ifelse(Permit.Returned.Status == "Permit not returned - some harvest reported", 1, 
                                ifelse(Permit.Returned.Status == "Permit returned", 1, 
                                       ifelse(Permit.Returned.Status == "Permit returned - did not fish", 2, 
                                              0)))), 
         status_desc = ifelse(status == 1, "returned & harvest reported", 
                              ifelse(status == 2, "returned - did not fish", "not returned"))) %>% 
  mutate(crab_year = ifelse(Year == cur_yr, 2020, 
                            ifelse(Year == prv_yr & Season == "S", 2020, 
                                   ifelse(Year == prv_yr-1 & Season == "S", 2019, 2019)))) %>% # need to group by crab year not calendar year
  group_by(crab_year, status, status_desc) %>% 
  summarise(number = sum(number), pots = sum(pots), n = sum(n))%>% 
  mutate(cpue = number/pots, cpue_permits = number/n) -> by_status_current
write.csv(by_status_current, paste0('./results/rkc/Juneau/personal_use_raw_summary_', cur_yr,'.csv'), row.names = FALSE)

by_status_current %>% 
  group_by(crab_year) %>% 
  summarise(total_c = sum(number, na.rm = TRUE)) -> total_c
# 0 = permit not returned
# 1 = permit returned and fished
# 2 = permit returned but NOT fished

## estimated number ----
# pervious notes on personal use suggest that an equation was used to estimate harvest from those permits
#   that were not returned
#     
# percent not returned 
by_status_current %>% 
  right_join(total_c) %>% 
  group_by(crab_year, status) %>% # only looking at one season here 2018/2019...or current
  summarise(n = sum(n, na.rm = TRUE), 
            number = sum(number, na.rm = TRUE), 
            pots = sum(pots, na.rm = TRUE), 
            total_c = total_c) %>% 
  select(crab_year, total_c, status, n) %>% 
  spread(status, n) %>% 
  as.data.frame() %>% 
  mutate(pct.r.that.fished = (`1`) / (`1` + `2`), 
         pnr = (`0`) / (`1` + `2` +`0`),  
         total_permits = (`1` + `2` +`0`), 
         adjustment = (total_permits / (total_permits - 0.762*(`0`))), 
         est.total.catch.numbers = adjustment*as.numeric(total_c)) -> summary_current
write.csv(summary_current, paste0('./results/rkc/Juneau/personal_use_estimate_total_', cur_yr, '.csv'), row.names = FALSE)

## **FIX ** 2020 stopped here.

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


