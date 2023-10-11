# K.Palof  ADF&G
# 7-11-18, updated 7-8-19 / 7-14-20 / 9-6-21/ 7-6-22 / 7-14-2023

# PU data for 11-A Juneau area -- see line 17
## PU data for regionwide permit - see below starting at line 105
# all years that have permit data available


#####Load Packages ---------------------------------
library(tidyverse)
library(xlsx)

cur_yr = 2023 # survey year

prv_yr = cur_yr-1 # fishery year NOT survey year


# 11-A personal use summary for 11-A -----------------
#####Load Data ---------------------------------------------------
personal_use <- read.csv(paste0(here::here(), "/data/harvest/11-A rkc pu_catch_updated2023.csv"))
permit_type <- read.csv(paste0(here::here(), "/data/harvest/PU RKC Juneau 2021 2022 2023 permit status summary.csv"))
permit_type_18 <- read.csv(paste0(here::here(), "/data/harvest/PU RKC Juneau permit status summary 2018 to present_updated 2023.csv"))
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
  mutate(crab_year = ifelse(Year == cur_yr, cur_yr, 
                            ifelse(Year == prv_yr & Season == "S", cur_yr, 
                                   ifelse(Year == prv_yr-1 & Season == "S", prv_yr, prv_yr)))) %>% # need to group by crab year not calendar year
  group_by(crab_year, Season, status, status_desc) %>% 
  summarise(number = sum(number, na.rm = T), pots = sum(pots, na.rm = T), n = sum(n))%>% 
  mutate(cpue = number/pots, cpue_permits = number/n) -> by_status_current2
write.csv(by_status_current2, paste0('./results/rkc/Juneau/personal_use_raw_summary2_', cur_yr,'.csv'), row.names = FALSE)

by_status_current2 %>% 
  group_by(crab_year, Season) %>% 
  summarise(total_c = sum(number, na.rm = TRUE)) -> total_c
# 0 = permit not returned
# 1 = permit returned and fished
# 2 = permit returned but NOT fished

## estimated number ----
# previous notes on personal use suggest that an equation was used to estimate harvest from those permits
#   that were not returned
#     
# percent not returned 
by_status_current2 %>% 
  right_join(total_c) %>% 
  group_by(crab_year, Season, status) %>% # only looking at one season here 2018/2019...or current
  summarise(n = sum(n, na.rm = TRUE), 
            number = sum(number, na.rm = TRUE), 
            pots = sum(pots, na.rm = TRUE), 
            total_c = total_c) %>% 
  select(crab_year, Season, total_c, status, n) %>% 
  spread(status, n) %>% 
  as.data.frame() %>% 
  mutate(pct.r.that.fished = (`1`) / (`1` + `2`), 
         pnr = (`0`) / (`1` + `2` +`0`),  
         total_permits = (`1` + `2` +`0`), 
         adjustment = (total_permits / (total_permits - 0.762*(`0`))), 
         est.total.catch.numbers = adjustment*as.numeric(total_c)) -> summary_current2
write.csv(summary_current2, paste0('./results/rkc/Juneau/personal_use_estimate_total2_', cur_yr, '.csv'), row.names = FALSE)

## can use legal weight from last years to extrapolate this into pounds ***need to have run current survey year data
          #   in JNUprocessingCODE.R 
          # use weight that matches fishery timing i.e. 2018 for 2018 summer
## only works IF the male_weights is loaded from the processing code - if not need to bring it in from
###     results folder
male_weights <- read.csv(paste0('./results/rkc/Juneau', 
                                '/', cur_yr, '/maleweights.csv'))

male_weights %>% 
  select(crab_year = Year, legal_lbs) -> male_weights1
summary_current2 %>% 
  right_join(male_weights1) %>% 
  mutate(est.catch.lbs = est.total.catch.numbers*legal_lbs) -> summary_current2
summary_current2 %>% 
  group_by(crab_year) %>% 
  summarise(est.total.numbers = sum(est.total.catch.numbers, na.rm = TRUE)) ### This value is put into the spreadsheet for the CSA for personal use catch for Juneau

write.csv(summary_current2, paste0('./results/rkc/Juneau/personal_use_estimate_total2_', cur_yr, '.csv'), row.names = FALSE)

## regionwide PU data ----------

# before loading data, create the .csv by running the script /code/pull_personal_use_data.R
# OR by downloading from OceanAK using the query "KC PU permit details 2018-present"

# load data -----
personal_use <- read.csv(paste0(here::here(), "/data/harvest/KC PU permit details 2018-2023.csv"))

personal_use %>% 
  summarise(total = sum(Harvest.Reported))

head(personal_use)
personal_use %>% 
  select(Year, Permit.Number, Permit.Returned.Status, Harvest.Reported, Did.Not.Fish, Is.Done.Fishing, 
         Location, Personal.Use.District, Location.Code) -> pu_sum
head(pu_sum)

pu_sum %>% 
  group_by(Year, Permit.Returned.Status, Location, Personal.Use.District, Location.Code) %>% 
  summarise(harvest = sum(Harvest.Reported), not_fish = sum(Did.Not.Fish))

pu_sum %>% 
  group_by(Year, Location, Personal.Use.District, Location.Code) %>% 
  summarise(harvest = sum(Harvest.Reported), not_fish = sum(Did.Not.Fish)) -> step1

write.csv(step1, paste0(here::here(), "/results/rkc/Region1/", cur_yr, "/pu_regional_harvest.csv"))

pu_sum %>% 
  group_by(Year, Personal.Use.District, Location) %>% 
  summarise(harvest = sum(Harvest.Reported), not_fish = sum(Did.Not.Fish)) -> step2

write.csv(step2, paste0(here::here(), "/results/rkc/Region1/", cur_yr, "/pu_regional_harvest2.csv"))

step2 %>% 
  sort(harvest)

## summary of catch by survey area --------------------
survey.area <- c("Pybus Bay", "Seymour Canal", "East Peril Strait", "West Peril Strait", "Excursion Inlet", 
                 "St. James Bay", "Gambier Bay")
pu_sum %>% 
  filter(Location %in% survey.area) %>%
  mutate(Location = ifelse(Location == "East Peril Strait" | Location == "West Peril Strait", "Peril Strait", Location)) %>% 
  group_by(Location, Year) %>% 
  summarise(harvest = sum(Harvest.Reported), not_fish = sum(Did.Not.Fish)) -> survey.area1
write.csv(survey.area1, paste0(here::here(), "/results/rkc/Region1/", cur_yr, "/pu_regional_survey_areas.csv"), row.names = F)

## total crab per year ----------
pu_sum %>% 
  group_by(Year) %>% 
  summarise(harvest = sum(Harvest.Reported)) %>% 
  mutate(crab_lbs = harvest *8.5) # using an average legal weight of 8.5 lbs

###############################################################################################################
## catch midpoint date --------
###############################################################################################################

# bring in catch data, reformat date, keep only dates for the current catch season
# (August of the previous year and January of the current year), and
# calculate the cumulative percentage of total catch for each catch date 

pu_catch_dates <- read.csv(paste0(here::here(), "/data/harvest/PU RKC Juneau 2022_2023 catch dates.csv")) %>%
  mutate(Date = round_date(ymd_hms(Catch.Date), unit="day")) %>%
  filter(Date > as.Date("2022-02-01")) %>%  
  mutate(cum.per = 100*cumsum(Number.of.Crab)/sum(Number.of.Crab))

# select the two dates that have the percentage of cumulative catch closest to 50
pu_catch_50 <- pu_catch_dates %>%
  mutate(diff = abs(cum.per - 50)) %>%
  arrange(diff) %>%
  slice_head(n = 2)

# list of these two catch dates 
pu.dates <- pu_catch_50$Date

# interval of these two catch dates
pu.date.int <- interval(min(pu.dates), max(pu.dates))

# personal use catch midpoint; see functions script for the int_midpoint function
pu.midpoint <- int_midpoint(pu.date.int)

# convert to Julian day
pu.midpoint.jul <- yday(pu.midpoint)

###############################################################################################################
## summary of harvest and permits fished since 2018 --------------
###############################################################################################################

# personal use harvest by year, season, and permit returned status since 2018
by_status_18 <- personal_use %>%
  filter(Year > 2017) %>% 
  group_by(Personal.Use.Area, Year, Season, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number)), 
            number = sum(Number.of.Crab, na.rm = TRUE), 
            pots = sum(Number.of.Pots.or.Tows))

# bring in permit information separated from harvest
# need this data set to get all the permit status - not returned and did not fish are not accounted for above due to lack of crab.
permit_status_18 <- permit_type_18 %>%  
  group_by(Personal.Use.Area, Year, Season, Permit.Returned.Status) %>% 
  summarise(n = length(unique(Permit.Number)))

# join personal use harvest with info on total permits 
number_crab_by_status_18 <- by_status_18 %>% 
  select(Personal.Use.Area, Year, Season, Permit.Returned.Status, number, pots) %>% 
  right_join(permit_status_18) %>% 
  as.data.frame()

# group by crab year
# 0 = permit not returned
# 1 = permit returned and fished
# 2 = permit returned but NOT fished
by_status_18b <- number_crab_by_status_18 %>% 
  mutate(status = ifelse(Permit.Returned.Status == "Permit entered online", 1, 
                         ifelse(Permit.Returned.Status == "Permit not returned - some harvest reported", 1, 
                                ifelse(Permit.Returned.Status == "Permit returned", 1, 
                                       ifelse(Permit.Returned.Status == "Permit returned - did not fish", 2, 
                                              0)))), 
         status_desc = ifelse(status == 1, "returned & harvest reported", 
                              ifelse(status == 2, "returned - did not fish", "not returned"))) %>% 
  # need to group by crab year not calendar year
  #mutate(crab_year = ifelse(Year == cur_yr, cur_yr, 
                            #ifelse(Year == prv_yr & Season == "S", cur_yr, 
                                   #ifelse(Year == prv_yr-1 & Season == "S", prv_yr, prv_yr)))) %>% 
  mutate(crab_year = case_when(
    Season == "S" ~ Year,
    Season == "W" ~ Year-1
  )) %>%
  group_by(crab_year, Season, status, status_desc) %>% 
  summarise(number = sum(number, na.rm = T), pots = sum(pots, na.rm = T), n = sum(n))%>% 
  mutate(cpue = number/pots, cpue_permits = number/n) %>%
  filter(Season %in% c("S", "W"))

# total permits by year and season
total_perm <- by_status_18b %>%
  group_by(crab_year, Season) %>%
  summarise(season_permits = sum(n, na.rm=TRUE))

# number of permits not returned by year and season
no_return <- by_status_18b %>%
  filter(status == 0) %>%
  mutate(season_no_return = n) %>%
  ungroup() %>%
  select(crab_year, Season, season_no_return)

# number of permits fished by year and season
p_fished <- by_status_18b %>%
  filter(status == 1) %>%
  mutate(season_fished = n) %>%
  ungroup() %>%
  select(crab_year, Season, season_fished)

# number of permits returned but not fished by year and season
p_notfished <- by_status_18b %>%
  filter(status == 2) %>%
  mutate(season_notfished = n) %>%
  ungroup() %>%
  select(crab_year, Season, season_notfished)

# combine these permit tables
perm_tab <- total_perm %>%
  right_join(no_return) %>%
  right_join(p_fished) %>%
  right_join(p_notfished) %>%
  mutate(per_no_return = (season_no_return/season_permits)*100, per_fished = (season_fished/season_permits)*100, per_notfished = (season_notfished/season_permits)*100)

# unadjusted harvest
total_c_18 <- by_status_18b %>% 
  group_by(crab_year, Season) %>% 
  summarise(total_c = sum(number, na.rm = TRUE))

## estimated number ----
# previous notes on personal use suggest that an equation was used to estimate harvest from those permits
#   that were not returned
#     
# percent not returned 
pu_summary_18 <- by_status_18b %>% 
  right_join(total_c_18) %>% 
  group_by(crab_year, Season, status) %>% # only looking at one season here
  summarise(n = sum(n, na.rm = TRUE), 
            number = sum(number, na.rm = TRUE), 
            pots = sum(pots, na.rm = TRUE), 
            total_c = total_c) %>% 
  select(crab_year, Season, total_c, status, n) %>% 
  spread(status, n) %>% 
  as.data.frame() %>% 
  mutate(pct.r.that.fished = (`1`) / (`1` + `2`), 
         pnr = (`0`) / (`1` + `2` +`0`),  
         total_permits = (`1` + `2` +`0`), 
         adjustment = (total_permits / (total_permits - 0.762*(`0`))), 
         est.total.catch.numbers = adjustment*as.numeric(total_c))

est_catch <- pu_summary_18 %>%
  select(crab_year, Season, est.total.catch.numbers) %>%
  right_join(perm_tab) #%>%
  #mutate(per_total = sum(per_fished, per_notfished, per_no_return))

pu_tab <- est_catch %>%
  pivot_wider(names_from = Season, values_from = c(season_permits, season_no_return, season_fished, season_notfished, per_no_return, per_fished, per_notfished, est.total.catch.numbers)) 

pu_tab_export <- pu_tab %>%
  mutate(Year = crab_year, `Summer Permits` = season_permits_S, `Summer % fished` = round(per_fished_S, 0), `Summer % returned but not fished` = round(per_notfished_S, 0), `Summer % not returned` = round(per_no_return_S, 0), `Summer estimated harvest` = round(est.total.catch.numbers_S, 0), `Winter Permits` = season_permits_W, `Winter % fished` = round(per_fished_W, 0), `Winter % returned but not fished` = round(per_notfished_W, 0), `Winter % not returned` = round(per_no_return_W, 0), `Winter estimated harvest` = round(est.total.catch.numbers_W, 0), `Total harvest` = round(est.total.catch.numbers_S + est.total.catch.numbers_W, 0)) %>%
  select(Year, `Summer Permits`, `Summer % fished`, `Summer % returned but not fished`, `Summer % not returned`, `Summer estimated harvest`, `Winter Permits`, `Winter % fished`, `Winter % returned but not fished`, `Winter % not returned`, `Winter estimated harvest`, `Total harvest`)

# export

write.csv(pu_tab_export, paste0(here::here(), "/results/rkc/Juneau/PU_since_2018_updated2023.csv"), row.names = F)
