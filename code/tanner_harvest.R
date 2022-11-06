#K.Palof 
# ADF&G 11-2-16 / 10-4-18 / 10-16-19 / 11-9-2020/ 11-2-21/ 10-19-22
# data from OceanAK summarize for use in Tanner CSA's 
# have to modify the output from "detailed fish tickets" need to add "Number of Animals...sum" to this.

# commercial catch - report called 'tanner_harvest'
# need to run 'tanner_logbook.R' prior to this script

# Load ---------------------------
library(tidyverse)
cur_yr = 2022
pr_yr = cur_yr-1
output_path <- paste0('results/tanner/harvest/', cur_yr) # output and results
dir.create(output_path) 

# Data ---------------------------------------------------
# change input file to most recent year's catch from OceanAK for each
harvest <- read.csv("./data/harvest/tanner_harvest.csv")
glimpse(harvest)

#harvest_all <- read.csv("./data/Tanner_Detailed Fish Tickets_98_18.csv")
harvest_all <- read.csv(paste0('./results/tanner/harvest/', pr_yr, '/comm_catch_by_statarea_97_', pr_yr,'.csv'))
#harvest_all <- read.csv("./data/harvest/Tanner_Detailed Fish Tickets_97_18.csv")
logb11510 <- read.csv(paste0('./results/tanner/harvest/', cur_yr,'/logbook_11510_all.csv')) # from tanner_logbook.R calculations


# data clean up --------
## need to update this output from OceanAK to be only region 1....**FIX**
harvest %>% 
  filter(Batch.Year == cur_yr & Office.Name == 'Petersburg') %>% 
  dplyr::rename(Year = Batch.Year) -> harvest2
  
### current year ----------------------
unique(harvest2$Stat.Area)
# need to create column that does what 'Survey area 3' does in Excel sheet
# refer to '2014-2015 fish tickets.xlsx'
harvest2 %>%
  mutate(survey.area = ifelse(Stat.Area ==11023, 'Gambier Bay', ifelse(Stat.Area == 11423, 'Icy Strait', 
                        ifelse(Stat.Area == 11470, 'Glacier Bay', ifelse(Stat.Area == 11012, 
                        'Thomas Bay', ifelse(Stat.Area == 11150| Stat.Area == 11155, 'North Juneau', 
                         ifelse(Stat.Area ==11021 | Stat.Area ==11022, 'Pybus Bay', 
                         ifelse(Stat.Area == 11480 |Stat.Area ==11425, 'Excursion Inlet', 
                         ifelse(Stat.Area==11120 | Stat.Area ==11121, 'Holkham Bay', 
                         ifelse(Stat.Area==11140|Stat.Area==11141|Stat.Area==11142|Stat.Area==11143,
                             'Stephens Passage', 
                         ifelse(Stat.Area == 11351|Stat.Area == 11352|Stat.Area == 11353|
                         Stat.Area == 11354|Stat.Area == 11355|Stat.Area == 11356|Stat.Area == 11357|
                         Stat.Area == 11358, 'Peril Strait', 
                         ifelse(Stat.Area == 11101|Stat.Area == 11102|Stat.Area == 11103|
                         Stat.Area == 11104|Stat.Area == 11105|Stat.Area == 11106|
                         Stat.Area == 11107|Stat.Area == 11108|Stat.Area == 11109|
                         Stat.Area == 11110|Stat.Area == 11111|Stat.Area == 11112|
                         Stat.Area == 11113|Stat.Area == 11114|Stat.Area == 11115|Stat.Area == 11116|
                         Stat.Area == 11117|Stat.Area == 11118, 'Seymour Canal', 
                         ifelse(Stat.Area == 11431|Stat.Area == 11432|Stat.Area == 11433|Stat.Area == 11434, 
                            'PFred', 
                         ifelse(Stat.Area == 11215, 'Lynn Sisters', 
                         ifelse(Stat.Area == 10940|Stat.Area == 10941|Stat.Area == 10942|Stat.Area == 10943|
                                Stat.Area ==10532, 'Camden', 'Other')))))))))))))))  -> harvest2
# remove 11511 from Lynn Canal - make it part of 'other'
# by stat area, not needed for this analysis
harvest2 %>%
 # filter(Date.of.Landing != '2018-07-13 00:00:00') %>% # not sure why this was in place last year but not present now
  group_by(Year, Stat.Area, survey.area) %>% # no season in current year's data - not sure why? **FIX**
  summarise(permits = length(unique(CFEC)), 
                             numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> harvest2a

write.csv(harvest2a, paste0('./results/tanner/harvest/', cur_yr,'/comm_catch_by_statarea', cur_yr,'.csv'))
#dat %>%
#  filter(Stat.Area == 11510, Season == 'Sep2015 - Aug16') %>%
#  select(Season, CFEC, Stat.Area, )

### current year by survey area --------------------------
harvest2 %>%
#  filter(Date.of.Landing != '2018-07-13 00:00:00') %>% 
  group_by(Year, survey.area)%>%
  summarise(permits = length(unique(CFEC)), numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> comm.catch.sum

# lynn sister and north juneau need to be manually split up in area 115-10
write.csv(comm.catch.sum, paste0('./results/tanner/harvest/', cur_yr, '/tanner_comm_catch', cur_yr,'.csv'))
### current year mid-catch date ------------------
harvest2 %>%
  #filter (Season == "Sep2017 - Aug18") %>% 
 # filter(Date.of.Landing != '2018-07-13 00:00:00') %>% 
  group_by(Year, survey.area, Date.of.Landing) %>%
  summarise(numbers = sum(Number.Of.Animals)) ->mid.catch

mid.catch %>% 
  group_by(survey.area, Year) %>% 
  summarise(total = sum(numbers)) -> step1
mid.catch %>% 
  left_join(step1) %>% 
  mutate(ratio_catch = numbers/total) -> mid.catch2

write.csv(mid.catch2, paste0('./results/tanner/harvest/', cur_yr,'/tanner_mid_catch_date', cur_yr, '.csv'))

### current year total annual harvest  ---------------------
comm.catch.sum %>%
  group_by(Year)%>%
  summarise(numbers = sum(numbers), pounds = sum(pounds)) -> annual_catch

write.csv(annual_catch, paste0('./results/tanner/harvest/', cur_yr, '/tanner_annual_catch_', cur_yr,'.csv'))


### all years ----------------------
 
# remove 11511 from Lynn Canal - make it part of 'other'
# by stat area, not needed for this analysis

# need this year to (be the beginning of the season year range NOT the end 
#  so in 2018/2019 season - it's 2019 but I need 2018
harvest2 %>%
  mutate(Year = Year-1) %>% 
  group_by(Year, Stat.Area, survey.area) %>%
  summarise(vessels = length(unique(ADFG.Number)), 
            people = length(unique(CFEC)),
            permits = length(unique(Permit.Holder.Name)), #permits = length(unique(Permit.Serial.Number)), 
            processor = length(unique(Processor.Code)),
            numbers = sum(Number.Of.Animals, na.rm = TRUE), 
            pounds = sum(Whole.Weight..sum., na.rm = TRUE)) %>% 
  mutate(year_caught = Year +1) -> harvest2_cur

### all years by survey area --------------------------

# SKIP only need to run this if pulling data from OceanAK and not from previous year's file ----
#  This is needed if you have season....current file has year caught...need to adjust this.
# need a season reference column in terms of years
#library(stringr)
#numextract <- function(string){ 
#  str_extract(string, "\\-*\\d+\\.*\\d*")
#} 

#harvest_all %>% 
#  mutate(Year = as.numeric(numextract(Season))) %>% 
#  select(-X, -Season) -> harvest_all2

#harvest_all2 %>% 
#  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, numbers, pounds) %>% 
#  bind_rows(harvest2_cur) -> harvest_all_update
# add correct year ----
# Combine current year ---------
harvest_all %>% 
  #mutate(year_caught = Year, Year = Year - 1) %>% # check here and make sure end year is cur_yr above -2, i.e. cur_yr = 2020, end year is 2018
  #select(-X) %>% 
  bind_rows(harvest2_cur) -> harvest_all_update

# Combine current year ---------

## merge logbook ----
# this is just to deal with 11510 - which was called "other" above but needs to be divided 
#     between North Juneau and Lynn Sisters.
logb11510 %>% 
  filter(survey.area == "North Juneau") %>% 
  select(Year = YEAR, percentNJ = percent) -> logb_merge

harvest_all_update %>% 
#harvest_all %>%  # placeholder for updates made in season after initial calcs are done
  filter(Stat.Area == 11510) %>% 
  left_join(logb_merge) %>% 
  mutate(no_NJ = numbers*percentNJ,
         no_LS = numbers*(1-percentNJ), 
         lb_NJ = pounds*percentNJ,
         lb_LS = pounds*(1-percentNJ)) %>% 
  select(Year, Stat.Area, vessels, people, permits, processor, no_NJ, no_LS, lb_NJ, lb_LS) %>% 
  gather("label", "value", 7:10) %>% 
  mutate(survey.area = case_when(grepl("NJ", label, ignore.case = TRUE) ~ "North Juneau",
                                 grepl("LS", label, ignore.case = TRUE) ~ "Lynn Sisters"), 
         units = case_when(grepl("no", label, ignore.case = TRUE) ~ "numbers", 
                           grepl("lb", label, ignore.case = TRUE) ~ "pounds")) %>% 
  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, units, value) %>% 
  spread(units, value) %>% 
  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, numbers, pounds, Year) %>% 
  mutate(year_caught = Year + 1) -> stat_11510


### Deal with 11510 -----------
# - take it out manipulate it above and add it back in
harvest_all_update %>%
#harvest_all %>%  
  filter(Stat.Area != 11510) %>% 
  bind_rows(stat_11510) -> harvest_all_update2

# !! this has update catch distribution between LS and NJ for stat area 11510
write.csv(harvest_all_update, 
          paste0('./results/tanner/harvest/', cur_yr, '/comm_catch_by_statarea_97_', cur_yr,'.csv'), row.names = F)

harvest_all_update2 %>% 
  group_by(survey.area, Year) %>%
  summarise(vessels = sum(vessels), people = sum(people),
            permits = sum(permits), processors = sum(processor), 
            numbers = sum(numbers), 
            pounds = sum(pounds)) -> comm.catch.sum_all

# lynn sister and north juneau need to be manually split up in area 115-10 (see above done prior)
write.csv(comm.catch.sum_all, paste0('./results/tanner/harvest/', cur_yr, '/tanner_comm_catch_97_', cur_yr,'.csv'))
### !!!!!!  These may not be correct for North Juneau, Stephens Passage and Lynn Sisters due to shared stat areas
##                    CHECK these with old excel files before going forward.
# checked harvest with sigma plot file:
# good: EI, PB, GB, SC, PS
# needs correcting using logbooks: NJ, LS, SP
# needs to be checked: GLB, IS, TB, HB

### all years total annual harvest  ---------------------
comm.catch.sum_all %>%
  group_by(Year)%>%
  summarise(numbers = sum(numbers), pounds = sum(pounds)) -> annual_catch_all

write.csv(annual_catch_all, paste0('./results/tanner/harvest/', cur_yr, '/tanner_annual_catch_97_', cur_yr,'.csv'))


# percent of total catch current year -----------
comm.catch.sum_all %>% 
  filter(Year > 2015) %>% 
  select(survey.area, Year, permits, lb_by_yr = pounds) %>% 
  left_join(annual_catch_all) %>% 
  mutate(percent_total = lb_by_yr/pounds*100) %>% 
  as.data.frame() %>% 
  write_csv(paste0('./results/tanner/harvest/', cur_yr, '/proportion_total_harvest_', cur_yr,'.csv'))
# **NOTE** year range was off prior to 2019. should be 1 year later - this was fixed previous. Now is OK if used summarized file.

## confidential catch -------------
comm.catch.sum_all %>% 
  filter(survey.area != "Camden", survey.area != "PFred") %>% 
  filter(permits < 3 | vessels < 3 | people < 3) %>% 
  as.data.frame()


comm.catch.sum_all %>% 
  mutate(confidential = ifelse(permits < 3 | vessels < 3 | people < 3, "y", "n")) -> comm.catch.sum_all_C
write.csv(comm.catch.sum_all_C, paste0('./results/tanner/harvest/', cur_yr, '/tanner_comm_catch_97_', cur_yr,'_confid.csv'))
