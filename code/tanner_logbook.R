# K.Palof 10-16-18 updated / 10-16-19 / 11-9-2020 / 11-3-2021/ 10-19-22
# Code to review logbook data for Tanner crab fishery.
# needed to seperate out catch for Lynn Sisters and North Juneau, previously done in .JMP and Excel

# logbook data from ALEX (as of 10-16-18) pull only data from district 115 - have to sort here for sub-district 10
#    since that's all that is needed. Pull for all years and save file in results/tanner 


#####Load Packages ---------------------------------
library(tidyverse)
library(readxl)

## global ------
cur_yr <- 2022 # update annually

#####Load Data -------------------------------------
# change input file and input folder for each
#logb <- read_excel(path = "./data/harvest/tanner_logbook_cur_yr.xlsx", sheet = "tanner_115")
# take the current year logbook data and seperate out only district 115 -could also do this in here....
logb <- read.csv(paste0('./data/harvest/tanner_logbook_', cur_yr, '.csv')) # all data (2021)
#logb_all <- read.csv("./data/harvest/logbook_11510_98_18.csv") # bring in previous yrs percentages
logb_all <- read.csv(paste0('./results/tanner/harvest/', cur_yr-1, '/logbook_11510_all.csv')) # bring in previous yrs percentages
# This is the master file with the percentage of catch in each year that is attributed to NJ or LS - it is added to and saved at the end 
#    of this file for the following year
glimpse(logb)

# need data and comments from stat area 115-10
logb %>% 
  filter(District == 115 & Sub.district == 10) %>% 
  select(Year, ADFG.Number, Entry.Date, District, Sub.district, Area.Description, Number.of.Pots.Lifted, 
         Target.Species.Retained, Comments) ->log11510

# sort into LS or NJ -----
#unique(log11510$Area.Description)
log11510 %>% 
  mutate(survey.area = case_when(grepl("james", Area.Description, ignore.case = TRUE) ~ "Lynn Sisters",
                                 grepl("sister", Area.Description, ignore.case = TRUE) ~ "Lynn Sisters",
                                 grepl("berner", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("ben", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("eagle", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("island", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("end", Area.Description, ignore.case = TRUE) ~ "Lynn Sisters", 
                                 grepl("sher", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("er", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("sher", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("mary", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("stone", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("pt", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("boat", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 grepl("canal", Area.Description, ignore.case = TRUE) ~ "North Juneau", 
                                 (Year == 2000 & is.na(Area.Description)) ~ "Lynn Sisters", 
                                 (Year == 2011 & is.na(Area.Description)) ~ "Lynn Sisters", 
                                 (Year == 2016 & is.na(Area.Description)) ~ "North Juneau", 
                                 (Year == 2017 & is.na(Area.Description)) ~ "North Juneau")) -> log11510
# deal with NA's 
# 1) looking at permit holder and effort date -
# 2) other knowledge - pers comm with shellfish group


# % from each by Year -------
# total per year 
log11510 %>% 
  group_by(Year) %>% 
  summarise(total_no = sum(Target.Species.Retained)) -> total_no

log11510 %>% 
  group_by(survey.area, Year) %>% 
  summarise(crabs = sum(Target.Species.Retained), 
            pots = sum(Number.of.Pots.Lifted)) %>% 
  left_join(total_no) %>% 
  mutate(percent = crabs/total_no, YEAR = Year) %>% ## edits due to pulling data from OceanAK and having column name changes
  select(survey.area, YEAR, crabs, pots, total_no, percent) -> percent_assigned_cur

write.csv(percent_assigned_cur, paste0('./results/tanner/harvest/', cur_yr,'/logbook_11510_', cur_yr,'.csv'))


# merge with previous years -------------
logb_all %>% 
  select(-X) %>% 
  bind_rows(percent_assigned_cur) -> log_all
write.csv(log_all, paste0('./results/tanner/harvest/', cur_yr, '/logbook_11510_all.csv'))
