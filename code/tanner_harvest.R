#K.Palof 
# ADF&G 11-2-16 / 10-4-18 / 10-16-19 / 11-9-2020/ 11-2-21/ 10-19-22
# data from OceanAK summarize for use in Tanner CSA's 
# have to modify the output from "detailed fish tickets" need to add "Number of Animals...sum" to this.

# commercial catch - report called 'tanner_harvest'
# need to run 'tanner_logbook.R' prior to this script

# Load ---------------------------
library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(gridExtra)
library(lubridate)

#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
cur_yr = 2025
pr_yr = cur_yr-1
output_path <- paste0('results/tanner/harvest/', cur_yr) # output and results
#dir.create(output_path) 

# Data ---------------------------------------------------
# change input file to most recent year's catch from OceanAK for each
harvest <- read.csv(paste0('./data/harvest/tanner_harvest_',cur_yr,'.csv')) # From oceanAK- in agreich folder now - tanner_harvest_se- UPDATE EVERY YEAR
#harvest <- read.csv("./data/harvest/2025 Detailed Fish Tickets_Tanner.csv")  #got from Tessa's OceakAK search, adding  Batch Year and Sum of Animals to the query.
glimpse(harvest)

#harvest_all <- read.csv("./data/Tanner_Detailed Fish Tickets_98_18.csv")
harvest_all <- read.csv(paste0('./results/tanner/harvest/', pr_yr, '/comm_catch_by_statarea_97_', pr_yr,'.csv'))
#harvest_all <- read.csv("./data/harvest/Tanner_Detailed Fish Tickets_97_18.csv")
logb11510 <- read.csv(paste0('./results/tanner/harvest/', cur_yr,'/logbook_11510_all.csv')) # from tanner_logbook.R calculations

#aside test AGR TK- DELETE THIS BLOC
#test <- read.csv(paste0('./data/harvest/tanner_logbook_', cur_yr,'.csv'))
#test2 <- test %>% filter(Year>2019)
#test3 <- read.csv(paste0('./data/harvest/tanner_logbook_2020_', pr_yr,'.csv'))
#library(diffr)
#diffr(test2, test3)

# these are for calculating std cpue
#logbook <- read.csv(paste0('./data/harvest/tanner_logbook_', cur_yr,'.csv')) %>% filter(Year > 2019)
logbook <- read.csv(paste0('./data/harvest/tanner_logbook_2020_', cur_yr,'.csv')) # get this from Oracle and name for the current year
logbook_all <- read_excel(path = "./data/harvest/All_logbook_tanner.xlsx", sheet = "AlexData") # from ALEX not in OCEAN AK; the historical data, it's in the folder- AGR
# only goes to 2019- which is why we have logbook to bind with that

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

#View(harvest2 %>% filter(survey.area =="Stephens Passage"))
#dat %>%
#  filter(Stat.Area == 11510, Season == 'Sep2015 - Aug16') %>%
#  select(Season, CFEC, Stat.Area, )

### current year by survey area --------------------------
harvest2 %>%
#  filter(Date.of.Landing != '2018-07-13 00:00:00') %>% 
  group_by(Year, survey.area)%>%
  summarise(permits = length(unique(CFEC)), numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.)) -> comm.catch.sum #TK AGR there is an NA here, revisit pease

# lynn sister and north juneau need to be manually split up in area 115-10
write.csv(comm.catch.sum, paste0('./results/tanner/harvest/', cur_yr, '/tanner_comm_catch', cur_yr,'.csv'))
### current year mid-catch date ------------------
harvest2 %>%
  #filter (Season == "Sep2017 - Aug18") %>% 
 # filter(Date.of.Landing != '2018-07-13 00:00:00') %>% 
  group_by(Year, survey.area, Date.of.Landing) %>%
  summarise(numbers = sum(Number.Of.Animals)) -> mid.catch

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


### all years ---------------------- #TK AGR  IDK what is going on heree
 
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
  select(Year = YEAR, percentNJ = percent) %>% 
  mutate(year_caught = Year) %>% # this has 'Year' as "year caught" NOT fishery year - fix this above.
  select(-Year) -> logb_merge

#stat_11510 <- harvest_all_update %>% #AGR deactivted chunk
#harvest_all %>%  # placeholder for updates made in season after initial calcs are done
 # filter(Stat.Area == 11510) %>% 
#  left_join(logb_merge) %>% # this has 'Year' as "year caught" NOT fishery year - fix this above.
 # mutate(no_NJ = numbers*percentNJ,
  #       no_LS = numbers*(1-percentNJ), 
   #      lb_NJ = pounds*percentNJ,
    #     lb_LS = pounds*(1-percentNJ)) %>% 
 # select(Year, Stat.Area, vessels, people, permits, processor, no_NJ, no_LS, lb_NJ, lb_LS) %>% 
#  gather("label", "value", 7:10) %>% 
 # mutate(survey.area = case_when(grepl("NJ", label, ignore.case = TRUE) ~ "North Juneau",
   #                              grepl("LS", label, ignore.case = TRUE) ~ "Lynn Sisters"), 
    #     units = case_when(grepl("no", label, ignore.case = TRUE) ~ "numbers", 
     #                      grepl("lb", label, ignore.case = TRUE) ~ "pounds")) %>% 
#  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, units, value) %>% 
 # spread(units, value) %>% 
#  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, numbers, pounds, Year) %>% 
 # mutate(year_caught = Year + 1) #this does not work - AGR TK

# alternative version since the above one wasn't working - Caitlin's comment #AGR TK here - I have an error here
#stat_11510 <- harvest_all_update %>% #AGR TK 
  #harvest_all %>%  # placeholder for updates made in season after initial calcs are done #AGR TK activated
#  filter(Stat.Area == 11510) %>% 
 # left_join(logb_merge, by= "year_caught") %>% # this has 'Year' as "year caught" NOT fishery year - fix this above.
#  mutate(no_NJ = case_when(survey.area %in% c("North Juneau", "Other") ~ numbers*percentNJ, .default = NA)) %>% 
#  mutate(no_LS = case_when(survey.area %in% c("Lynn Sisters", "Other") ~ numbers*(1 - percentNJ), .default = NA)) %>% 
# mutate(lb_NJ = case_when(survey.area %in% c("North Juneau", "Other") ~ pounds*percentNJ, .default = NA)) %>% 
 # mutate(lb_LS = case_when(survey.area %in% c("Lynn Sisters", "Other") ~ pounds*(1 - percentNJ), .default = NA)) %>% 
#  select(Year, Stat.Area, vessels, people, permits, processor, no_NJ, no_LS, lb_NJ, lb_LS) %>% 
 # gather("label", "value", 7:10) %>% 
#  filter(is.na(value) == FALSE) %>%
 # mutate(survey.area = case_when(grepl("NJ", label, ignore.case = TRUE) ~ "North Juneau",
  #                               grepl("LS", label, ignore.case = TRUE) ~ "Lynn Sisters"), 
   #      units = case_when(grepl("no", label, ignore.case = TRUE) ~ "numbers", 
    #                       grepl("lb", label, ignore.case = TRUE) ~ "pounds")) %>% 
#  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, units, value) %>% 
 # spread(units, value) %>% 
#  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, numbers, pounds, Year) %>% 
 # mutate(year_caught = Year + 1) 


#AGR tK error above - this one also does not work

###START CAITLIN ADD OCT 24
stat_11510 <- harvest2_cur %>% 
  #harvest_all %>%  # placeholder for updates made in season after initial calcs are done
  filter(Stat.Area == 11510) %>% 
  left_join(logb_merge) %>% # this has 'Year' as "year caught" NOT fishery year - fix this above.
  mutate(no_NJ = case_when(survey.area %in% c("North Juneau", "Other") ~ numbers*percentNJ, .default = NA)) %>% 
  mutate(no_LS = case_when(survey.area %in% c("Lynn Sisters", "Other") ~ numbers*(1 - percentNJ), .default = NA)) %>% 
  mutate(lb_NJ = case_when(survey.area %in% c("North Juneau", "Other") ~ pounds*percentNJ, .default = NA)) %>% 
  mutate(lb_LS = case_when(survey.area %in% c("Lynn Sisters", "Other") ~ pounds*(1 - percentNJ), .default = NA)) %>% 
  select(Year, Stat.Area, vessels, people, permits, processor, no_NJ, no_LS, lb_NJ, lb_LS) %>% 
  gather("label", "value", 7:10) %>% 
  filter(is.na(value) == FALSE) %>%
  mutate(survey.area = case_when(grepl("NJ", label, ignore.case = TRUE) ~ "North Juneau",
                                 grepl("LS", label, ignore.case = TRUE) ~ "Lynn Sisters"), 
         units = case_when(grepl("no", label, ignore.case = TRUE) ~ "numbers", 
                           grepl("lb", label, ignore.case = TRUE) ~ "pounds")) %>% 
  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, units, value) %>% 
  spread(units, value) %>% 
  select(Year, Stat.Area, survey.area, vessels, people, permits, processor, numbers, pounds, Year) %>% 
  mutate(year_caught = Year + 1)

harvest_all_update2 <- harvest_all_update %>%
  #harvest_all %>%  
  filter(!(Stat.Area == 11510 & year_caught == cur_yr)) %>% 
  bind_rows(stat_11510)

#END CAITLIN ADD OCT 24

#AGR ADD Oct 16 2024
##mid date catch for lynn sistsers. Lynn sisters is two stat areas, ask caitlin to explain it to me later
mid.catch.ls <- harvest2 %>% filter(Stat.Area %in% c(11510, 11215)) %>% arrange(Date.Fishing.Began) %>%
  
  group_by(Date.Fishing.Began) %>%
  
  summarise(numbers = sum(Number.Of.Animals),
            survey.area = max(survey.area),
            Year= max(Year))

mid.catch.ls %>% 
  
  group_by(survey.area, Year) %>% 
  
  summarise(total = sum(numbers)) -> step1.ls

mid.catch.ls %>% 
  
  left_join(step1.ls) %>% 
  
  mutate(ratio_catch = numbers/total) -> mid.catch2.ls

write.csv(mid.catch2.ls, paste0('./results/tanner/harvest/', cur_yr, '/lynnsisters_middate', cur_yr,'.csv'))
##end AGR add

### Deal with 11510 -----------
# - take it out manipulate it above and add it back in
#harvest_all_update %>%
#harvest_all %>%  
 # filter(Stat.Area != 11510) %>% 
#  bind_rows(stat_11510) -> harvest_all_update2

# !! this has update catch distribution between LS and NJ for stat area 11510
write.csv(harvest_all_update2, #pretty sure this should be the data frame with the new 11510
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


## std cpue -------------

## data manipulation -------

## current year ---------

logbook1a <- logbook %>% 
  rename_at(1, ~"Year") %>%
  filter(Year >= 2020) %>% 
  select(Year, effort.date = Entry.Date, District, 
         Sub.district, ADFG_NO = ADFG.Number, pots = Number.of.Pots.Lifted, 
         numbers = Target.Species.Retained) %>% 
  mutate(effort.date = as.Date(ymd_hms(effort.date))) %>% 
  as.data.frame()

logbook1 <- logbook1a %>% 
  mutate(day = strftime(effort.date, format = "%j"))

# std cpue current year -------
logbook1 %>% 
  filter(pots > 0) %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  arrange(day) %>% 
  mutate(cum.pots = cumsum(pots), cpue = numbers/pots) %>% 
  filter(cum.pots <= 12521) %>% 
  summarise(avg.cpue = mean(cpue), 
            se = sd(cpue)/sqrt(length(cpue)))


## all years -------------
# add current years data 
#logbook_all %>% 
#  bind_rows(logbook) -> logbook_all

# need to convert effort date to day of year
logbook_all %>% select(Year = YEAR, effort.date = EFFORT_DATE, District = DISTRICT, 
                       Sub.district = SUB_DISTRICT, ADFG_NO, pots = NUMBER_POTS_LIFTED, 
                       numbers = TARGET_SPECIES_RETAINED) %>% 
  as.data.frame() -> logbook_all1 

logbook_all1 %>% 
  mutate(day = strftime(effort.date, format = "%j")) -> logbook_all1

## add years since 2019 since that's all I have in all logbook data...need to **fix** this. #TK AGR has this been fixed?? Watch out for rbinding repeating years into the final df...
logbook_all1 %>% 
  bind_rows(logbook1) -> logbook_all_cur
### std cpue -----------------
## determine cumulative pots ordered by day
cpue_by_year <- logbook_all_cur %>% 
  filter(pots >0) %>% 
  arrange(day) %>% 
  group_by(Year) %>% 
  arrange(day) %>% 
  mutate(cum.pots = cumsum(pots), cpue = numbers/pots) %>% 
  filter(cum.pots <= 12521) %>% 
  summarise(avg.cpue = mean(cpue, na.rm = TRUE), 
            se = sd(cpue, na.rm = TRUE)/sqrt(length(cpue))) %>% 
  as.data.frame()

write_csv(cpue_by_year, paste0('./results/tanner/harvest/', cur_yr, '/std_commercial_cpue' , cur_yr, '.csv'))
