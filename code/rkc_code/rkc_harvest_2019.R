# K.Palof  ADF&G
# 7-8-19 updated for 2018 fishery
# code here summarizes harvest data for the current year RKC
# this code is set for the 2017 fishery.

# Code added at the end of this file to estimate the harvest rate for 2017 fishery.
#     Need to have the updated 2018 biomass estimates file for this.
# harvest data from fish tickets in OceanAK summarize for use in Southeast RKC CSA's 
# have to modify the output from "detailed fish tickets" need to add "Number of Animals...sum" to this.

# commercial catch

####Load Packages ---------------------------------
source('./code/functions.R')
cur_yr = 2019
library(xlsx)

#####Load Data ---------------------------------------------------
# change input file and input folder for each
harvest <- read.csv("./data/redcrab/RKC_fish_tickets_2017.csv") #
# data pulled from OceanAK August 23rd, 2018
#harvest_A <- read.csv("./data/redcrab/RKC_Detailed Fish Tickets_aug23.csv") 
# from OceanAK detailed fish tickets 
glimpse(harvest)
survey.area <- read.xlsx('data/redcrab/rkc_biomass_2017_model.xlsx', sheetName = "Sheet2") #stat area to survey area conversion
personal_use <- read.csv("./data/redcrab/personal_use_RKC_11A_2017.csv")
  
### clean up  ------
unique(harvest$Species.Code.and.Name)
# only works for 2017 due to issues with 115-10
harvest %>% 
  select(Species.Class, Season, CFEC, ADFG.Number, Date.of.Landing, Stat.Area, 
         Species.Code.and.Name, Number.Of.Animals, Whole.Weight..sum., Pot.Lifts) %>%  
  rename(stat.area = Stat.Area) %>% 
  left_join(survey.area) %>% 
  mutate(survey.area = ifelse(stat.area == 11510, 'juneau', 
                              as.character(survey.area))) -> harvest2


harvest_A %>% 
  select(Species.Class, Season, CFEC, ADFG.Number, Date.of.Landing, Stat.Area, 
         Species.Code.and.Name, Number.Of.Animals, Whole.Weight..sum., Pot.Lifts) %>%  
  rename(stat.area = Stat.Area) %>% 
  left_join(survey.area) -> harvest2_A

# by stat area ----
harvest2 %>% 
  group_by(Season, stat.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) -> catch_by_stat
harvest2_A %>% 
  group_by(Season, stat.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) -> catch_by_stat_A

# by stat area and survey area ----
harvest2 %>% 
  group_by(Season, stat.area, survey.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) -> catch_by_stat_survey

harvest2_A %>% 
  group_by(Season, stat.area, survey.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) -> catch_by_stat_survey_A
# by survey area ----
harvest2 %>% 
  group_by(Season, survey.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) %>% 
  mutate(avg_wt = pounds/numbers, 
         proportion = pounds/(sum(pounds))) -> catch_by_survey
harvest2_A %>% 
  group_by(Season, survey.area) %>% 
  summarise(permits = length(unique(CFEC)), 
            numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) %>% 
  mutate(avg_wt = pounds/numbers, 
         proportion = pounds/(sum(pounds))) -> catch_by_survey_A

write.csv(catch_by_stat, './results/redcrab/comm_catch_by_statarea.csv')
write.csv(catch_by_survey, './results/redcrab/comm_catch_by_surveyarea.csv')
write.csv(catch_by_stat_survey, './results/redcrab/comm_catch_by_stat_and_surveyarea.csv')


### mid-catch date ------------------
harvest2 %>%
  group_by(survey.area, Date.of.Landing) %>%
  summarise(numbers = sum(Number.Of.Animals)) ->mid.catch
write.csv(mid.catch, './results/redcrab/rkc_mid_catch_date.csv')

### total annual harvest  ---------------------
harvest2 %>%
  group_by(Season)%>%
  summarise(numbers = sum(Number.Of.Animals), 
            pounds = sum(Whole.Weight..sum.), 
            pots = sum(Pot.Lifts, na.rm = TRUE)) %>% 
  mutate(avg.wt = pounds/numbers, cpue = numbers/pots) -> annual_catch

write.csv(annual_catch, './results/redcrab/rkc_annual_catch_17.csv')


### personal use -------------------
### estimated HR cur yr ---------------------------------
biomass <- read.csv("./data/redcrab/biomass.csv") 
# 2018 biomass estimates for each survey area from the CSA model.
# use harvest in this file since it include PU harvest estimates - see exploitation
#         rate analysis project for more details on this.
mr_adjust <- read.csv('./data/redcrab/adj_final_stock_assessment.csv')

# estimated harvest rate for 2017 by survey area -----
biomass %>% 
  filter(Year == 2017) %>% 
  mutate(HR = ifelse(Location == "Juneau", round(harvest/mature.biomass*100, 2), 
                     round(harvest/adj.mature*100, 2))) -> est_HR
write.csv(est_HR, (paste0('./results/redcrab/estimate_HR', 
                                       cur_yr,'.csv')))

# % of baseline for each area ------------    
mr_adjust %>% 
  select(-X) %>% 
  mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
  select(-area) -> mr_adjust2
biomass %>% 
  left_join(mr_adjust2) %>% 
  mutate(adj.legal = legal.biomass*weighted_ADJ, 
         adj.mature = mature.biomass*weighted_ADJ) -> biomass

biomass %>% 
  select(-weighted_ADJ) %>% 
  gather(type, pounds, harvest:adj.legal, factor_key = TRUE) %>% 
  filter(Year >= 1993) -> biomass_graph

biomass_graph %>% 
  filter(Year <= 2007, type == 'mature.biomass') %>% 
  group_by(Location) %>% 
  summarise(mature_base = mean(pounds)) -> baseline_mature_biomass
# want this summarizes as % below or above baseline 
biomass %>% 
  filter(Year ==2018) %>% 
  left_join(baseline_mature_biomass) %>% 
  mutate(percent_away_mature_base = 
           100-(mature.biomass/mature_base*100)) ->percent_mature_base
write.csv(percent_mature_base, (paste0('./results/redcrab/percentage_of_mature_base', 
                                       cur_yr,'.csv')))
