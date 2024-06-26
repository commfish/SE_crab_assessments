# K.Palof   katie.palof@alaska.gov
# ADF&G 8-4-16 updated for Peril Strait(Deadman's Reach)  / updated 8-2-18/ 7-30-19/ 8-23-21/ 7-26-22

# R script contains code to process data from Ocean AK to use in crab CSA models, code to run CSA model, and calls to create 
#     output and figures for annual stock health report.

# Read me:
#     update code with date updated (top), change global year, and pull new survey data (see below)

## load -------------------------
source('./code/functions.R')

## setup global ---------------
cur_yr <- 2023 # udpate annually
pr_yr <- cur_yr -1
survey.location <- 'Peril'   # area is Peril but in data files it's Deadman Reach

dir.create(file.path(paste0('results/rkc/', survey.location), cur_yr))
dir.create(file.path(paste0('text'), cur_yr))

## data -------------------
dat <- read.csv(paste0('./data/rkc/', survey.location,'/RKCsurveyCSA_PS_21_22.csv'))# file name will change annually
# this is input from OceanAK - set up as red crab survey data for CSA
#   survey area should match that in the name of this script file; Deadmans Reach
area <- read.csv(paste0('./data/rkc/', survey.location, '/Peril_strata_area.csv')) # same every year

histdat <- read.csv(paste0('./results/rkc/', survey.location, '/', pr_yr, '/PS_perpot_all_', pr_yr, '.csv'))
## !!!!  this file will be 'EI_perpot_all_pr_yr' and just get updated with current years data.
females <- read.csv(paste0('./results/rkc/', survey.location,'/', pr_yr, '/largef_all.csv'))


baseline <- read.csv("./data/rkc/longterm_means.csv") # same every year
biomass <- read.csv("./data/rkc/biomass.csv") # ** update ** from CSA model
#   file for all locations. Has biomass estimates from CSA,
#   must be updated after CSA model is run for current year USING current year's model
#             NOT historic forecast!


## survey data QAC -------
head(dat)
glimpse(dat) # confirm that data was read in correctly.
sapply(dat, unique)

# remove pots with Pot condition code that's not "normal" or 1 
unique(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)
# pot 40 - 2018 - specimen 1 has no length **fix**

# **FIX **  calculate soak time 
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

## CPUE calc --------------
##### By Pot -------------------------------
# Now summarize by pot - remember to keep areas seperate.
# need Number of Specimens by recruit class
# keep trip no. to merge with historic data 
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

head(dat3)# check to make sure things worked.

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
# Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata


#####Weighted CPUE current year -----------------------------------
# the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.
tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  dplyr::rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
# this is neccessary so that current years file (dat5) matches the historic file names

# This version is ready to calculate CPUE for each recruit class
# Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = weighted.mean(Juvenile, weighting), Juv_SE = (weighted.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = weighted.mean(Large.Females, weighting), MatF_SE = (weighted.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt
CPUE_wt
# check to confirm last years CPUEs match - that's why we use two years.

write.csv(CPUE_wt, paste0('./results/rkc/', survey.location, '/', cur_yr, '/PS_CPUE_',cur_yr, '.csv'), 
          row.names = FALSE)

# weighted cpue by strata --- just for comparison
dat5 %>%
  group_by(Year, Density.Strata.Code) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = weighted.mean(Juvenile, weighting), Juv_SE = (weighted.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = weighted.mean(Large.Females, weighting), MatF_SE = (weighted.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/
                                                                          (sqrt(sum(!is.na(Small.Females)))))) 

#### survey mid date -----
#  ** fix **  make this calculated from data not just visual
head(dat)
unique(dat$Time.Hauled)
# need to seperate time hauled to just have data hauled look for mid-date 
dat %>% filter(Year == cur_yr)  # 7-15
### ***fix *** this needs to be calculated better

##### Historic file ---------------------------------------
# need to add current years pot summary to the historic pot summary file.  
# For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match
head(histdat) 
head(dat5)

histdat %>% 
  select(Year, Location, Trip.No, Pot.No, Strata.Code, Missing, 
                   Juvenile, Large.Females, Post_Recruit, Pre_Recruit, 
                   Recruit, Small.Females, Area, npots, inverse_n, 
                   weighting) -> historicdata
dat5 %>% 
  dplyr::rename(Strata.Code = Density.Strata.Code) -> dat6

# need to add current year to historicdata file
# only current years
dat6 %>%
  filter(Year == cur_yr) -> dat5_cur_yr
CPUE_ALL_YEARS <- rbind(historicdata, dat5_cur_yr)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, paste0('./results/rkc/', 
                                 survey.location, '/', cur_yr, '/PS_perpot_all_', cur_yr,'.csv'), 
                                  row.names = FALSE)

## Trends - short and long and female stats for stock health weighting ---------------
### Short term trends -------------------
#look at trend for the last 4 years.  Need a file with last four years 
CPUE_ALL_YEARS %>%
  filter(Year >= cur_yr - 3) -> bypot_st # short term file has last 4 years in it

#function creates output file in folder /results/rkc/'area'
short_t(bypot_st, cur_yr, "Peril")

# output is saved as shortterm.csv
bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, factor_key = TRUE) 
ggplot(bypot_st_long, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)


##### Long term trends ---------------------
# compare current year CPUE distribution to the long term mean
head(dat5_cur_yr)
# make sure you have a file with only current years data - created above

long_t(dat5_cur_yr, baseline, cur_yr, 'Peril', 'Deadman Reach')
# output is saved as longterm.csv

##### Weights from length - weight relatinship.-----------------
    # Linear model is changed for each area
    # Peril linear model: exp(3.02*log(length in mm)-7.18)*2.2/1000
glimpse(dat1) # raw data for both 2016 and 2017
    # slope = 3.02
    # intercept = 7.18
    # use function found in functions.R code file
weights(dat1, 3.02, 7.18, "Peril", cur_yr)
# output saved as maleweights.csv

###### Females ----------------------------------------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
#LgF_dat1 %>%
# mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1
#need to remove if missing data
#LgF_dat1 %>%
#  filter(!is.na(Egg.Percent)) -> LgF_dat1
LgF_dat1 %>% 
  filter(Year == cur_yr) %>% 
  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_curyr

# Currently (2019) just load the largef_all.csv file and add current year
head(females)

# want to add 0's for egg percent if egg development code is 3 or 4
#LgF_dat1_all %>% 
#  mutate(Egg.Percent = ifelse((Egg.Development.Code == 3 & 
#                                 Egg.Condition.Code == 4 |Egg.Condition.Code == 5), 
#                              0, Egg.Percent)) -> LgF_dat1_all

largef_all <- rbind(females, LgF_dat1_curyr) # raw female data for all years.
write.csv(largef_all, (paste0('./results/rkc/', survey.location, '/', cur_yr, '/', 
                              'largef_all.csv')))

##### % poor (<10 %) clutch -----------------------------------
poor_clutch(largef_all, 'Peril', cur_yr)
# output is saved as poorclutch_current.csv - which has all pots for current year
#     and poorclutch_17.csv which has the percentage and SD of poor clutches for current year 

##### Long term females -------------------------
poorclutch_current <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                      '/poorclutch1_current.csv'))
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Peril', cur_yr)
# output saved as lt_female.csv

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years
poorclutch_all <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                  '/poorclutch_all.csv'))

#function for short term trends and output saving.
poor_clutch_short(poorclutch_all, 'Peril', cur_yr)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(largef_all, 'Peril', cur_yr)
# output saved as egg_percent_mean.csv

### total stock health table -----------------------
total_health('Peril', cur_yr)
# works as long as all files are saved in folder with area name

#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### !!! 
# Run CSA model - either excel or here --
# Put Biomass estimates for this area in 'data/biomass.csv'. this contains this years estimates.

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year, Location) %>%  select(Year, Location, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(sum) -> raw_samp

dat5 %>% 
  group_by(Year) %>% 
  summarise (effective_no_pots=n()) %>% 
  right_join(raw_samp) %>% 
  as.data.frame() -> raw_samp

write.csv(raw_samp, paste0('./results/rkc/', survey.location, '/', cur_yr, '/raw_sample.csv'))

### stock assessment figures --------------
head(CPUE_ALL_YEARS)
CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = weighted.mean(Juvenile, weighting), Juv_SE = (weighted.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = weighted.mean(Large.Females, weighting), MatF_SE = (weighted.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all  
CPUE_wt_all %>% filter(Year >= 1993) -> CPUE_wt_from93

write.csv(CPUE_wt_from93, paste0('results/rkc/', survey.location, '/', 
                                 cur_yr, '/cpue_wt_since_93.csv'), row.names = FALSE)

write.csv(CPUE_wt_all, paste0('results/rkc/', survey.location, '/', 
                              cur_yr, '/cpue_wt_all_yrs.csv'), row.names = FALSE)

#panel_figure('Peril', 2019, 'Deadman Reach')
### important to make sure biomass.csv is updated

panel_figure('Peril', cur_yr, 'Deadman Reach', 1, 0) # panel with all 3 figures
panel_figure('Peril', cur_yr, 'Deadman Reach', 2, 0) # male panel
panel_figure('Peril', cur_yr, 'Deadman Reach', 3, 0) # female panel

# panel_figure <- function(survey.location, cur_yr, base.location)
# base.location is the location name in the baseline file, can be different

### NON CONF panel --------------
panel_figure_NC('Peril', 2019, 'Deadman Reach', 1, 0) # panel with all 3 figures
panel_figure_NC('Peril', cur_yr, 'Deadman Reach', 2, 0)

### presentation figure -----
panel_figure_NC_PRES('Peril', cur_yr, 'Deadman Reach', 2, 0, 'Peril Strait')
panel_figure_NC_PRES('Peril', cur_yr, 'Deadman Reach', 3, 0, 'Peril Strait')

### female file all years -----
# create females file for all years
# raw_data has OceanAK output until 2016. 

levels(raw_data$Pot.Condition)
raw_data %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> raw_dat1
raw_dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> all_LgF_dat1
all_LgF_dat1[is.na(all_LgF_dat1$Egg.Percent),]
all_LgF_dat1 %>% 
  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_all

LgF_dat1 %>% 
  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_last2

largef_all <- rbind(LgF_dat1_all, LgF_dat1_last2) # raw female data for all years.
write.csv(largef_all, (paste0('./results/rkc/', survey.location, '/', cur_yr, '/', 
                              'largef_all.csv')))











