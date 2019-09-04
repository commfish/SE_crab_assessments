#K.Palof 
# ADF&G 8-12-16 updated for Gambier Bay  / updated 8-8-17/8-10-18
# code to process data from Ocean AK to use in crab CSA models.  
#  
# Current year: 2018

rm(list = ls())# clear workspace from previous area 
##Load Packages/functions ---------------------------------
source('./code/functions.R')

## setup year --------
cur_yr <- 2018
pr_yr <- cur_yr -1
survey.location <- 'Gambier'
# area is Peril but in data files it's Deadman Reach

#####Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv(paste0('./data/redcrab/', survey.location,'/RKCsurveyCSA_GB_17_18.csv'))
            # this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv(paste0('./data/redcrab/', survey.location, '/Gambier_strata_area.csv')) 
            #this file is the same every year.  Unless the survey methods change
histdat <- read.csv(paste0('./results/redcrab/', survey.location, '/', pr_yr, '/GB_perpot_all_17.csv'))
        ## !!!!  this file will be 'EI_perpot_all_16' and just get updated with current years data.
#females <- read.csv(paste0('./results/redcrab/', survey.location,'/', pr_yr, '/largef_all.csv'))
      # fixed at bottom of code, should work fine for years in future (past 2018)
      ## use this for raw historic female data in 2017, create input file for future
raw_data <- read.csv(paste0('./data/redcrab/', survey.location, 
                            '/RKC survey_historicpots_GB.csv'))

baseline <- read.csv("./data/redcrab/longterm_means.csv")
# update this file after running CSA - 
biomass <- read.csv("./data/redcrab/biomass.csv") 
# file for all locations.  Has legal and mature biomass from CSA, harvest

head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)
# one female here that don't have recruit status due to no lengths

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - RKC soak time should be between 18-24??? double check this

##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2
# keep trip no to merge with historic data.

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No +Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)

head(dat3)# check to make sure things worked.

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata


##### Weighted CPUE current year -----------------------------------
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5
# this is neccessary so that current years file (dat5) matches the historic file names

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt
CPUE_wt
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_wt, paste0('./results/redcrab/', survey.location, '/', cur_yr, '/GB_CPUE_',cur_yr, '.csv'), 
          row.names = FALSE)

# weighted cpue by strata --- just for comparison
dat5 %>%
  group_by(Year, Density.Strata.Code) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/
                                                                          (sqrt(sum(!is.na(Small.Females)))))) 

#### survey mid date -----
head(dat)
unique(dat$Time.Hauled)
# need to seperate time hauled to just have data hauled look for mid-date 
dat %>% filter(Year == cur_yr)  # 7-27



##### Historic file ---------------------------------------
#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match

head(histdat) # see if any columns don't match those in dat5 - why doesn't historic have npots?
# new historic data has density strata as "Strata.Code"

head(dat5)

histdat %>% select(Year, Location, Trip.No, Pot.No, Strata.Code, Missing, 
                   Juvenile, Large.Females, Post_Recruit, Pre_Recruit, 
                   Recruit, Small.Females, Area, npots, inverse_n, 
                   weighting) -> historicdata
dat5 %>% rename(Strata.Code = Density.Strata.Code) -> dat6

# need to add current year to historicdata file
# only current years
dat6 %>%
  filter(Year == cur_yr) -> dat5_cur_yr
CPUE_ALL_YEARS <- rbind(historicdata, dat5_cur_yr)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, paste0('./results/redcrab/', survey.location, '/', 
                                 cur_yr, '/GB_perpot_all_', cur_yr,'.csv'), 
          row.names = FALSE)


##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years
CPUE_ALL_YEARS %>%
  filter(Year >= cur_yr - 3) -> bypot_st # short term file has last 4 years in it

#function creates output file in folder /results/redcrab/'area'
short_t(bypot_st, cur_yr, "Gambier")

# output is saved as shortterm.csv
bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, factor_key = TRUE) 
ggplot(bypot_st_long, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)

bypot_st %>% 
  select(Year, Pot.No, Strata.Code, Pre_Recruit, Recruit, Post_Recruit) %>% 
  gather(recruit.status, crab, Pre_Recruit:Post_Recruit, factor_key = TRUE) %>% 
  ggplot(aes(Year, crab)) +geom_point() + facet_wrap(~recruit.status)

### short term plots----------------
plot(BYPOT_ST$Year, BYPOT_ST$Juvenile)
Juv_fit <-lm(Juvenile ~ Year, data = BYPOT_ST, weights = weighting)
abline(Juv_fit, col= 'red')
summary(Juv_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Large.Females)
Lfem_fit <-lm(Large.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(Lfem_fit, col= 'red')
summary(Lfem_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Post_Recruit)
PR_fit <-lm(Post_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PR_fit, col= 'red')
summary(PR_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Pre_Recruit)
PreR_fit <-lm(Pre_Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(PreR_fit, col= 'red')
summary(PreR_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Recruit)
R_fit <-lm(Recruit ~ Year, data = BYPOT_ST, weights = weighting)
abline(R_fit, col= 'red')
summary(R_fit)

plot(BYPOT_ST$Year, BYPOT_ST$Small.Females)
smF_fit <-lm(Small.Females ~ Year, data = BYPOT_ST, weights = weighting)
abline(smF_fit, col= 'red')
summary(smF_fit)

##### Long term trends ---------------------
#compare current year CPUE distribution to the long term mean
dat5_cur_yr
#make sure you have a file with only current years data - created above

long_t(dat5_cur_yr, baseline, cur_yr, 'Gambier', 'Gambier')
# output is saved as longterm.csv

##### Weights from length - weight relatinship.-----------------
    # Linear model is changed for each area
    # Gambier linear model: exp(2.921*log(length in mm)-6.695)*2.2/1000
glimpse(dat1) # raw data for both 2016 and 2017
    # slope = 2.921
    # intercept = 6.695
    # use function found in functions.R code file
weights(dat1, 2.921, 6.695, "Gambier", cur_yr)
# output saved as maleweights.csv

##### Females - large or mature females --------------------------
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
# need to change these to 0 if just juvenile
#LgF_dat1 %>%
#  filter(!is.na(Egg.Percent)) -> LgF_dat1
LgF_dat1 %>% 
  filter(Year == cur_yr) %>% 
  select(Year, Project.Code, Trip.No, Location, Pot.No, Number.Of.Specimens, 
         Recruit.Status, Sex.Code, Length.Millimeters, Egg.Percent, 
         Egg.Development.Code, Egg.Condition.Code)-> LgF_dat1_curyr

#largef_all <- rbind(females, LgF_dat1_curyr) # raw female data for all years.
# use this next year, 2018 file created below to bring in historic years

##### % poor (<10 %) clutch -----------------------------------

poor_clutch(largef_all, 'Gambier', cur_yr)
# output is saved as poorclutch_current.csv - which has all pots current year
#     and poorclutch_17.csv which has the percentage and SD of poor clutches for current year 

##### Long term females -------------------------
poorclutch_current <- read.csv(paste0('./results/redcrab/', survey.location, '/', cur_yr,
                                      '/poorclutch1_current.csv'))
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Gambier', cur_yr)
# output saved as lt_female.csv

##### Short term females ------------------------
#look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years
poorclutch_all <- read.csv(paste0('./results/redcrab/', survey.location, '/', cur_yr,
                                  '/poorclutch_all.csv'))

#function for short term trends and output saving.
poor_clutch_short(poorclutch_all, 'Gambier', cur_yr)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(largef_all, 'Gambier', cur_yr)
# output saved as egg_percent_mean.csv

### total stock health table -----------------------
total_health('Gambier', cur_yr)
# works as long as all files are saved in folder with area name

#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

### raw sample size -----------
head(dat5)
dat5 %>% group_by(Year, Location) %>%  select(Year, Location, Juvenile, Small.Females, 
                                              Large.Females, Pre_Recruit, Recruit,Post_Recruit) %>% 
  summarise_all(funs(sum)) -> raw_samp

dat5 %>% 
  group_by(Year) %>% 
  summarise (effective_no_pots=n()) %>% 
  right_join(raw_samp) %>% 
  as.data.frame() -> raw_samp

write.csv(raw_samp, paste0('./results/redcrab/', survey.location, '/', cur_yr, '/raw_sample.csv'))


### stock assessment figures --------------
head(CPUE_ALL_YEARS)
CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt_all
CPUE_wt_all  
CPUE_wt_all %>% filter(Year >= 1993) -> CPUE_wt_from93

write.csv(CPUE_wt_from93, paste0('results/redcrab/', survey.location, '/', 
                                 cur_yr, '/cpue_wt_all_yrs.csv'), row.names = FALSE)


panel_figure('Gambier', 2018, 'Gambier', 1) # panel with all 3 figures
panel_figure('Gambier', 2018, 'Gambier', 2) # male panel
panel_figure('Gambier', 2018, 'Gambier', 3) # female panel

# panel_figure <- function(survey.location, cur_yr, base.location)
# base.location is the location name in the baseline file, can be different

### presentation figure -----
panel_figure_NC_PRES('Gambier', 2018, 'Gambier', 2, 0)
panel_figure_NC_PRES('Gambier', 2018, 'Gambier', 3, 0)

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
write.csv(largef_all, (paste0('./results/redcrab/', survey.location, '/', cur_yr, '/', 
                              'largef_all.csv')))






