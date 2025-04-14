# K.Palof    katie.palof@alaska.gov
# recent date updated: 7-14-2020/ 6-8-2021
# ADF&G updated for JUNEAU area
##AR update: 7/10/24 - for 23-24 rkc stock assessment

# R script contains code to process data from Ocean AK to use in crab CSA models, code to run CSA model, and calls to create 
#     output and figures for annual stock health report.


# Read me:
#     update code with date updated (top), change global year, and pull new survey data (see below)


## load -------------------------
source('./code/functions.R')

## setup global ---------------
cur_yr <- 2024 # change this upon receiving new data
pr_yr <- cur_yr -1
survey.location <- 'Juneau'

dir.create(file.path(paste0('results/rkc/', survey.location), cur_yr))
dir.create(file.path(paste0('text'), cur_yr))
dir.create(file.path(paste0('figures/rkc/'), cur_yr))

## data -------------------
dat <- read.csv(paste0(here::here(), "/data/rkc/Juneau/RKC_survey_CSA_Juneau_23_24.csv")) # file name will change annually
# this is input from OceanAK - set up as red crab survey data for CSA
#   survey area should match that in the name of this script file
#   Juneau area includes Juneau and Barlow
area <- read.csv(paste0(here::here(), "/data/rkc/Juneau/Juneau_Barlow_strata_area.csv")) # same every year

histdat <- read.csv(paste0('./results/rkc/', survey.location, 
                           '/', pr_yr, '/JNU_perpot_all_', pr_yr,'.csv'))
## !!!!  this file will be 'JNU_perpot_all_pr_yr' and just get updated with current years data.
females <- read.csv(paste0('./results/rkc/', survey.location, 
                           '/', pr_yr, '/largef_all.csv'))

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
levels(dat$Pot.Condition)

dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) 
# this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1, Year == cur_yr) -> temp

# write out csv of rows with missing recruit status, if they exist
#write_csv(temp, paste0('./data/rkc/', survey.location, '/JNU_missing_recruit_status' , cur_yr, '.csv'))

# **FIX ** issues with recruit class 2021 pot# 191
#write.csv(temp, paste0('./results/rkc/', survey.location,'/', 
                          #cur_yr, '/data_issues' , cur_yr, '.csv'), row.names = FALSE)

# Calculate soak time - RKC soak time should be 18-24 hrs
dat_soak <- dat1 %>%
  mutate(time_set = as.POSIXct(Time.Set,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())) %>%
  mutate(time_hauled = as.POSIXct(Time.Hauled,format="%Y-%m-%d %H:%M:%S",tz=Sys.timezone())) %>%
  mutate(soak_time = time_hauled - time_set) %>%
  filter(soak_time > 24 | soak_time < 18)

## juvenile molt ?-------------
head(dat1)

dat1 %>% 
  filter(Year == pr_yr, Recruit.Status == "Juvenile", Length.Millimeters >= 40) %>% 
  mutate(cohort = ifelse(Length.Millimeters >=113, "A", 
                         ifelse(Length.Millimeters < 113 & Length.Millimeters >= 97, "B", "C")))-> juvies

# juneau pot 25, specimen #4 length is recorded as 16 ?? is this correct
ggplot(juvies, aes(Length.Millimeters)) +
  geom_histogram(binwidth = .5)

# growth increment of 16mm
# 129, 113, 97
ggplot(juvies, aes(x = Length.Millimeters, fill = cohort)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(juvies, aes(x = Length.Millimeters, fill = cohort)) + geom_density(alpha=.3)

dat1 %>% 
  filter(Year == cur_yr, Recruit.Status == "Juvenile", Length.Millimeters >= 40) %>% 
  mutate(cohort = ifelse(Length.Millimeters >=113, "A", 
                         ifelse(Length.Millimeters < 113 & Length.Millimeters >= 97, "B", "C")))-> juvies20

ggplot(juvies20, aes(Length.Millimeters)) +
  geom_histogram(binwidth = .5)

ggplot(juvies20, aes(x = Length.Millimeters, fill = cohort)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

#ar- graph of lengths -exploratory
ggplot(dat1) + aes(x=Length.Millimeters) + geom_density()
ggplot(dat1) + aes(x=Length.Millimeters, color=factor(Year), fill=factor(Year), group = factor(Year)) + geom_density(alpha=0.3)+
  facet_wrap(~Recruit.Status) + scale_color_manual(values=c("lightgreen", "violet"))+
  scale_fill_manual(values=c("lightgreen", "violet"))

ggplot(dat1) + aes(x=Length.Millimeters, color=factor(Year), fill=factor(Year), group = factor(Year)) + geom_density(alpha=0.3)+
  facet_wrap(~Recruit.Status) + scale_color_manual(values=c("lightgreen", "violet"))+
  scale_fill_manual(values=c("lightgreen", "violet"))

ggplot(dat1) + aes(x=Length.Millimeters, color=factor(Year), fill=factor(Year), group = factor(Year)) + geom_histogram(alpha=0.3, position="dodge")+
  facet_wrap(~Recruit.Status) + scale_color_manual(values=c("lightgreen", "violet"))+
  scale_fill_manual(values=c("lightgreen", "violet"))

ggplot(dat1) + aes(x=Length.Millimeters, color=Recruit.Status, fill=Recruit.Status, group = Recruit.Status) + geom_histogram(alpha=0.3, position="dodge")+
  facet_wrap(~factor(Year)) #+ scale_color_manual(values=c("lightgreen", "violet"))+
  #scale_fill_manual(values=c("lightgreen", "violet"))

## CPUE calc --------------
##### By Pot -------------------------------
# Now summarize by pot - remember to keep areas separate.
# need Number of Specimens by recruit class
# keep trip no. to merge with historic data 
dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Density.Strata.Code, Recruit.Status) %>%
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + Location + Trip.No + Pot.No + Density.Strata.Code ~ Recruit.Status, sum, drop=TRUE)
head(dat3) # confirm data is summarized by pot with recruit classes each in a column 

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

#####Weighted CPUE current year -----------------------------------
# the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.
tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) -> dat5

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
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/
                                                                          (sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt

write.csv(CPUE_wt, paste0('./results/rkc/', survey.location,'/', 
                          cur_yr, '/JNU_CPUE_' , cur_yr, '.csv'), row.names = FALSE)

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
# look at results to see the spread between stratas...in high biomass years even low strata 1,2 had higher CPUE. >1 or 2

#### survey mid date -----  

# list of unique dates (day only, excluding time)
dates <- unique(round_date(ymd_hms(dat$Time.Hauled), unit="day"))

# only survey dates from the current year
dates.cur <- dates[dates > as.Date(paste0(year(as.Date(as.character(pr_yr), format = "%Y")),"-12-31"))]

# interval of minimum and maximum survey dates
date.int <- interval(min(dates.cur, na.rm=TRUE), max(dates.cur, na.rm=TRUE))

# survey midpoint; see functions script for the int_midpoint function
sur.midpoint <- int_midpoint(date.int)

# convert to Julian day
sur.midpoint.jul <- yday(sur.midpoint)

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
  dplyr::rename(Strata.Code = Density.Strata.Code) -> dat6 # masking???

# need to add current year to historicdata file
# only current years
dat6 %>%
  filter(Year == cur_yr) -> dat5_cur_yr
CPUE_ALL_YEARS <- rbind(historicdata, dat5_cur_yr)
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(CPUE_ALL_YEARS, paste0('./results/rkc/', 
                                 survey.location, '/', cur_yr, '/JNU_perpot_all_', cur_yr,'.csv'), 
                                 row.names = FALSE)

# number of pots per year --
CPUE_ALL_YEARS %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(n = n()) #AR - "by is missing with no default"- added dplyr:: to code

CPUE_ALL_YEARS %>% count(Year)

## Trends - short and long and female stats for stock health weighting ---------------
### Short term trends -------------------
# look at trend for the last 4 years.  Need a file with last four years in to JNU_CPUE_ALL
CPUE_ALL_YEARS %>%
  filter(Year >= cur_yr - 3) -> bypot_st # short term file has last 4 years in it

# function creates output file in folder /results/redcrab/'area'
short_t(bypot_st, cur_yr, "Juneau")
# output is saved as shortterm.csv
bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, 
                        factor_key = TRUE) 
ggplot(bypot_st_long, aes(Year,crab)) +geom_point() +facet_wrap(~recruit.status)


##### Long term trends ---------------------
# compare current year CPUE distribution to the long term mean
head(dat5_cur_yr)
# make sure you have a file with only current years data - created above

long_t(dat5_cur_yr, baseline, cur_yr, 'Juneau', 'Juneau')
# output is saved as longterm.csv


##### Weights from length - weight relationship-----------------
# Linear model is changed for each area
# Juneau linear model: exp(3.03*log(length in mm)-7.23)*2.2/1000
glimpse(dat1) # raw data for last 2 years
# slope = 3.03
# intercept = 7.23
# use function found in functions.R code file
weights(dat1, 3.03, 7.23, "Juneau", cur_yr)
# output saved as maleweights.csv

# weights by stage for GMACS

dat_allyrs <- read.csv(paste0(here::here(), "/data/rkc/Juneau/RKC_survey_CSA_Juneau_all_years.csv")) %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed")

weights_stage <- function(dat, slope, intercept, area, year){
  dat2 <- dat %>%
    mutate(weight_lb = (exp((slope*log(Length.Millimeters))-intercept))*(2.2/1000))
  
  Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
  Legal =c("Recruit", "Post_Recruit")
  # summary of weights all together - would like these in one calc and one summary table
  male_weights <- dat2 %>% 
    group_by(Year) %>% 
    filter(Sex.Code == 1) %>% 
    summarise(prer_lbs = weighted.mean(weight_lb[Recruit.Status == "Pre_Recruit"], 
                                       Number.Of.Specimens[Recruit.Status == "Pre_Recruit"], na.rm = TRUE),
              rec_lbs = weighted.mean(weight_lb[Recruit.Status == "Recruit"], 
                                         Number.Of.Specimens[Recruit.Status == "Recruit"], na.rm = TRUE), 
              postr_lbs = weighted.mean(weight_lb[Recruit.Status == "Post_Recruit"], 
                                        Number.Of.Specimens[Recruit.Status == "Post_Recruit"], na.rm = TRUE)) %>%
    mutate(across(where(is.numeric), round, 3))
  # final results with score - save here
  write_csv(male_weights, paste0('results/rkc/', area, '/', year, '/maleweights_stage.csv'))
}

weights_stage(dat_allyrs, 3.03, 7.23, "Juneau", cur_yr) # warning: depreciated- AR - still works tho?

###### Females ----------------------------------------------------------
# large or mature females
dat1 %>%
  filter(Sex.Code == 2, Recruit.Status == 'Large Females') -> LgF_dat1

# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if development = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_dat1[is.na(LgF_dat1$Egg.Percent),]
# need to change these to 0 if applicable. 
#LgF_dat1 %>%
#  mutate(Egg.Percent =ifelse(is.na(Egg.Percent), 0, Egg.Percent)) -> LgF_dat1
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
poor_clutch(largef_all, 'Juneau', cur_yr)
# output is saved as poorclutch1_current.csv - which has all pots for 2017
# and poorclutch_summary_all.csv which has the percentage and 
#                                          SD of poor clutches for all years

##### Long term females -------------------------
poorclutch_current <- read.csv(paste0('./results/rkc/', survey.location,'/', 
                                      cur_yr, '/poorclutch1_current.csv'))
# bring in output from function above with the current years pots. 
glimpse(poorclutch_current)
# function to compare this to a long term mean of 10% and save for .Rmd output
poor_clutch_long(poorclutch_current, 'Juneau', cur_yr)
# output saved as lt_female.csv

##### Short term females ------------------------
# look at trend for the last 4 years.  Need a file with last four years in it - females from above
# input data the first time (2016) and then add to it.
# save this file here for future years
poorclutch_all <- read.csv(paste0('./results/rkc/', survey.location, '/',
                                  cur_yr,'/poorclutch_all.csv'))
#function for short term trends and output saving.
poor_clutch_short(poorclutch_all, 'Juneau', cur_yr)
# output saved as short_female.csv

##### egg percentage overall -----------------------------------
egg_percent(largef_all, 'Juneau', cur_yr)
# output saved as egg_percent_mean_all.csv, creates mean and SE egg percentage for all years

### total stock health table -----------------------
total_health('Juneau', cur_yr)
# works as long as all files are saved in folder with area name


#### STOP HERE AND run .Rmd file for this area for summary and to confirm things look ok
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## see 'personal_use.R' and 'rkc_harvest_XX.R' to get all data for CSA model

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
CPUE_wt_all %>% filter(Year >= 1995) -> CPUE_wt_from95

write.csv(CPUE_wt_from95, paste0('results/rkc/', survey.location, '/', 
                                 cur_yr, '/cpue_wt_since_95.csv'), row.names = FALSE)

write.csv(CPUE_wt_all, paste0('results/rkc/', survey.location, '/', 
                              cur_yr, '/cpue_wt_all_yrs.csv'), row.names = FALSE)

## baseline calc redo without 93/94 ----
CPUE_wt_from95 %>% 
  filter(Year >=1995 & Year <= 2007) %>% 
  summarise_all(mean, na.rm = TRUE)


panel_figure('Juneau', cur_yr, 'Juneau', 1, 0) # panel with all 3 figures
panel_figure('Juneau', cur_yr, 'Juneau', 2, 0) # male panel
panel_figure('Juneau', cur_yr, 'Juneau', 3, 0) # female panel

# panel_figure <- function(survey.location, cur_yr, base.location)
# base.location is the location name in the baseline file, can be different

### see 'juneau_hindcast_figure.R' file for other figures for the Juneau document

### presentation figure -----
panel_figure_NC_PRES('Juneau', cur_yr, 'Juneau', 2, 0, 'Juneau Area')
panel_figure_NC_PRES('Juneau', cur_yr, 'Juneau', 3, 0, 'Juneau Area')

### presentation figures with different titles -----
panel_figure_NC_PRES_title('Juneau', cur_yr, 'Juneau', 2, 0, "Males", "Females and juveniles")
panel_figure_NC_PRES_title('Juneau', cur_yr, 'Juneau', 3, 0, "Males", "Females and juveniles")


### total mature male CPUE and CV -----

mat_male_cpue <- CPUE_ALL_YEARS %>%
  ungroup() %>%
  rowwise() %>%
  mutate(mat_males = sum(Pre_Recruit, Recruit, Post_Recruit)) %>%
  group_by(Year) %>%
  summarise(mat_males_wt = weighted.mean(mat_males, weighting), mat_males_SE = (weighted.sd(mat_males, weighting)/(sqrt(sum(!is.na(mat_males))))), weighted.cv = (weighted.sd(mat_males, weighting)/weighted.mean(mat_males, weighting))/10) 

# format for GMACS
mat_male_cpue1 <- mat_male_cpue %>%
  mutate(Index = 1, Seas = 1, Fleet = 2, Sex = 1, Maturity = 0, Abundance = round(mat_males_wt*1000, 3), CV = round(weighted.cv, 4), abundance_units = 2, timing = 0) %>%
  select(Index, Year, Seas, Fleet, Sex, Maturity, Abundance, CV, abundance_units, timing)

# write out data file

write.csv(mat_male_cpue1, file = paste0(here::here(), "/data/rkc/Juneau/RKC_matmale_cpue.csv"), row.names=FALSE)

write.table(mat_male_cpue1, file = paste0(here::here(), "/data/rkc/Juneau/RKC_matmale_cpue.txt"), sep = "\t",
            row.names = FALSE, quote = FALSE)




# CPUE by stage

CPUE_wt_gmacs <- CPUE_ALL_YEARS %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), PreR_cv = (weighted.sd(Pre_Recruit, weighting)/weighted.mean(Pre_Recruit, weighting))/10,
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), Recruit_cv = (weighted.sd(Recruit, weighting)/weighted.mean(Recruit, weighting))/10,
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PostR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))), PostR_cv = (weighted.sd(Post_Recruit, weighting)/weighted.mean(Post_Recruit, weighting))/10)


# format for GMACS - does not work - AR 7/12
#male_cpue_gmacs <- CPUE_wt_gmacs %>%
 # unite(col="DataVec", c(Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt), sep="\t") %>%
#  dplyr::mutate(Index = 1, Seas = 1, Fleet = 1, Sex = 1, Type = 0, Shell = 0, Maturity = 0, Nsamp = total.crab) %>% #added the dyplr spec - AR
#  select(Index, Year, Seas, Fleet, Sex, Maturity, Abundance, CV, abundance_units, DataVec)

# write out data file

#write.csv(male_cpue_gmacs, file = paste0(here::here(), "/data/rkc/Juneau/RKC_male_cpue.csv"), row.names=FALSE)

#write.table(male_cpue_gmacs, file = paste0(here::here(), "/data/rkc/Juneau/RKC_male_cpue.txt"), sep = "\t",
        #    row.names = FALSE, quote = FALSE)

#caitlin made figure 2- added 7/18/24

# create model fit plot ---

# note: each year, add one row to the import ranges (e.g., if in 2023 ranges are A8:F53 and R8:T53, then in 2024 ranges are A8:F54 and R8:T54)

library(readxl)

cpue_fit <- read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Estimates 3 Stage", range = "A8:F54") %>%
  cbind(read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Estimates 3 Stage", range = "R8:T54")) %>%
  select(-c(`...2`, `...3`)) %>%
  dplyr::rename(Year = `...1`, Obs_prerecruits = `...4`, Obs_recruits = `...5`, Obs_postrecruits = `...6`, Est_prerecruits = Prerecruits, Est_recruits = Recruits, Est_postrecruits = Postrecruits) %>%
  mutate(across(c(Obs_prerecruits, Obs_recruits, Obs_postrecruits, Est_prerecruits, Est_recruits, Est_postrecruits), as.numeric)) %>% #added step so things to explode- ar
  pivot_longer(cols = c(Obs_prerecruits, Obs_recruits, Obs_postrecruits, Est_prerecruits, Est_recruits, Est_postrecruits), values_to = "survey_index") %>%
  mutate(type = case_when(
    grepl("Obs", name) ~ "Observed",
    grepl("Est", name) ~ "Estimated"
  )) %>%
  mutate(stage = case_when(
    name == "Obs_prerecruits" ~ "Pre-recruits",
    name == "Obs_recruits" ~ "Recruits",
    name == "Obs_postrecruits" ~ "Post-recruits",
    name == "Est_prerecruits" ~ "Pre-recruits",
    name == "Est_recruits" ~ "Recruits",
    name == "Est_postrecruits" ~ "Post-recruits"
  )) %>%
  mutate(stage = factor(stage, levels = c("Pre-recruits", "Recruits", "Post-recruits")))

cpue_fit_plot <- ggplot(cpue_fit, aes(x = Year, y = survey_index, group = stage)) +
  geom_point(data = subset(cpue_fit, type == "Observed")) +
  geom_line(data = subset(cpue_fit, type == "Estimated"), color = "blue") + 
  #facet_grid(. ~ stage)
  facet_wrap(vars(stage)) + #ncol=1 to make it long form
  theme_bw() +
  ylab("CPUE")

ggsave(filename = paste0(here::here(), '/figures/rkc/', cur_yr, '/', 
                         'Juneau_cpue_model_fit.png'), plot = cpue_fit_plot, height = 4, width = 6.5, units = "in") #ar- I switched the width and height


#catlin additional fig 7/23/24- might be incorrect, oops
biom_change_closure <- read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Table 2", range = "A5:A50") %>%
  cbind(read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Table 2", range = "F5:F50")) %>%
  cbind(read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Table 2", range = "J5:J50")) %>%
  cbind(read_excel(paste0(here::here(), "/CSA excel/Juneau ", cur_yr, " new weighting.xls"), sheet = "Table 2", range = "N5:N50")) %>%
  dplyr::rename("Year" = "...1") %>%
  dplyr::rename("Status" = "...1") %>%
  filter(Year > 2004) %>%
  mutate("Est. harvest rate in previous year" = lag(`Harvest Rate`)) %>%
  mutate("Fishery status in previous year" = lag(`Status`))

biom_change_plot <- biom_change_closure %>% filter(Year > 2005)

ggplot(biom_change_plot, aes(x = `Est. harvest rate in previous year`, y = `pop change`, color = `Fishery status in previous year`)) +
  geom_point() + 
  geom_hline(yintercept=0, linetype="dashed", 
             color = "grey", size=1) +
  ylab("Est. % change in mature population") + 
  geom_text(aes(label = Year), hjust = -0.25,  vjust = 0.5)

#################
#Adam's requested graph of just the top panel from Juneau figure 2-specific to 2024

CPUE_wt_graph <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                 '/cpue_wt_since_95.csv'))
mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv')
baseline <- read.csv("./data/rkc/longterm_means.csv")
biomass <- read.csv("./data/rkc/biomass.csv") 
CPUE_wt_graph %>% 
  select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
         PreR_SE, Rec_SE, PR_SE) -> males
males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
males_long %>% 
  mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                "pre.recruit", ifelse(recruit.status == "Recruit_wt", 
                                                      "recruit", ifelse(recruit.status == "PreR_SE", 
                                                                        "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                                                              "recruit", "post.recruit "))))) %>% 
  mutate(type = ifelse(recruit.status == "PreR_SE",
                       "se", 
                       ifelse(recruit.status == "Rec_SE", 
                              "se", ifelse(recruit.status == "PR_SE", 
                                           "se", "mean"))))-> males_long
males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph

# baseline cpue values -----
baseline %>% 
  filter(Location == "Juneau") -> baseline2


p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
  geom_point(aes(colour = recruit.class, shape = recruit.class, 
                 fill = recruit.class), size =3) +
  geom_line(aes(group = recruit.class, colour = recruit.class))+
  #scale_colour_manual(name = "", values = c("grey1", "grey65", "grey34"))+
  #scale_fill_manual(name = "", values = c("grey1", "grey65", "grey34")) +
  scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
  scale_shape_manual(name = "", values = c(15, 16, 17))+
  scale_y_continuous(breaks = seq(min(0),max((max(males_graph$mean) + max(males_graph$se))), by = 1)) + # change to have more tick marks
  #scale_y_continuous(limits = c(0,(max(males_graph$mean) + max(males_graph$se))),
  #                   oob = rescale_none) +
  #ylim(0,(max(males_graph$mean) + max(males_graph$se))) + 
  ggtitle(survey.location) + ylab("CPUE (number/pot)")+ xlab(NULL)+
  theme(plot.title = element_text(hjust =0.5)) + #got rid of: axis.text.x = element_blank(),
  scale_x_continuous(breaks = seq(min(1996),max(cur_yr), by =2)) + #changed from min(1995) so my graphs will end at 2024 - ar
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
              alpha = 0.2) +
  #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
  #              width =.4) +
  geom_hline(yintercept = baseline2$Pre_Recruit, color = "#E69F00", 
             linetype = "dotdash", lwd = 0.75)+
  geom_hline(yintercept = baseline2$Recruit, color = "#56B4E9", 
             linetype = "longdash", lwd = 0.75)+
  geom_hline(yintercept = baseline2$Post_Recruit, color = "#999999", 
             lwd = 0.75)+
  theme(legend.position = c(0.5,0.8), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size = 24))

#save the requested plot
ggsave(paste0('./figures/rkc/',cur_yr, '/', survey.location, '_', cur_yr, '_', 
              'justJuneauCPUE', '.png'), p1,  #save teh plot
       dpi = 800, width = 8, height = 5)


