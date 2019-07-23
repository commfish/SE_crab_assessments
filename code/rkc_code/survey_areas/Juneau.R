# K.Palof    katie.palof@alaska.gov
# ADF&G 7-3-19/ updated for JUNEAU area

# R script contains code to process data from Ocean AK to use in crab CSA models, code to run CSA model, and calls to create 
#     output and figures for annual stock health report.


# Read me:
#     update code with date updated (top), change global year, and pull new survey data (see below)


## load -------------------------
source('./code/functions.R')

## setup global ---------------
cur_yr <- 2019
pr_yr <- cur_yr -1
survey.location <- 'Juneau'

## data -------------------
dat <- read.csv("./data/rkc/Juneau/jnu_18_19_oceanAK_out_RAW.csv") # file name will change annually
# this is input from OceanAK - set up as red crab survey data for CSA
#   survey area should match that in the name of this script file
#   Juneau area includes Juneau and Barlow
area <- read.csv("./data/rkc/Juneau/Juneau_Barlow_strata_area.csv") # same every year

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

# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1
dat1 %>%
  filter(Recruit.Status == "", Length.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1)

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
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.
tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area) ->dat5
dat5 %>%
  dplyr::rename(Missing = Var.6, Large.Females = `Large Females`, Small.Females = `Small Females`) -> dat5

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/
                                                                          (sqrt(sum(!is.na(Small.Females)))))) -> CPUE_wt

write.csv(CPUE_wt, paste0('./results/rkc/', survey.location,'/', 
                          cur_yr, '/JNU_CPUE_' , cur_yr, '.csv'), row.names = FALSE)

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
# look at results to see the spread between stratas...in high biomass years even low strata 1,2 had higher CPUE. >1 or 2







