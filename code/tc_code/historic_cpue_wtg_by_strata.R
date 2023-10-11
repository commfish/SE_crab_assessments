# Process Tanner crab survey historic data - pre stratification - to use with current data - 2013 +
#       Strata were assigned to these pots after the fact to allow for a calculation of weighted stratified CPUE
#         that could be used to compare CPUEs over the entire time series. Current processing code 
#         'TCS_processingCODE.R' includes 2013 to current year.  This file will be added to the current years output

# K.Palof
# 11-5-18

# Load -------
source('./code/tanner_functions.R')


# Data ----
hbdat <- read_excel('./data/TCS/historic_data_with_new_strata/HB_79_12_ONLY TCS_raw_W strata_USE.xlsx',
                    sheet = "raw data all years")
glbdat <- read_excel('./data/TCS/historic_data_with_new_strata/99 to 13_GLB_TCS_raw bio data_with strata.xlsx',
                     sheet = "99 to 13_GLB")
tbdat <- read_excel('./data/TCS/historic_data_with_new_strata/01_12_Thomas_TCS_raw.xlsx',
                    sheet = "01_13_TB")
isdat <- read_excel('./data/TCS/historic_data_with_new_strata/97_12_icy_tcs_raw with strata.xlsx',
                    sheet = "97_13_icy_tcs")
area <- read.csv("./data/TCS/TCSstrata_area.csv")
# clean up -------
hbdat %>% 
  mutate(Location = as.factor(LOCATION), 
         Pot.Condition = as.factor(POT_CONDITION)) %>% 
  select(Year = YEAR, Trip.No = TRIP_NO, Location, Pot.No = POT_NO, 
         Pot.Condition, Density.Strata.Code = `Tanner crab_Strata`, 
         Density.Strata = `Tanner crab_DENSITY_STRATA`, Specimen.no = SPECIMEN_NO, 
         Number.Of.Specimens = NUMBER_OF_CRAB, Sex.Code = SEX_CODE, Width.Millimeters = WIDTH_MILLIMETERS, 
         Shell.Condition.Code = SHELL_CONDITION_CODE, Egg.Percent = EGG_PERCENT, 
         Egg.Development.Code = EGG_DEVELOPMENT_CODE, Egg.Condition.Code = EGG_CONDITION_CODE) %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") %>% 
  as.data.frame()-> hb_dat1

glbdat %>% 
  mutate(Location = as.factor(LOCATION), 
         Pot.Condition = as.factor(POT_CONDITION)) %>% 
  select(Year = YEAR, Trip.No = TRIP_NO, Location, Pot.No = POT_NO, 
         Pot.Condition, Density.Strata.Code = DENSITY_STRATA_CODE, 
         Density.Strata = DENSITY_STRATA, Specimen.no = SPECIMEN_NO, 
         Number.Of.Specimens = NUMBER_OF_CRAB, Sex.Code = SEX_CODE, Width.Millimeters = WIDTH_MILLIMETERS, 
         Shell.Condition.Code = SHELL_CONDITION_CODE, Egg.Percent = EGG_PERCENT, 
         Egg.Development.Code = EGG_DEVELOPMENT_CODE, Egg.Condition.Code = EGG_CONDITION_CODE) %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") %>% 
  as.data.frame()-> glb_dat1

tbdat %>% 
  mutate(Location = as.factor(LOCATION), 
         Pot.Condition = as.factor(POT_CONDITION)) %>% 
  select(Year = YEAR, Trip.No = TRIP_NO, Location, Pot.No = POT_NO, 
         Pot.Condition, Density.Strata.Code = DENSITY_STRATA_CODE, 
         Density.Strata = DENSITY_STRATA, Specimen.no = SPECIMEN_NO, 
         Number.Of.Specimens = NUMBER_OF_CRAB, Sex.Code = SEX_CODE, Width.Millimeters = WIDTH_MILLIMETERS, 
         Shell.Condition.Code = SHELL_CONDITION_CODE, Egg.Percent = EGG_PERCENT, 
         Egg.Development.Code = EGG_DEVELOPMENT_CODE, Egg.Condition.Code = EGG_CONDITION_CODE) %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") %>% 
  as.data.frame()-> tb_dat1

isdat %>% 
  mutate(Location = as.factor(LOCATION), 
         Pot.Condition = as.factor(POT_CONDITION)) %>% 
  select(Year = YEAR, Trip.No = TRIP_NO, Location, Pot.No = POT_NO, 
         Pot.Condition, Density.Strata.Code = DENSITY_STRATA_CODE, 
         Density.Strata = DENSITY_STRATA, Specimen.no = SPECIMEN_NO, 
         Number.Of.Specimens = NUMBER_OF_CRAB, Sex.Code = SEX_CODE, Width.Millimeters = WIDTH_MILLIMETERS, 
         Shell.Condition.Code = SHELL_CONDITION_CODE, Egg.Percent = EGG_PERCENT, 
         Egg.Development.Code = EGG_DEVELOPMENT_CODE, Egg.Condition.Code = EGG_CONDITION_CODE) %>% 
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") %>% 
  as.data.frame()-> is_dat1

hb_dat1 %>% 
  bind_rows(glb_dat1) %>% 
  bind_rows(tb_dat1) %>% 
  bind_rows(is_dat1) %>% 
  mutate(Location = as.factor(Location)) -> dat1

##### Tanner specific manipulations -----------------------------
###     Survey areas ONLY 
#   confirm that only the four surveys areas are present.
levels(dat1$Location) # 

### add columns used later 
dat1 %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', 
                              ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
                                       !is.na(Width.Millimeters), 'Juvenile', 
                                     ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
                                              !is.na(Width.Millimeters),'Pre_Recruit', 
                                            ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                                                     !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                                                   ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                                                            Width.Millimeters >169|(Shell.Condition.Code >3 & 
                                                                                      Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                                                          ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                                                                 ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                                                                        ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) -> Tdat1

# confirm this worked
Tdat1 %>% 
  filter(mod_recruit == "Missing")

##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class USE mod_recruit here.
Tdat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code, mod_recruit) %>% 
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No + Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  #select( -`NA`) %>% #remove NA column.  This is due to some data errors that need to be fixed in the entry
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

##### Weighted CPUE current year -----------------------------------
# the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area_km) ->dat5

#check to make sure there aren't crab without a assigned recruit class. 
dat5 %>%
  filter(No_crab > 0)

#This version is ready to calculate CPUE for each recruit class
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Location, Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females))))),
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females)))))) -> CPUE_wt_all


# check to confirm previous years CPUEs match
write.csv(CPUE_wt_all, paste0('./results/TCS/', cur_yr, '/Historic_CPUE_all.csv'))


##### Females - large or mature females --------------------------
# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

#make sure this does NOT include immature females
# this is egg_development_code == 4
LgF_Tdat1 %>%
  filter(Egg.Development.Code == 4)
##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_Tdat1[is.na(LgF_Tdat1$Egg.Percent),]
# need to change these to 0. 
LgF_Tdat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent) & Egg.Development.Code > 0,
                             0, Egg.Percent)) -> LgF_Tdat1
LgF_Tdat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_Tdat1 # where 1 is yes and 2 is no

LgF_Tdat1 %>%
  filter(!is.na(Less25)) %>% 
  group_by(Year, Location, Pot.No, Less25) %>%
  summarise(no_sum = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Location, Year)%>%
  summarise(Pclutch = mean(var1)*100 , Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, paste0('./results/TCS/', cur_yr, '/historic_TCS_precent_low_clutch.csv'))

##### egg percentage overall -----------------------------------
LgF_Tdat1 %>%
  filter(!is.na(Egg.Percent)) %>% 
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Location, Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch
write.csv(percent_clutch, paste0('./results/TCS/', cur_yr, '/historic_TCS_percent_clutch.csv'))
