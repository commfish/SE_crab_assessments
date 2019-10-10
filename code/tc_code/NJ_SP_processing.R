#K.Palof 
# ADF&G 11-15-16 / 10-11-17 / 11-2-18
# Areas: tanner crab assessment of red crab areas : North Juneau and Stephens Passage
#   done seperately because they need to be divided into these two areas based on the pot locations, 
#   division is done by Kellii Wood - she places SP pots in their correct strata and the rest of the pots 
#   for that year are used for NJ
# process current year's data first and then add to older data already stored. 

# code to process data from Ocean AK to use in crab CSA models.  
# This was done in excel then JMP, prior to 2016  

#####Load -------------
#source('./code/tanner_functions.R')
source('./code/sp_nj_figures.R')
cur_yr <- 2018

#####Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/nj_stp/Juneau_red crab survey for Tanner crab CSA_2018.csv")
# all current year pots from Juneau and Barlow Cove
# this is input from OceanAK - set up as red crab survey data for CSA
area <- read.csv("./data/nj_stp/stp_strata_area.csv") 
#     density strata for tanner stratification in Stephen's Passage area
seperate <- read.csv("./data/nj_stp/2018_sp_pots.csv") 
#     from Kellii using GIS, puts Juneau area pots into density strata
baseline <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")

SP_hist <- read.csv(paste0('./results/nj_stp/', cur_yr-1, '/SP_rawdata_all.csv'))
NJ_hist <- read.csv(paste0('./results/nj_stp/', cur_yr-1, '/NJ_rawdata_all.csv'))
# bring in historic data for each area below.

#females <- read.csv("./data/Juneau/RKC_11_16_large females_by_pot.csv")
head(dat)
glimpse(dat) # confirm that data was read in correctly.

### prep pots to give to Kellii----------
dat %>% select(Year, Location.Code, Location, Pot.No, Depth.Fathoms, Latitude.Decimal.Degrees, 
               Longitude.Decimal.Degrees) %>% 
  group_by(Year, Location, Pot.No) %>% 
  summarise(Depth.Fathoms = mean(Depth.Fathoms), Latitude.Decimal.Degrees = mean(Latitude.Decimal.Degrees), 
            Longitude.Decimal.Degrees = mean(Longitude.Decimal.Degrees)) -> juneau_pot_info
write.csv(juneau_pot_info, paste0('./data/nj_stp/juneau_pot_info_', cur_yr,'.csv'))

##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1)  # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.

# also need to check soak time and to make sure all crab that were measured have a recruit status
#come back later and add a soak time column - tanner soak time should be between 16-20??? double check this

##### seperate NJ and Juneau (also known as SP) ---------------------
dat1 %>%
  mutate(area = ifelse(Location == "Barlow Cove", "NJ", 
                       ifelse(Location == "Juneau" & Pot.No %in% seperate$Pot_No, "Juneau", "NJ"))) ->dat1
#seperating the areas since North Juneau does not have density strata - since it's a red crab area
# and Juneau does since it's based on the Tanner Stephens Passage strata.
dat1 %>% 
  filter(area == "NJ") ->dat.NJ
dat1 %>% 
  filter(area == "Juneau") -> dat.SP

#### North Juneau ----------------------
###  need to keep barlow (location code 12, 1) and juneau (location code 13, 23) seperate
##### Historic file ---------------------------------------
### these files are read in above
#     need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
#       any issues with recalculating the crab per pot due to edits in data.
#       read in historic by pot file and make sure variable names match
glimpse(NJ_hist) # make sure the column names here match those in dat.NJ
dat.NJ %>% select(- Latitude.Decimal.Degrees, -Longitude.Decimal.Degrees) -> dat.NJ
NJ_hist %>% select ( -X ) -> NJ_hist
data.NJ.all <- rbind(NJ_hist, dat.NJ)
write.csv(data.NJ.all, paste0('./results/nj_stp/', cur_yr,'/NJ_rawdata_all.csv'))

### data manipulations ----------------------
# easier area since there are NO strata
data.NJ.all %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
                  !is.na(Width.Millimeters), 'Juvenile', 
                   ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
                    !is.na(Width.Millimeters),'Pre_Recruit', 
                     ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                      !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                       ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                        Width.Millimeters >169|(Shell.Condition.Code >3 & Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                         ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                          ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                           ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) %>%
  mutate(sub_area = ifelse(Location.Code == 12 | Location.Code == 1, 'Barlow', 'Juneau')) -> Tdat1

###
##### By Pot ----------------------------------------------------
#### Keep sub_area in the data frame!!!!!!!!!!!
#Now summarize by pot - only one area - NJ
Tdat1 %>%
  group_by(Year, area, sub_area, Pot.No, mod_recruit) %>% # use area here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + area + sub_area + Pot.No ~ mod_recruit, sum, drop=TRUE)
head(dat3)# check to make sure things worked.
#write.csv(dat3, './results/nj_stp/dat3.csv')

# No weighting by strata here for RKCS data due to it being designed for RKC.

##### CPUE historic -----------------------------------
#This version is ready to calculate CPUE for each recruit class
#Calculates a  mean CPUE and SE for each recruit class # not weighted due to lack of tanner specific strata on red crab survey
dat3 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_ALL
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_ALL, paste0('./results/nj_stp/', cur_yr, '/NJ_CPUE_ALL.csv'))

##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

# can I use red crab function here?  **FIX**
#source('./code/functions.R')
head(dat3)

dat3 %>%
  filter(Year >= cur_yr-3) -> dat3a # confirm that is only contains the last 4 years.  

dat3_long <- gather(dat3a, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

dat3_long %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(mod_recruit) %>%
  do(fit = lm(crab ~ Year, data =.)) -> short_term

short_term %>%
  tidy(fit) -> short_term_slope

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(mod_recruit, r.squared, p.value)->short_term_out2
short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, paste0('./results/nj_stp/', cur_yr, '/NJ_shortterm.csv'))

dat3_long %>%
  filter(mod_recruit %in% recruit_used) ->st_dat3_long
ggplot(st_dat3_long, aes(Year, crab, color = mod_recruit))+geom_point() 

##### Long term trends ---------------------
#compare current year CPUE distribution to the long term mean
dat3 %>%
  filter(Year == cur_yr) ->dat3_current
#make sure you have a file with only current years data
baseline %>% 
  filter(AREA == 'NJ') -> baseline_NJ
# long term baseline values are different for each area, I guess make a file for each area?
#
# the y = has to be changed for each area but once they are set they are the same from year to year
t.test(dat3_current$Large.Females, mu = baseline_NJ$Large.Female)
t.test(dat3_current$Pre_Recruit, mu = baseline_NJ$Pre_Recruit)
t.test(dat3_current$Recruit, mu = baseline_NJ$Recruit)
t.test(dat3_current$Post_Recruit, mu = baseline_NJ$Post_Recruit)

# **FIX** need to summarize these to save the results - see function for red crab

##### Weights from length - weight relatinship--------------------
# Linear model is changed for each area
# North Juneau linear model: exp(3.16*log(length in mm)-8.84)*2.2/1000
glimpse(Tdat1) # raw data for all years
Tdat1 %>%
  mutate(weight_lb = (exp((3.16*log(Width.Millimeters)) - 8.84 ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")

datWL %>% 
  group_by(Year) %>% 
  filter(Sex.Code == 1) %>% 
  summarise(mature_lbs = wt.mean(weight_lb[mod_recruit %in% Mature], 
                                 Number.Of.Specimens[mod_recruit %in% Mature]), 
            legal_lbs = wt.mean(weight_lb[mod_recruit %in% Legal], 
                                Number.Of.Specimens[mod_recruit %in% Legal]), 
            prer_lbs = wt.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                               Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights

write.csv(male_weights, paste0('./results/nj_stp/', cur_yr, '/NJ_weights.csv'))

##### survey mid-date --------------------
Tdat1 %>%
  filter(Year == cur_yr) %>%
  distinct(Time.Hauled)

##### Females - large or mature females --------------------------
# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

#make sure this does NOT include immature females
# this is egg_condition_code == 4
LgF_Tdat1 %>%
  filter(Egg.Development.Code == 4)
##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
 
LgF_Tdat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent) & Egg.Development.Code > 0,
                             0, Egg.Percent)) -> LgF_Tdat1
LgF_Tdat1 %>% 
  filter(is.na(Egg.Percent))

LgF_Tdat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_Tdat1 # where 1 is yes and 2 is no

LgF_Tdat1 %>%
  filter(!is.na(Less25)) %>% 
  group_by(Year, sub_area, Pot.No, Less25) %>%
  summarise(no_sum = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + sub_area + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Year)%>%
  summarise(Pclutch = mean(var1)*100 , 
            Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, paste0('./results/nj_stp/', cur_yr, '/NJ_precent_low_clutch.csv'))

##### Long term females -------------------------
glimpse(poorclutch1)
#compare current year's CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == cur_yr) ->poorclutch1_current
#make sure you have a file with only current year's data
#calculate the t.test
t.test(poorclutch1_current$var1, mu = 0.10)

##### Short term females ------------------------

#look at trend for the last 4 years.  Need a file with last four years in it 
head(poorclutch1) # should have the last 4 years from OceanAK

poorclutch1 %>%
  filter(Year >= cur_yr-3) -> LgF_short # short term file has last 4 years in it

# need to run the regression for each area.
LgF_short %>% 
  mutate(Location = 'North Juneau')%>%
  group_by(Location) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  tidy(fit) %>% 
  filter(term == "Year") %>% 
  select(Location, estimate) -> one
LgF_short %>% 
  mutate(Location = 'North Juneau')%>%
  group_by(Location) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  glance(fit) %>% select(Location, r.squared, p.value) ->two
one %>%
  right_join(two) -> F_short_term_results # estimate here is slope from regression

#Now need to add column for significance and score
F_short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> F_short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(F_short_term_results, paste0('./results/nj_stp/', cur_yr, '/NJ_Fem_shortterm.csv'))
ggplot(poorclutch1, aes(Year, var1))+geom_point() 

##### egg percentage overall -----------------------------------
LgF_Tdat1 %>%
  filter(!is.na(Egg.Percent)) %>% 
  group_by(Year, sub_area, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Year)%>%
  summarise(mean = mean(egg_mean), 
            egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch
write.csv(percent_clutch, paste0('./results/nj_stp/', cur_yr, '/NJ_percent_clutch.csv'))

#### Stephens Passage  ----------------------
###  All these are Juneau so no sub_area (location code 13, 23) 
## add tanner density strata to dat.SP from seperate file
# in seperate 'classes' is the Tanner.Density.Strata, simplify seperate
seperate %>%
  select(Year, Pot_No, Classes) %>%
  rename(Tanner.Density.Strata.Code = Classes, Pot.No = Pot_No) %>%
  mutate(Density.Strata = ifelse(Tanner.Density.Strata.Code ==1, 'Low/Zero', 
                                 ifelse(Tanner.Density.Strata.Code ==2, 'Medium Low', 
                                  ifelse(Tanner.Density.Strata.Code ==3, 'Medium', 
                                   ifelse(Tanner.Density.Strata.Code ==4, 'Medium High', 
                                          'High'))))) -> strata_codes_SP
dat.SP %>%
  select(-Density.Strata.Code, -Density.Strata) %>%
  right_join(strata_codes_SP) -> dat.SP

##### Historic file ---------------------------------------

#need to add current years CPUE to the historic CPUE file.  For simplicity reasons this will be inputed for each of the bays.  This will avoid
# any issues with recalculating the crab per pot due to edits in data.
# read in historic by pot file and make sure variable names match
glimpse(SP_hist) # make sure the column names here match those in dat.NJ

dat.SP %>% 
  select( -Latitude.Decimal.Degrees, -Longitude.Decimal.Degrees) -> dat.SP
data.SP.all <- rbind(SP_hist, dat.SP)
write.csv(data.SP.all, paste0('./results/nj_stp/', cur_yr,'/SP_rawdata_all.csv', row.names = FALSE))

### data manipulations ----------------------
# easier area since there are NO strata
data.SP.all %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
            !is.na(Width.Millimeters), 'Juvenile', 
             ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
              !is.na(Width.Millimeters),'Pre_Recruit', 
               ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                 ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                  Width.Millimeters >169|(Shell.Condition.Code >3 & Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                   ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                    ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                     ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) -> Tdat1
# write.csv(Tdat1, paste0('./results/nj_stp/', cur_yr, '/Tdat1_SP.csv'))
# add in area and weighting by strata

##### By Pot ----------------------------------------------------
#Now summarize by pot - only one area - Juneau(also known as SP)
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, area, Pot.No, Tanner.Density.Strata.Code,mod_recruit) %>% # use AREA here instead of location due to multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) -> dat2

dat3 <- dcast(dat2, Year + area + Pot.No + Tanner.Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)
# write.csv(dat3, paste0('./results/nj_stp/', cur_yr, '/SP_dat3.csv'))
# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
#Calculates the number of pots per strata.  
tab %>%
  group_by(Year, area, Tanner.Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No)) -> pots_per_strata

##### Weighted CPUE current year -----------------------------------
##
#the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.

tab %>%
  right_join(pots_per_strata) -> dat4

dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area_km) ->dat5

#check to make sure there aren't crab without a assigned recruit class. 
dat5 %>%
  filter(No_crab > 0)


##### CPUE historic -----------------------------------
#Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(area, Year) %>%
  summarise(Pre_Recruit_wt = wt.mean(Pre_Recruit, weighting), PreR_SE = (wt.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = wt.mean(Recruit, weighting), Rec_SE = (wt.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = wt.mean(Post_Recruit, weighting), PR_SE = (wt.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = wt.mean(Juvenile, weighting), Juv_SE = (wt.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            SmallF_wt = wt.mean(Small.Females, weighting), SmallF_SE = (wt.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females))))),
            MatF_wt = wt.mean(Large.Females, weighting), MatF_SE = (wt.sd(Large.Females, weighting)/
                (sqrt(sum(!is.na(Large.Females)))))) -> CPUE_wt_all
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder here.
write.csv(CPUE_wt_all, paste0('./results/nj_stp/', cur_yr, '/SP_CPUE_ALL.csv'))

##### Short term trends -------------------------------------
#look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
#tidy(Lfem_fit) # want to save $estimate here
#glance(Lfem_fit) # want to save r.squared and p.value

head(dat3)
dat3 %>%
  filter(Year >= cur_yr-3) -> dat3 
# confirm that is only contains the last 4 years.  This year needs to be changed every year

dat3_long <- gather(dat3, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

dat3_long %>% # doesn't work with dat2 data because there are no 0's for missing data
  group_by(mod_recruit) %>%
  do(fit = lm(crab ~ Year, data =.)) -> short_term

short_term %>%
  tidy(fit) -> short_term_slope

short_term %>%
  glance(fit) ->short_term_out

recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
short_term_out %>%
  filter(mod_recruit %in% recruit_used) %>%
  select(mod_recruit, r.squared, p.value)->short_term_out2
short_term_slope %>%
  filter(mod_recruit %in% recruit_used, term == 'Year') %>%
  select(mod_recruit, estimate) %>%
  right_join(short_term_out2)->short_term_results # estimate here is slope from regression
#Now need to add column for significance and score
short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(short_term_results, paste0('./results/nj_stp/', cur_yr, '/SP_shortterm.csv'))

dat3_long %>%
  filter(mod_recruit %in% recruit_used) ->st_dat3_long
ggplot(st_dat3_long, aes(Year, crab))+geom_point() +facet_wrap (~ mod_recruit)

##### Long term trends ---------------------
#compare current year CPUE distribution to the long term mean
dat5 %>%
  filter(Year == cur_yr) ->dat5_current
#make sure you have a file with only 2016 data
# long term baseline values are different for each area, I guess make a file for each area?
#
# the y = has to be changed for each area but once they are set they are the same from year to year
# THIS NEEDS TO BE A WEIGHTED MEAN - see processingCODE.R
dat5_current %>%
  filter(area == "Juneau") ->long_term_current
wtd.t.test(long_term_current$Large.Females, y = 5.32, 
           weight = long_term_current$weighting, samedata=FALSE)
wtd.t.test(long_term_current$Pre_Recruit, y = 4.24, 
           weight = long_term_current$weighting, samedata=FALSE)
wtd.t.test(long_term_current$Recruit, y = 4.64, 
           weight = long_term_current$weighting, samedata=FALSE)
wtd.t.test(long_term_current$Post_Recruit, y = 2.64, weight = long_term_current$weighting, samedata=FALSE)


##### Weights from length - weight relatinship--------------------
# Linear model is changed for each area
# stephens passage linear model: exp(3.38*log(length in mm)-9.99)*2.2/1000
glimpse(Tdat1) # raw data for all years
Tdat1 %>%
  mutate(weight_lb = (exp((3.38*log(Width.Millimeters)) - 9.99 ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")


datWL %>% 
  group_by(Year) %>% 
  filter(Sex.Code == 1) %>% 
  summarise(mature_lbs = wt.mean(weight_lb[mod_recruit %in% Mature], 
                                 Number.Of.Specimens[mod_recruit %in% Mature]), 
            legal_lbs = wt.mean(weight_lb[mod_recruit %in% Legal], 
                                Number.Of.Specimens[mod_recruit %in% Legal]), 
            prer_lbs = wt.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                               Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights
# final results with score - save here

write.csv(male_weights, paste0('./results/nj_stp/', cur_yr, '/SP_weights.csv'))

##### survey mid-date --------------------
Tdat1 %>%
  filter(Year == cur_yr) %>%
  distinct(Time.Hauled)

##### Females - large or mature females --------------------------
# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

#make sure this does NOT include immature females
# this is egg_condition_code == 4
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
  group_by(Year, area, Pot.No, Less25) %>%
  summarise(no_sum = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + area + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(area, Year)%>%
  summarise(Pclutch = mean(var1)*100 , 
            Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, paste0('./results/nj_stp/', cur_yr, '/SP_precent_low_clutch.csv'))

##### Long term females -------------------------
glimpse(poorclutch1)
#compare 2016 CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == cur_yr) ->poorclutch1_current
#make sure you have a file with only 2016 data
#calculate the t.test
t.test(poorclutch1_current$var1, mu = 0.10)
##### Short term females ------------------------

#look at trend for the last 4 years.  Need a file with last four years in it 
head(poorclutch1) # should have the last 4 years from OceanAK

poorclutch1 %>%
  filter(Year >= cur_yr-3) -> LgF_short # short term file has last 4 years in it

# need to run the regression for each area.
LgF_short %>% 
  group_by(area) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  tidy(fit) %>% 
  filter(term == "Year") %>%
  select(area, estimate) -> one
LgF_short %>% 
  group_by(area) %>%
  do(fit = lm(var1 ~ Year, data =.)) %>%
  glance(fit) %>% select(area, r.squared, p.value) ->two
one %>%
  right_join(two) -> F_short_term_results # estimate here is slope from regression

#Now need to add column for significance and score
F_short_term_results %>%
  mutate(significant = ifelse(p.value < 0.05 & estimate > 0, 1,
                              ifelse(p.value <0.05 & estimate <0, -1, 0))) %>%
  mutate(score = 0.25*significant) -> F_short_term_results #estimate is slope from regression
# final results with score - save here
write.csv(F_short_term_results, paste0('./results/nj_stp/', cur_yr, '/SP_Fem_shortterm.csv'))
ggplot(poorclutch1, aes(Year, var1))+geom_point() 

##### egg percentage overall -----------------------------------
LgF_Tdat1 %>%
  filter(!is.na(Egg.Percent)) %>% 
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Location, Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch
write.csv(percent_clutch, paste0('./results/nj_stp/', cur_yr, '/SP_percent_clutch.csv'))



# panel figure SP ---------------
survey.location = "Juneau"
area = "Stephens Passage"
abrv = "SP"
panel_figure_jnu("Juneau", 2018, "Stephens Passage", "SP", 2)
panel_figure_jnu("Juneau", 2018, "Stephens Passage", "SP", 3)

# presentation figures----------------
panel_figure_jnu_pres("Juneau", 2018, "Stephens Passage", "SP", 2)
panel_figure_jnu_pres("Juneau", 2018, "Stephens Passage", "SP", 3)


# panel figure NJ ----
# issues is that prior to 2009 don't have good idea of what raw data is used.  Take CPUE/SE and 
#     female info from Sigma Plot file.
survey.location = "North Juneau"
area = "North Juneau"
abrv = "NJ"
panel_figure_nj("North Juneau", 2018, "North Juneau", "NJ", 2)
panel_figure_nj("North Juneau", 2018, "North Juneau", "NJ", 3)

# presentation figures----------------
panel_figure_nj_pres("North Juneau", 2018, "North Juneau", "NJ", 2)
panel_figure_nj_pres("North Juneau", 2018, "North Juneau", "NJ", 3)
