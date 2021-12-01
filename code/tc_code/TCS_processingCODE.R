#K.Palof 
# ADF&G 11-4-16 / 10-17-17 / 10-26-18 / 10-29-19 / 11-11-20 / 11-3-21
# Areas: Tanner crab survey areas - includes Holkham, Thomas, Glacier Bay and Icy Strait
# code to process data from Ocean AK to use in crab CSA models.  
# Prior to 2016 this was done in excel then JMP

#####Load ------------
source('./code/tanner_functions.R')
cur_yr <- 2021
fig_path <- paste0('figures/tanner/', cur_yr) # folder to hold all figs for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('results/tanner/tanner_tcs/', cur_yr) # output and results
dir.create(output_path) 

# Load Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/tanner/tanner_tcs/tanner crab survey for CSA_13_21.csv")
# file name changes annually - maybe should change this??? need to update end date
# this is input from OceanAK - set up as tanner crab survey for CSA
area <- read.csv("./data/tanner/tanner_tcs/TCSstrata_area.csv") 
baseline <- read.csv("./data/tanner/tanner_tcs/longterm_means_TC.csv")
# brought in all data since 2013 - this was after survey was stratified.  
# biomass <- read.csv() # need to bring this in for figures later. **FIX**

### historic file ---------
# need this data for biomass and CPUE trend figures
hist_dat <- read.csv(paste0('./results/tanner/tanner_tcs/',cur_yr-1, '/',cur_yr-1,'_CPUE_historic.csv'))
# Older data needs to be imported 1997 to 2013 has post-strata asignments. 
#   DO NOT take from OceanAK since it won't have survey strata designations in OceanAK

## survey data QAC -------
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
unique(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1) #-> test1
# check pot 15, 2013 Holkham Bay - pull again from OceanAK **FIX**

# **FIX **  calculate soak time 
#come back later and add a soak time column - tanner soak time should be between 16-20??? double check this

##### Tanner specific manipulations -----------------------------
###     Tanner Survey areas ONLY 
#   confirm that only the four surveys areas are present.
unique(dat1$Location) # 

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
# same issues with pot 15 in HB, 2013 **FIX**

## CPUE calc -----------------
##### By Pot ----------------------------------------------------
# Now summarize by pot - remember to keep areas seperate.
# need Number of Specimens by recruit class USE mod_recruit here.
Tdat1 %>%
  group_by(Year, Location, Pot.No, Density.Strata.Code, mod_recruit) %>% 
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit)) -> dat2

dat3 <- dcast(dat2, Year + Location + Pot.No + Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
dat3 %>%
  right_join(area) -> tab
# Calculates the number of pots per strata.  
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

# This version is ready to calculate CPUE for each recruit class
# Calculates a weighted mean CPUE and SE for each recruit class
dat5 %>%
  group_by(Location, Year) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = weighted.mean(Juvenile, weighting), Juv_SE = (weighted.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females))))),
            MatF_wt = weighted.mean(Large.Females, weighting), MatF_SE = (weighted.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females)))))) -> CPUE_wt_all
# check to confirm previous years CPUEs match
write.csv(CPUE_wt_all, paste0('./results/tanner/tanner_tcs/', cur_yr, '/', cur_yr,'CPUE_all.csv')) # contains last four years of survey data 

# create historic file --------------
hist_dat %>% 
  #select(-X) %>% 
  filter(Year < 2013) %>% 
  bind_rows(CPUE_wt_all) -> all_CPUE_data
write.csv(all_CPUE_data, paste0('./results/tanner/tanner_tcs/', cur_yr, '/', cur_yr,'_CPUE_historic.csv'), 
          row.names = FALSE)

##### Short term trends -------------------------------------
# look at trend for the last 4 years.  Need a file with last four years

# function 
head(dat5) # make sure this is the file with each recruit class as a column by year, location and pot no
dat5 %>%
  #select (- `NA`) %>% 
  filter(Year >= (cur_yr -3)) -> dat5 # confirm that is only contains the last 4 years.  This year needs to be changed every year

short_t_tanner(dat5, cur_yr)

dat5_long <- gather(dat5, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE)

ggplot(dat5_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~Location)

### specific area/class combos that need a closer looks ------------
### just thomas bay Large.Females
#dat3_long %>%
#  filter(Location == 'Thomas Bay', mod_recruit == 'Large.Females') -> graph1
#ggplot(graph1, aes(Year, crab, color = mod_recruit)) + geom_point() +geom_smooth(method = 'lm')
### just holkham bay for recruits
#dat3_long %>%
#  filter(Location == 'Holkham Bay', mod_recruit == 'Recruit') -> graph1
#ggplot(graph1, aes(Year, crab, color = mod_recruit)) + geom_point() +geom_smooth(method ='lm')

##### Long term trends ---------------------
# compare current year's data to long term mean - for each Location
# need to use dat5 because the weighting column is needed.

areas <- c('Icy Strait', 'Glacier Bay', 'Holkham Bay', 'Thomas Bay')

long_term <- lapply(areas, long_loop_17, curyr = cur_yr) # uses dat5 data
long_term_all <- bind_rows(long_term)

write.csv(long_term_all, paste0('./results/tanner/tanner_tcs/', cur_yr,'/long_term.csv'))

##### Weights from length - weight relatinship--------------------
#
# Linear model is changed for each area
weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())

#Location = unique(dat5$Location) #"Glacier Bay" "Holkham Bay" "Icy Strait"  "Thomas Bay" 
#slope = c(3.30, 3.34, 3.29, 3.32) # these are from W-L relationships established for each area
#coeff = c(9.48, 9.73, 9.48, 9.67)

weight_length <- data.frame(Location = unique(dat5$Location), slope = c(3.30, 3.34, 3.29, 3.32),
                            coeff = c(9.48, 9.73, 9.48, 9.67))

glimpse(Tdat1) # raw data f 
Tdat1 %>%
  right_join(weight_length) %>%
  mutate(weight_lb = (exp((slope*log(Width.Millimeters)) - coeff ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")

datWL %>% 
  group_by(Location, Year) %>% 
  filter(Sex.Code == 1) %>% 
  summarise(mature_lbs = weighted.mean(weight_lb[mod_recruit %in% Mature], 
                                 Number.Of.Specimens[mod_recruit %in% Mature]), 
            legal_lbs = weighted.mean(weight_lb[mod_recruit %in% Legal], 
                                Number.Of.Specimens[mod_recruit %in% Legal]), 
            prer_lbs = weighted.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                               Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights
# final results with score - save here
write.csv(male_weights, paste0('./results/tanner/tanner_tcs/', cur_yr, '/TCS_weights.csv'))


##### Females - large or mature females --------------------------
# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

# make sure this does NOT include immature females
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
write.csv(percent_low_clutch, paste0('./results/tanner/tanner_tcs/', cur_yr, '/TCS_percent_low_clutch.csv'))

##### Long term females -------------------------
glimpse(poorclutch1)
#compare current year's CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == cur_yr) ->poorclutch1_current
#make sure you have a file with only current year's  data and that area vector is defined - line 146

#calculate the t.test
Fem_long_term <- lapply(areas, Fem_long_loop) #assumes above file is named 'poorclutch1_current'
Fem_long_term

Fem_long_term_all <- bind_rows(Fem_long_term)
write.csv(Fem_long_term_all, paste0('./results/tanner/tanner_tcs/', cur_yr,'/Female_long_term.csv'))

##### Short term females ------------------------
#look at trend for the last 4 years. 
head(poorclutch1) # has data since 2013

poor_clutch_short(poorclutch1, cur_yr)

ggplot(poorclutch1, aes(Year, var1))+geom_point() +facet_wrap(~Location)

##### egg percentage overall -----------------------------------
LgF_Tdat1 %>%
  filter(!is.na(Egg.Percent)) %>% 
  group_by(Year, Location, Pot.No) %>%
  summarise (egg_mean = weighted.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(Location, Year)%>%
  summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch
write.csv(percent_clutch, paste0('./results/tanner/tanner_tcs/', cur_yr,'/TCS_percent_clutch.csv'))

## female historic data combined ----------------
historic_low <- read.csv('./results/tanner/tanner_tcs/historic_TCS_precent_low_clutch.csv')
historic_clutch <- read.csv('./results/tanner/tanner_tcs/historic_TCS_percent_clutch.csv')

historic_low %>% 
  filter(Year < 2013) %>% 
  select (-X) %>% 
  bind_rows(percent_low_clutch) %>% 
  write.csv(paste0('./results/tanner/tanner_tcs/', cur_yr, '/all_years_percent_low_clutch.csv'))

historic_clutch %>% 
  filter(Year < 2013) %>% 
  select (-X) %>% 
  bind_rows(percent_clutch) %>% 
  write.csv(paste0('./results/tanner/tanner_tcs/', cur_yr, '/all_years_percent_clutch.csv'))


## stock health -------------
total_health("tanner_tcs", cur_yr)

### STOP here and run .Rmd file for these results ------------------------

# need commericial harvest from fish tickets for each area CSA - look in:
# ./results/tanner/tanner_comm_catch_97_2019.csv and tanner_mid_catch_date2019.csv

# need to update biomass .csv with output from CSA models for all areas

## panel figures -----
panel_figure("Icy Strait", cur_yr, "Icy Strait", 2, "include", 0.55, 0.8)
panel_figure("Icy Strait", cur_yr, "Icy Strait", 3, "include", 0.55, 0.8)

panel_figure("Glacier Bay", cur_yr, "Glacier Bay", 2, "include", 0.55, 0.8)
panel_figure("Glacier Bay", cur_yr, "Glacier Bay", 3, "include", 0.55, 0.8)

panel_figure("Thomas Bay", cur_yr, "Thomas Bay", 2, "include", 0.55, 0.8)
panel_figure("Thomas Bay", cur_yr, "Thomas Bay", 3, "include", 0.55, 0.8)

panel_figure("Holkham Bay", cur_yr, "Holkham Bay", 2, "include", 0.35, 0.8)
panel_figure("Holkham Bay", cur_yr, "Holkham Bay", 3, "include", 0.55, 0.8)


# non-confidential areas  ------------
panel_figure("Icy Strait", cur_yr, "Icy Strait", 2, "exclude", 0.55, 0.8)
panel_figure("Holkham Bay", cur_yr, "Holkham Bay", 2, "exclude", 0.35, 0.8)
panel_figure("Thomas Bay", cur_yr, "Thomas Bay", 2, "exclude", 0.55, 0.8)
panel_figure("Glacier Bay", cur_yr, "Glacier Bay", 2, "exclude", 0.55, 0.8)

## these are NOT up to date DO NOT USE without fixing input files.
# presentation figures----------------
panel_figure_pres("Icy Strait", cur_yr, "Icy Strait", 2, "exclude")
panel_figure_pres("Icy Strait", cur_yr, "Icy Strait", 3, "exclude")

panel_figure_pres("Glacier Bay", cur_yr, "Glacier Bay", 2, "exclude")
panel_figure_pres("Glacier Bay", cur_yr, "Glacier Bay", 3, "exclude")

panel_figure_pres("Holkham Bay", cur_yr, "Holkham Bay", 2, "exclude")
panel_figure_pres("Holkham Bay", cur_yr, "Holkham Bay", 3, "exclude")

panel_figure_pres("Thomas Bay", cur_yr, "Thomas Bay", 2, "exclude")
panel_figure_pres("Thomas Bay", cur_yr, "Thomas Bay", 3, "exclude")


