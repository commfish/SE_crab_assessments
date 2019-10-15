#K.Palof 
# ADF&G 9-21-18 updated and reworked similar to RKC code
# Areas: RKCS areas for Tanner crab - EXCLUDES north juneau and stephens passage
# includes: Excursion, Seymour Canal, Pybus Bay, Gambier Bay, Peril Strait, and Lynn Sisters
# code to process data from Ocean AK to use in crab CSA models.  

## Load ---------------------------------
source('./code/tanner_rkc_functions.R') # need to create versions of this code to deal with mutiple areas at once.
## setup global ---------------
cur_yr <- 2019
pr_yr <- cur_yr -1

## Data ---------------------------------------------------
# change input file and input folder for each
dat <- read.csv("./data/tanner/tanner_rkc/red crab survey for Tanner crab CSA_19.csv")
                  # this is input from OceanAK - set up as red crab survey data for CSA has all years
                  # 1997 to present 
                  # all data in this file do not need area, historic or female files here
baseline <- read.csv("./data/tanner/tanner_rkc/longterm_means_TC.csv")
biomass <- read.csv("./data/tanner/tanner_rkc/tanner_2019_biomassmodel.csv") 
# this file should be updated with current year model output.

# survey data QAC -------
head(dat)
glimpse(dat) # confirm that data was read in correctly.

##### Initial review of new data ---------------------------------
# remove pots with Pot condition code that's not "normal" or 1 
levels(dat$Pot.Condition)
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1) -> test1
# 2018 excursion pot 2 and 42
# 2019 gambier bay pot 39
write.csv(test1, "./results/tanner/tanner_rkc/data_issues.csv")
# **FIX **  calculate soak time 
#come back later and add a soak time column - tanner soak time should be between 16-20??? double check this

##### Tanner specific manipulations -----------------------------
####Survey areas ONLY 
# remove Juneau and Barlow - do these seperately due to needing GIS to seperate the areas.
# also need to remove other "experimental" areas - simplify to areas used in the assessment
levels(dat1$Location)
rkc_usable_areas <- c("Deadman Reach", "Excursion Inlet", "Gambier Bay", "Lynn Sisters", "Pybus Bay", 
                      "Seymour Canal", "St. James Bay")
dat1 %>%
  filter(Location %in% rkc_usable_areas) %>%
  mutate(experimental = ifelse(Location == 'Seymour Canal' & Pot.No > 54 & Year == 2015, 1, 
                               ifelse(Location == 'Seymour Canal' &  Pot.No > 54 & Year == 2019, 1,
                                      ifelse(Location == "Excursion Inlet" & Pot.No > 56 & Year == 2019, 1, 0 )))) %>%
  filter(experimental == 0) -> dat1a
  # remove seymour canal swan cove pots, these are only pot #'s greater than 54 in 2015.

  # need to confirm that rkcs area tagging pots in 2019 - seymour and excusion are NOT included
dat1a %>% 
  filter(Year == 2019) %>% 
  group_by(Location) %>% 
  summarise(n = max(Pot.No)) # confirm 2019 pot removed.  **FIX** this for future

##### add columns used later ----------------------------
dat1a %>%
  mutate(AREA = ifelse(Location.Code == 26 | Location.Code == 42, 'LS',
                       ifelse(Location.Code == 4, 'PS', ifelse(Location.Code == 9, 'EI', 
                            ifelse(Location.Code== 15, 'GB', ifelse(Location.Code == 37, 
                                'PB', ifelse(Location.Code==39, 'SC', 0))))))) ->dat1ab

dat1ab %>%
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

Tdat1 %>% 
  filter(mod_recruit == "Missing") %>%  # check for data issues
  write.csv('./results/tanner/tanner_rkc/problemstanner1.csv')
Tdat1 %>% filter(is.na(mod_recruit)) %>% 
  write.csv('./results/tanner/tanner_rkc/problemstanner2.csv')
##### By Pot ----------------------------------------------------
#Now summarize by pot - remember to keep areas seperate.
#Need Number of Specimens by recruit class
Tdat1 %>%
  group_by(Year, AREA, Pot.No, mod_recruit) %>% # use AREA here instead of location due to 
                                                #  multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit)) -> dat2 #remove any NAs due to data issues.

dat3 <- dcast(dat2, Year + AREA + Pot.No ~ mod_recruit, sum, drop=TRUE)

#head(dat3)# check to make sure things worked.

# No weighting by strata here for RKCS data due to it being designed for RKC.

##### CPUE for all years ----------------------------------
# This version is ready to calculate CPUE for each recruit class
# Calculates a  mean CPUE and SE for each recruit class 
# not weighted due to lack of tanner specific strata on red crab survey
dat3 %>%
  group_by(AREA, Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_all
# check to confirm last years CPUEs match - that's why we use two years.
# change name and folder for each area
write.csv(CPUE_all, paste0('./results/tanner/tanner_rkc/', cur_yr, '/rkcs_CPUE_all.csv'))

##### Historic file ---------------------------------------

#brought in all the years - 2013 to 2016 - needed at once from OceanAK in the future can do this or add current
#   year to this file.
# this is the final file by pot.  Now this file can be summarized to give CPUE by year like above (see dat 5 to CPUE_wt_JNU_2016)
# change same of folder and file.
write.csv(dat3, paste0('./results/RKCS_tanner/', cur_yr, '/RKCS_perpot_allyears.csv'))

##### Short term trends -------------------------------------
# look at trend for the last 4 years.  Need a file with last four years
# attempt to use broom for short term trends 
# tidy(Lfem_fit) # want to save $estimate here
# glance(Lfem_fit) # want to save r.squared and p.value

head(dat3)
dat3 %>%
  filter(Year >= cur_yr-3 ) %>% 
  select( -Missing) -> dat3a # confirm that is only contains the last 4 years.  This year needs to be changed every year
#remove Missing and NA columns

short_t_tanner(dat3a, cur_yr)

dat3a_long <- gather(dat3a, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) # need the long version for this.

ggplot(dat3a_long, aes(Year, crab, color = mod_recruit))+geom_point() +facet_wrap(~AREA)

##### Long term trends ---------------------
# compare current year CPUE distribution to the long term mean - need to create or have file with long term means.
dat3 %>%
  filter(Year == cur_yr) ->dat3_curyr
baseline
# make sure you have a file with only current years data 

# create a function to run the t.test and loop it over each area

# long_ttest function does not need editing each year, BUT the long_loop_17 
#     function needs to be edited with the current year and confirmed that the 
#     input files are the same names.

areas <- c('PB', 'EI', 'LS', 'GB', 'SC', 'PS')

long_term <- lapply(areas, long_loop_17, curyr = cur_yr)
write.csv(long_term, paste0('./results/RKCS_tanner/', cur_yr, '/long_term.csv'))

##### Weights from length - weight relatinship--------------------
# Linear model is changed for each area
weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())

#AREA = unique(Tdat1$AREA) #"LS" "PS" "EI" "GB" "PB" "SC"
#slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10)
#coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55)

weight_length <- data.frame(AREA = unique(Tdat1$AREA), slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10),
                            coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55))

glimpse(Tdat1) # raw data for all years
Tdat1 %>%
  right_join(weight_length) %>%
  mutate(weight_lb = (exp((slope*log(Width.Millimeters)) - coeff ))*(2.2/1000))-> datWL

Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
Legal =c("Recruit", "Post_Recruit")

datWL %>% 
  group_by(AREA, Year) %>% 
  filter(Sex.Code == 1) %>% 
  summarise(mature_lbs = wt.mean(weight_lb[mod_recruit %in% Mature], 
                                 Number.Of.Specimens[mod_recruit %in% Mature]), 
            legal_lbs = wt.mean(weight_lb[mod_recruit %in% Legal], 
                                Number.Of.Specimens[mod_recruit %in% Legal]), 
            prer_lbs = wt.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                               Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights

write.csv(male_weights, paste0('./results/RKCS_tanner/', cur_yr, '/RKCS_weights.csv'))


##### mid-date survey-------------
glimpse(Tdat1)
# need just the date of Time.Set and then to get the mid-date
#Tdat1 %>%
# mutate(time.set = as.POSIXlt(Time.Set)) -> Tdat1
Tdat1 %>% filter(Year == 2018 & AREA == "SC")


##### Females - large or mature females --------------------------

# large or mature females
Tdat1 %>%
  filter(Sex.Code == 2, mod_recruit == 'Large.Females') -> LgF_Tdat1

##### % poor (<10 %) clutch -----------------------------------
# This selects those rows that do not have an egg percentage.
# if these rows have a egg. development code and egg condition code then the egg percentage should be there
# if developement = 3 and condition is 4 or 5 then egg percentage should be 0.
LgF_Tdat1[is.na(LgF_Tdat1$Egg.Percent),]
# change to 0 only if followed by a Egg.Development.Code and Egg.Condition. Code
LgF_Tdat1 %>%
  mutate(Egg.Percent =ifelse(is.na(Egg.Percent) & Egg.Development.Code > 0,
                             0, Egg.Percent)) -> LgF_Tdat1

LgF_Tdat1 %>%
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_Tdat1 # where 1 is yes and 2 is no

LgF_Tdat1 %>%
  group_by(Year, AREA, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + AREA + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) %>% 
  filter(!is.na(var1)) -> poorclutch1

poorclutch1 %>%
  group_by(AREA, Year)%>%
  summarise(Pclutch = mean(var1)*100 , Pclutch.se = ((sd(var1))/sqrt(sum(!is.na(var1))))*100) -> percent_low_clutch
write.csv(percent_low_clutch, paste0('./results/RKCS_tanner/', cur_yr, '/RKCS_percent_low_clutch.csv'))

##### egg percentage overall -----------------------------------
LgF_Tdat1 %>%
  group_by(Year, AREA, Pot.No) %>%
  summarise (egg_mean = wt.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot

clutch_by_pot %>%
  group_by(AREA, Year)%>%
  summarise(mean = mean(egg_mean, na.rm = TRUE), 
            egg.se = (sd(egg_mean, na.rm = TRUE)/sqrt(sum(!is.na(egg_mean))))) ->percent_clutch

# add this to the table with percent_low_clutch?
percent_low_clutch %>%
  right_join(percent_clutch) -> female_clutch_info
write.csv(female_clutch_info, paste0('./results/RKCS_tanner/', cur_yr, '/RKCS_percent_clutch.csv'))


##### Long term females -------------------------
glimpse(poorclutch1)
#compare current year's CPUE distribution to the long term mean
poorclutch1 %>%
  filter(Year == cur_yr) ->poorclutch1_current
#make sure you have a file with only current years data
#calculate the t.test

Fem_long_term <- lapply(areas, Fem_long_loop)
Fem_long_term
write.csv(Fem_long_term, paste0('./results/RKCS_tanner/', cur_yr, '/Female_long_term.csv'))
# need to figure out a way to store these results in a better format

##### Short term females ------------------------
# look at trend for the last 4 years.  Need a file with last four years in it - females from above

head(poorclutch1) # has all years of data from OceanAK

# need to run the regression for each area.
# use function found in source function code
poor_clutch_short(poorclutch1, cur_yr)

ggplot(poorclutch1, aes(Year, var1))+geom_point() +facet_wrap(~AREA)
###

## panel figures -----
panel_figure("EI", 2018, "Excursion Inlet", 2, "include")
panel_figure("EI", 2018, "Excursion Inlet", 3)

panel_figure("SC", 2018, "Seymour Canal", 2, "include")
panel_figure("SC", 2018, "Seymour Canal", 3)

panel_figure("PB", 2018, "Pybus Bay", 2, "include")
panel_figure("PB", 2018, "Pybus Bay", 3)

panel_figure("GB", 2018, "Gambier Bay", 2, "include")
panel_figure("GB", 2018, "Gambier Bay", 3)

panel_figure("PS", 2018, "Peril Strait", 2, "include")
panel_figure("PS", 2018, "Peril Strait", 3)

panel_figure("LS", 2018, "Lynn Sisters", 2, "include")
panel_figure("LS", 2018, "Lynn Sisters", 3)


# non-confidential areas 2018 ------------
panel_figure("GB", 2018, "Gambier Bay", 2, "exclude")
panel_figure("LS", 2018, "Lynn Sisters", 2, "exclude")
panel_figure("PS", 2018, "Peril Strait", 2, "exclude")
panel_figure("PB", 2018, "Pybus Bay", 2, "exclude")

# presentation figures ------------
panel_figure_pres("EI", 2018, "Excursion Inlet", 2, "include")
panel_figure_pres("EI", 2018, "Excursion Inlet", 3, "include")

panel_figure_pres("SC", 2018, "Seymour Canal", 2, "include")
panel_figure_pres("SC", 2018, "Seymour Canal", 3, "include")

panel_figure_pres("PB", 2018, "Pybus Bay", 2, "exclude")
panel_figure_pres("PB", 2018, "Pybus Bay", 3, "exclude")

panel_figure_pres("GB", 2018, "Gambier Bay", 2, "exclude")
panel_figure_pres("GB", 2018, "Gambier Bay", 3, "exclude")

panel_figure_pres("PS", 2018, "Peril Strait", 2, "exclude")
panel_figure_pres("PS", 2018, "Peril Strait", 3, "exclude")

panel_figure_pres("LS", 2018, "Lynn Sisters", 2, "exclude")
panel_figure_pres("LS", 2018, "Lynn Sisters", 3, "exclude")


####
##### input for CSA in R ---------------------------
####













