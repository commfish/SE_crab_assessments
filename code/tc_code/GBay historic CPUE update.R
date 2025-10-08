###GBAY historic CPUE update
# Alex Reich alex.reich@alaska.gov
# this should be one once (in 2025) and ideally will never need to be run again

## in 2024 the survey crew reduced the size of Glacier bay
##and sent me info of the pots from past years set outside this survey area
## as a result, I re- ran the CPUE for 2013-2025
## BUT I still need to update the historical files(1999-2013) for GBAY.
## THIS IS THAT UPDATE - October 2025
library(tidyverse)

#I manipulated the read-in code to get the data before 2013. Here it is
old_data <- read.csv("data/tanner/tanner_tcs/tanner crab survey for CSA_OLD.csv")

area <- read.csv("./data/tanner/tanner_tcs/TCSstrata_area.csv") 
baseline <- read.csv("./data/tanner/tanner_tcs/longterm_means_TC.csv")

#initial review of old data
# remove pots with Pot condition code that's not "normal" or 1 
unique(old_data$Pot.Condition)
dat1 <- old_data %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") %>%
  # only the four survey areas
  filter(Location %in% c("Thomas Bay", "Holkham Bay", "Icy Strait", "Glacier Bay"))

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) # this SHOULD produce NO rows.  If it does you have data problems go back and correct
# before moving forward.
dat1 %>% filter(Recruit.Status == "", Number.Of.Specimens >= 1) -> test1

### add columns used later 
Tdat1 <- dat1 %>%
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
                                                                        ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) 


# confirm this worked
Tdat1 %>% 
  filter(mod_recruit == "Missing") #some issues in 2010 in Holkham but I've bothered Zane enough this week- FLAG for later

old_data <- Tdat1

#just interested in gbay
Gbay_old_data <- old_data %>% filter(Location == "Glacier Bay")

#read in the matchy matchy pots I dont want in GBAY data
GB_pots_outside <- read.csv("data/glacier bay 2024 restrat/Glacier Bay pots in old strata.csv")

#get my new historical dataset
Old_pots_gone <- Gbay_old_data %>%
  anti_join(
    GB_pots_outside,
    by = c("Location", "Year", "Pot.No")
  )
#flag. DAMMIT. unique(Old_pots_gone %>% filter(is.na(Density.Strata.Code))%>% select(Year)) - many years are missing strata assigments. 
##Another data request for Zane, it will have to wait until post-survey.

#now I need to re-run the CPUE standardization for these old years:
##and the format being the same, code from TCS_processing should work
dat2 <- Old_pots_gone %>%
  group_by(Year, Pot.No, Location, Density.Strata.Code, mod_recruit) %>% 
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit))

dat3 <- dcast(dat2, Year + Location + Pot.No + Density.Strata.Code ~ mod_recruit, sum, drop=TRUE)

# Join area input file with dat3 - which is the data summarized by pot.  Each sampling area has it's own area file or area per
#     strata.  This is used to calculating the weighting for weighted CPUE.
tab <- dat3 %>%
  right_join(area)

# Calculates the number of pots per strata.  
pots_per_strata <- tab %>%
  group_by(Year, Location, Density.Strata.Code) %>%
  summarise(npots  = length(Pot.No))

##### Weighted CPUE current year -----------------------------------
# the weighting is the product of the area for each strata and the inverse (1/n) of the number of pots per strata per year
# need to combine data sets to accomplish this.
dat4 <- tab %>%
  right_join(pots_per_strata)

dat5 <- dat4 %>%
  mutate(inverse_n = 1 / npots, weighting = inverse_n * Area_km)

#check to make sure there aren't crab without a assigned recruit class.
dat5 %>%
  filter(No_crab > 0)

# This version is ready to calculate CPUE for each recruit class
# Calculates a weighted mean CPUE and SE for each recruit class
CPUE_wt_all <- dat5 %>%
  group_by(Location, Year) %>%
  summarise(Pre_Recruit_wt = weighted.mean(Pre_Recruit, weighting), PreR_SE = (weighted.sd(Pre_Recruit, weighting)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_wt = weighted.mean(Recruit, weighting), Rec_SE = (weighted.sd(Recruit, weighting)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_wt = weighted.mean(Post_Recruit, weighting), PR_SE = (weighted.sd(Post_Recruit, weighting)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_wt = weighted.mean(Juvenile, weighting), Juv_SE = (weighted.sd(Juvenile, weighting)/(sqrt(sum(!is.na(Juvenile))))), 
            SmallF_wt = weighted.mean(Small.Females, weighting), SmallF_SE = (weighted.sd(Small.Females, weighting)/(sqrt(sum(!is.na(Small.Females))))),
            MatF_wt = weighted.mean(Large.Females, weighting), MatF_SE = (weighted.sd(Large.Females, weighting)/(sqrt(sum(!is.na(Large.Females))))))

write.csv(CPUE_wt_all, "results/tanner/tanner_tcs/2025/GBAY_fixed_historic_CPUE_99_12.csv")
