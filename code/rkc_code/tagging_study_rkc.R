# K.Palof
# katie.palof@alaska.gov
# 08/03/2018 / 9-4-19 / 8-30-20/ 8-29-21

## RKC tagging data ------
# Tagging project in conjuction with normal survey ---

# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Previous figures were in SigmaPlot. 
#     Figure 2 is regional biomass from CSA estimate - use current year's model

# load -----
source('./code/functions.R')
#dir.create(file.path(paste0('results/rkc/Region1'), cur_yr))

# data -----
cur_yr <- 2021
pry_yr <- cur_yr-1
tags <- read.csv('./data/rkc/tagged_rkc_2021.csv')

head(tags)

tags %>% 
  group_by(Tag.No) %>% 
  summarise(n = sum(Number.Of.Specimens)) %>% 
  filter(n > 1) -> recap_tags
  
recaps <- recap_tags$Tag.No[2:18]

tags %>% 
  filter(Tag.No %in% recaps) -> recaps1

recaps1 %>% 
  select(Year, Tag.No, Recruit.Status, Shell.Condition.Code, Length.Millimeters, Specimen.Comments) %>% 
  arrange(desc(Tag.No)) %>% 
  mutate(Specimen.Comments = ifelse(Tag.No == 150074 & Recruit.Status == "Post_Recruit", "RECAP", 
                                    ifelse(Tag.No == 150091 & Length.Millimeters == 154, "RECAP", Specimen.Comments))) %>% 
  group_by(Tag.No) %>% 
  summarise(diff = Length.Millimeters[2]-Length.Millimeters[1]) -> increment
  
recaps1 %>%
  select(Year, Tag.No, Recruit.Status, Shell.Condition.Code, Length.Millimeters, Specimen.Comments) %>% 
  arrange(desc(Tag.No)) %>% 
  mutate(Specimen.Comments = ifelse(Tag.No == 150074 & Recruit.Status == "Post_Recruit", "RECAP", 
                                    ifelse(Tag.No == 150091 & Length.Millimeters == 154, "RECAP", Specimen.Comments))) -> step1
step1 %>% 
  left_join(increment) %>% 
  filter(Specimen.Comments == "RECAP") %>% 
  arrange(desc(Tag.No)) %>% 
  select(-Year)-> step2

step1 %>% 
  arrange(desc(Tag.No)) %>% 
  filter(Specimen.Comments == "") %>% 
  mutate(Length.mm.1 = Length.Millimeters, Shell.cond.1 = Shell.Condition.Code, 
         Recruit.Status.1 = Recruit.Status) %>% 
  select(Tag.No, Recruit.Status.1, Length.mm.1, Shell.cond.1) %>% 
  arrange(desc(Tag.No)) -> step3

step2 %>% 
  left_join(step3) %>% 
  select(Tag.No, Recruit.Status.1, Shell.cond.1, Recruit.Status, Shell.Condition.Code, 
         Length.mm.1, Length.Millimeters, Specimen.Comments, diff) -> step4

write.csv(step4, paste0(here::here(), "/results/rkc/recap_tagging_proj.csv"), row.names = F)

step4 %>% 
  filter(diff >= 1) %>% 
  summarise(avg_inc = mean(diff), min_inc = min(diff), max_inc = max(diff))
