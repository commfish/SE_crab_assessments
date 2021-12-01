# k.palof
# SE Tanner crab Bitter crab disease incidence from survey 

# 11-30-2021 

# prep for KTTF meeting presenation on Tanner crab surey results

# load ------
# load -----
source('./code/tanner_functions.R')

# data -----
cur_yr <- 2021

raw_data <- read.csv(paste0('./data/tanner/tanner_tcs/Bitter_crab_Tanner_survey_', cur_yr, '.csv')) 
raw_rkc <- read.csv(paste0('./data/tanner/tanner_rkc/Bitter_crab_RKC_survey_', cur_yr, '.csv')) 

## summarize -----
raw_data %>% 
  group_by(Location, Year, Parasite) %>% 
  summarise(total_crab = sum(Number.Of.Specimens)) -> step1

raw_rkc %>% 
  group_by(Location, Year, Parasite) %>% 
  summarise(total_crab = sum(Number.Of.Specimens)) -> step2

step1 %>% 
  bind_rows(step2) -> by_survey_area

by_survey_area %>% 
  #select(-Parasite.Code) %>% 
  spread(Parasite, total_crab) %>% 
  mutate(total_crab = sum(V1, `Bitter crab`, `None present`, na.rm = TRUE), 
         per_bitter = (`Bitter crab`/total_crab)*100, 
         per_bitter = round(ifelse(is.na(per_bitter), 0, per_bitter), 1)) -> bitter_summary

### table and figure output -----
bitter_summary %>% 
  select(Location, Year, per_bitter) %>% 
  filter(Year >= cur_yr-1) %>% 
  spread(Year, per_bitter) %>% 
  arrange(desc(`2020`)) -> table_bitter
write.csv(table_bitter, paste0('./results/tanner/', cur_yr, '/bitter_crab_table_KTTF_pwpt.csv'))

table_bitter %>% 
  melt(id.vars = "Location") %>% 
  rename(Year = variable, per_bitter = value) %>% 
  ggplot(aes(x = reorder(Location, -per_bitter), y = per_bitter, fill = Year)) +
   geom_bar(stat = "identity", position = position_dodge(), color = "black") +
   scale_fill_manual(values=c('#999999','#E69F00')) +
   theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(title= "Bitter crab occurence in SE Tanner crab survey data",
       x="Survey Location",y="% Bitter crab") +
  ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_bitter_crab_KTTF_pwpt.png'), dpi = 800,
         width = 8.5, height = 6.0)
