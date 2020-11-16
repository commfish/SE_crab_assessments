# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Figure 1: regional biomass from CSA estimate - using ONLY survey areas 
#                 - As of 2018 uses current years model output.  
#                 - Needs to be adjusted in early years for areas that weren't surveyed at the beginning of the 
#                     time series - see below.
#     Figure 2: Harvest and standardized commercial CPUE (based on the year with the fewest pot lifts - 2008/09)  
#               - pot lifts in that season were 12,521

# K.Palof
# katie.palof@alaska.gov
# 11/07/2018 / 11-15-2020

# load -----
source('./code/tanner_functions.R')

# data -----
cur_yr <- 2020
#survey_biomass <- read.csv("./data/TCS/survey_areas_biomass.csv") #add to each year
# above file had point estimates from each year and was kept historically in SigmaPlot. Now this is tracked in 
# an appendix table in the stock health document. 
biomass <- read.csv(paste0('./data/tanner/tanner_', cur_yr, '_biomassmodel.csv'))          
harvest_old <- read.csv("./data/harvest/Tanner_Detailed Fish Tickets_85_18.csv")
harvest <- read.csv(paste0('./data/harvest/Tanner_Detailed Fish Tickets_ALL_years_', cur_yr, '.csv'))
# add current years catch to this file or repull all years
std_cpue <- read.csv(paste0("C:/Users/kjpalof/Documents/R projects/tanner-crab/results/std_commericial_cpue", cur_yr, ".csv"))
#calculated in a seperate project "tanner-crab"
hist_biomass <- read.csv("./data/tanner/tanner_annual_pt_estimate_historic.csv")


# data prep for Figure 1 ---------------
biomass %>% 
  group_by(Year) %>% 
  summarise(Total_L = sum(Legal), Total_M = sum(Mature)) -> year_totals

# adjustments for missing data --------
biomass %>% 
  select(-Prerecruit) %>% 
  filter(Year <= 2001)
# Thomas Bay - no estimates for 1997, 1998, 1999, 2000
# Holkham Bay - no 1997
# Glacier Bay - no 1997, 1998
adj.97 <- c("Thomas Bay", "Holkham Bay", "Glacier Bay")
adj.98 <- c("Thomas Bay", "Glacier Bay")
adj.99 <- ("Thomas Bay")
## adjustments using all years ---
# first survey year until 2018 
 biomass %>% 
  left_join(year_totals) %>% 
  filter(Area == "Thomas Bay"| Area == "Glacier Bay"| Area == "Holkham Bay") %>% 
  mutate(prop_L = Legal/Total_L, prop_M = Mature/Total_M) %>% 
  group_by(Area) %>% 
  summarise(avg.ctb.L = mean(prop_L), avg.ctb.M = mean(prop_M)) -> data_adjust1

data_adjust1 %>% 
  mutate(adj.97L = sum(avg.ctb.L), 
         adj.97M = sum(avg.ctb.M), 
         adj.98L = sum(avg.ctb.L[Area %in% adj.98]), 
         adj.98M = sum(avg.ctb.M[Area %in% adj.98]), 
         adj.99L = sum(avg.ctb.L[Area %in% adj.99]), 
         adj.99M = sum(avg.ctb.M[Area %in% adj.99])) -> data_adjust1

Year <- c(1997:2000)
adj_L <- c(data_adjust1$adj.97L[1], data_adjust1$adj.98L[1], data_adjust1$adj.99L[1], data_adjust1$adj.99L[1])
adj_M <- c(data_adjust1$adj.97M[1], data_adjust1$adj.98M[1], data_adjust1$adj.99M[1], data_adjust1$adj.99M[1])

adjust <- data.frame(Year, adj_L, adj_M) 

# add adjustments to the totals in years neccesary
year_totals %>% 
  left_join(adjust) %>% 
  mutate(Legal = ifelse(!is.na(adj_L), Total_L*(1+adj_L), Total_L), 
         Mature = ifelse(!is.na(adj_M), Total_M*(1+adj_M), Total_M)) %>% 
  select(Year, Legal, Mature) -> cur_yr_biomass
write.csv(cur_yr_biomass, paste0('./results/tanner/', cur_yr, '/surveyed_areas_total_', cur_yr, '_model.csv'))
# these are listed in Table A1 - Appendix in Tanner crab stock health document

## adjustments using data pre-2007 -------
# first survey year up to 2007 - uses 2007 data
# NOT CURRENTLY used was done for an exercise to determine difference.
biomass %>% 
  left_join(year_totals) %>% 
  filter(Area == "Thomas Bay"| Area == "Glacier Bay"| Area == "Holkham Bay") %>% 
  mutate(prop_L = Legal/Total_L, prop_M = Mature/Total_M) %>% 
  filter(Year <= 2007) %>% 
  group_by(Area) %>% 
  summarise(avg.ctb.L = mean(prop_L), avg.ctb.M = mean(prop_M)) -> data_adjust2

data_adjust2 %>% 
  mutate(adj.97L = sum(avg.ctb.L), 
         adj.97M = sum(avg.ctb.M), 
         adj.98L = sum(avg.ctb.L[Area %in% adj.98]), 
         adj.98M = sum(avg.ctb.M[Area %in% adj.98]), 
         adj.99L = sum(avg.ctb.L[Area %in% adj.99]), 
         adj.99M = sum(avg.ctb.M[Area %in% adj.99])) -> data_adjust2

Year <- c(1997:2000)
adj_L <- c(data_adjust2$adj.97L[1], data_adjust2$adj.98L[1], data_adjust2$adj.99L[1], data_adjust2$adj.99L[1])
adj_M <- c(data_adjust2$adj.97M[1], data_adjust2$adj.98M[1], data_adjust2$adj.99M[1], data_adjust2$adj.99M[1])

adjust2 <- data.frame(Year, adj_L, adj_M) 

# add adjustments to the totals in years neccesary
year_totals %>% 
  left_join(adjust2) %>% 
  mutate(Legal = ifelse(!is.na(adj_L), Total_L*(1+adj_L), Total_L), 
         Mature = ifelse(!is.na(adj_M), Total_M*(1+adj_M), Total_M)) %>% 
  select(Year, Legal, Mature) -> cur_yr_biomass2

# Figure 1 ------------
# Now is calculated base on the current years model output.
# use average contribution in early years with all years data 
cur_yr_biomass %>% 
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, y = pounds/1000000, group = type)) +
  geom_line(aes(color = type, linetype = type))+
  geom_point(aes(fill = type, shape = type), size =3) +
  scale_fill_manual(name = "", values = c("black", "gray100")) + 
  scale_colour_manual(name = "", values = c("gray1", "grey48"))+
  scale_shape_manual(name = "", values = c(21, 21))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  ylab("Biomass (1,000,000 lbs)") + 
  xlab("Survey Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(limits = c(0,max(cur_yr_biomass$Mature/1000000, 
                                                      na.rm = TRUE) + 1.5), 
                     breaks= seq(min(0), max(max(cur_yr_biomass$Mature/1000000, 
                                                 na.rm = TRUE)+ 1.5), by = 1.0)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

  ggsave(paste0('./figures/tanner/',  cur_yr,'/', cur_yr,'_figure1_curyr_data.png'), dpi = 800,
         width = 8, height = 5.75)

# Figure A1 for appendix --------------
# make sure you update this csv with current year values - or pull from above 
tail(hist_biomass) 
  
# these are projected biomass for each end year - old data/years here are NOT updated.
  
# add code to pull in current year from above here and then re-save from the following year
 hist_biomass %>% 
    gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
    ggplot(aes(Year, y = pounds/1000000, group = type)) +
    geom_line(aes(color = type, linetype = type))+
    geom_point(aes(fill = type, shape = type), size =3) +
    scale_fill_manual(name = "", values = c("black", "gray100")) + 
    scale_colour_manual(name = "", values = c("gray1", "grey48"))+
    scale_shape_manual(name = "", values = c(21, 21))+
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    ylab("Biomass (1,000,000 lbs)") + 
    xlab("Survey Year") +
    ggtitle("Historic Point Estimates") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    scale_y_continuous(labels = comma, limits = c(0,max(cur_yr_biomass$Mature/1000000, 
                                                        na.rm = TRUE) + 1.5), 
                       breaks= seq(min(0), max(max(cur_yr_biomass$Mature/1000000, 
                                                   na.rm = TRUE)+ 1.5), by = 1.0)) +
    theme(legend.position = c(0.65,0.80), 
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title=element_text(size=14,face="bold"))
  
  ggsave(paste0('./figures/tanner/', cur_yr,'/', cur_yr, '_figureA1_historic_data.png'), dpi = 800,
         width = 8, height = 5.75)

# Figure 2 data prep --------------
# needs to only include region 1 harvest
# unique(harvest$Fishery)
# last 4 years harvest ------
tanner1 <- c('FredSnd/Lwr StephPsg Tanner', 'Icy Strait Tanner Crab', 'Lynn Canal/Upp StephPsg Tanner', 'Other Tanner Crab')
harvest %>% 
  filter(Fishery %in% tanner1) %>% 
  group_by(Year = Batch.Year) %>%
  #filter(Year >= cur_yr-4) %>% 
  summarise(permits = length(unique(CFEC)), 
              numbers = sum(Number.Of.Animals, na.rm = TRUE), 
              pounds = sum(Whole.Weight..sum., na.rm = TRUE)) %>% 
  filter(Year >= 2000) -> annual_harvest_cur
harvest_old %>% 
    filter(Fishery %in% tanner1) %>% 
    group_by(Season) %>%
    summarise(permits = length(unique(CFEC)), 
              numbers = sum(Number.Of.Animals, na.rm = TRUE), 
              pounds = sum(Whole.Weight..sum., na.rm = TRUE)) -> annual_harvest
# add year ----
# need a season reference column in terms of years
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
annual_harvest %>% 
  mutate(Year = as.numeric(numextract(Season))+1) -> annual_harvest

annual_harvest %>% 
  select(-Season) %>% 
  select(Year, permits, numbers, pounds) %>% 
  union(annual_harvest_cur) -> annual_harvest_all
# annual harvest----
# pull from OceanAK now has year so don't need to create it from season.

annual_harvest_all %>% 
  select(Year, pounds) %>% 
  filter(Year > 1991) %>% 
  left_join(std_cpue) -> figure2
# issues with current pull from OceanAK
#annual_harvest_cur %>% 
#  select(Year, pounds) %>% 
#  filter(Year > 1991) %>% 
#  left_join(std_cpue) -> figure2c

# Figure 2a ----
ggplot(figure2, aes(x = Year, y = pounds/1000000)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black") +
  ggtitle("Commercial Tanner crab harvest") +
  ylab("Harvest (1,000,000 lbs)") + 
  xlab(NULL) +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1991),max(cur_yr), by =2)) +
  scale_y_continuous(limits = c(0,max(figure2$pounds/1000000, 
                                                      na.rm = TRUE) + 0.5), 
                     breaks= seq(min(0), max(max(figure2$pounds/1000000, 
                                                 na.rm = TRUE)+ 0.5), by = 1.0)) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2a

# Figure 2b --------------
ggplot(figure2, aes(x = Year, y = avg.cpue)) +
  geom_line(aes(x = Year, y = avg.cpue)) +
  geom_point(aes(x = Year, y = avg.cpue), size =3) +
  geom_ribbon(aes(ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), 
              alpha = 0.2) +
  #geom_errorbar(aes(x = Year, ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), #now displayed as confidence intervals
  #            width = 0.2, na.rm = TRUE) +
  expand_limits(y = 0) +
  ylab("Fishery CPUE (crab per pot)") + 
  xlab("Fishery Year") +
  scale_x_continuous(breaks = seq(min(1991),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0, 40), 
                     breaks= seq(min(0), max(40), by = 10)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2b#+
  #geom_hline(yintercept = mean(figure2$avg.cpue, na.rm = TRUE)) 


plot_grid(fig2a, fig2b, ncol = 1, align = 'v')
ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_figure2.png'), dpi = 800,
       width = 8, height = 9.0)


### biomass vs. harvest annual ------
# biomass is just regional and does not include non-surveyed areas. 
# for simplicity use current % survey/non here...note this does NOT reflect changes to survey areas historically
# i.e. those survey areas that were added or removed.
cur_yr_biomass %>% 
  mutate(Regional_Legal = Legal/.66, Regional_Mature = Mature/0.66) %>% 
  left_join(annual_harvest_all) %>% 
  mutate(hrate = pounds/Regional_Mature*100) -> biomass_harvest

biomass_harvest %>% 
  select(Year, Regional_Mature, harvest = pounds) %>% 
  gather(type, pounds, Regional_Mature:harvest, factor_key = TRUE) %>% 
  ggplot(aes(Year, y = pounds/1000000, group = type)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black")
  
# Old with point estimates Figure 1 ------------
survey_biomass %>% 
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, y = pounds/1000000, group = type)) +
  geom_line(aes(color = type, linetype = type))+
  geom_point(aes(fill = type, shape = type), size =3) +
  scale_fill_manual(name = "", values = c("black", "gray100")) + 
  scale_colour_manual(name = "", values = c("gray1", "grey48"))+
  scale_shape_manual(name = "", values = c(21, 21))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  ylab("Biomass (1,000,000 lbs)") + 
  xlab("Survey Year") +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(survey_biomass$Mature/1000000, 
                                                      na.rm = TRUE) + 1.5), 
                     breaks= seq(min(0), max(max(survey_biomass$Mature/1000000, 
                                                 na.rm = TRUE)+ 1.5), by = 1.0)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

ggsave(paste0('./figures/tanner/', cur_yr,'_figure1.png'), dpi = 800,
       width = 8, height = 5.75)