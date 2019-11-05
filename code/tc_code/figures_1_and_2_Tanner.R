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
# 11/07/2018

# load -----
source('./code/tanner_functions.R')

# data -----
cur_yr <- 2018
#survey_biomass <- read.csv("./data/TCS/survey_areas_biomass.csv") #add to each year
# above file had point estimates from each year and was kept historically in SigmaPlot. Now this is tracked in 
# an appendix table in the stock health document. 
biomass <- read.csv(paste0('./data/rkc_tanner/tanner_', cur_yr, '_biomassmodel.csv'))          
harvest <- read.csv("./data/Tanner_Detailed Fish Tickets_85_18.csv") 
# add current years catch to this file or repull all years
std_cpue <- read.csv("C:/Users/kjpalof/Documents/R projects/tanner-crab/results/std_commericial_cpue.csv")
#calculated in a seperate project "tanner-crab"

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
write.csv(cur_yr_biomass, paste0('./results/TCS/', cur_yr, '/surveyed_areas_total_', cur_yr, '_model.csv'))
# these are listed in Table A1 - Appendix in Tanner crab stock health document

## adjustments using data pre-2007 -------
# first survey year up to 2007 - uses 2007 data
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
# Now is calculated base on the 2018 model output.
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
  scale_y_continuous(labels = comma, limits = c(0,max(cur_yr_biomass$Mature/1000000, 
                                                      na.rm = TRUE) + 1.5), 
                     breaks= seq(min(0), max(max(cur_yr_biomass$Mature/1000000, 
                                                 na.rm = TRUE)+ 1.5), by = 1.0)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

  ggsave(paste0('./figures/tanner/', cur_yr,'_figure1_curyr_data.png'), dpi = 800,
         width = 8, height = 5.75)


# Figure 2 data prep --------------
harvest %>% 
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
  select(Year, pounds) %>% 
  filter(Year > 1991) %>% 
  left_join(std_cpue) -> figure2


# Figure 2a ----
ggplot(figure2, aes(x = Year, y = pounds/1000000)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black") +
  ggtitle("Commercial Tanner crab harvest") +
  ylab("Harvest (1,000,000 lbs)") + 
  xlab(NULL) +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = seq(min(1991),max(cur_yr), by =2)) +
  scale_y_continuous(labels = comma, limits = c(0,max(figure2$pounds/1000000, 
                                                      na.rm = TRUE) + 0.5), 
                     breaks= seq(min(0), max(max(figure2$pounds/1000000, 
                                                 na.rm = TRUE)+ 0.5), by = 0.5)) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2a

# Figure 2b --------------
ggplot(figure2, aes(x = Year, y = avg.cpue)) +
  geom_line(aes(x = Year, y = avg.cpue)) +
  geom_point(aes(x = Year, y = avg.cpue), size =3) +
  geom_errorbar(aes(x = Year, ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), #now displayed as confidence intervals
              width = 0.2, na.rm = TRUE) +
  expand_limits(y = 0) +
  ylab("CPUE (crab per pot))") + 
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
ggsave(paste0('./figures/tanner/', cur_yr,'_figure2.png'), dpi = 800,
       width = 8, height = 9.0)


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