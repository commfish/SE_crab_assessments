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
# 11/07/2018 / 11-15-2020 / 3-2-21 / 11-8-21/ 11-7-22
#agr 10-31-24

cur_yr <- 2024
# load -----
source('./code/tanner_functions.R')
output_path <- paste0('results/tanner/', cur_yr) # output and results
dir.create(output_path) 

# data -----
cur_yr <- 2024
n_yr <- cur_yr + 1
#survey_biomass <- read.csv("./data/TCS/survey_areas_biomass.csv") #add to each year #tk agr what are you talking about????
# above file had point estimates from each year and was kept historically in SigmaPlot. Now this is tracked in 
# an appendix table in the stock health document. 
biomass <- read.csv(paste0('./data/tanner/tanner_', n_yr, '_biomassmodel.csv'))          
harvest_old <- read.csv("./data/harvest/Tanner_Detailed Fish Tickets_85_18.csv")
#harvest <- read.csv(paste0('./data/harvest/Tanner_Detailed Fish Tickets_ALL_years_', cur_yr, '.csv'))
#harvest <- read.csv("./data/harvest/tanner_harvest.csv") # harvest harvest since 2017; add current year's harvest to this file or 
# repull all years
harvest <- read.csv("./data/harvest/tanner_harvest_2024.csv") #ok I think I updated this for 2024? TK agr. This part could def be more streamlined
#ok I THINK I successfully updated the year - maybue use the paste0 current year trick to streamline

std_cpue <- read.csv(paste0("./results/tanner/harvest/", cur_yr, "/std_commercial_cpue", cur_yr, ".csv")) # calculated in tanner_harvest.R
hist_biomass <- read.csv(paste0("./data/tanner/tanner_annual_pt_estimate_historic_", cur_yr-1, ".csv"))
hist_biomass_update <- read.csv(paste0("./data/tanner/tanner_annual_pt_estimate_historic_", cur_yr, ".csv"))
# this is updated in the cur_yr_tanner_draft.Rmd file #tk agr I already ran that so this shouuuuuld be ok....?

# repeat previous year's estimates for Peril Strait, due to lack of survey in 2023
##agr tk not relevant in 24?? skipped. ok run but be careful
biomass_ps_2023 <- biomass %>% filter(Year == 2022 & Area == "Peril Strait") %>% mutate(Year = 2023)
biomass_ps <- rbind(biomass, biomass_ps_2023)
biomass <- biomass_ps #looks like this just adds peril for 2023. Will also have to do for 2025

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

# add adjustments to the totals in years neccessary
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

# add adjustments to the totals in years necessary
year_totals %>% 
  left_join(adjust2) %>% 
  mutate(Legal = ifelse(!is.na(adj_L), Total_L*(1+adj_L), Total_L), 
         Mature = ifelse(!is.na(adj_M), Total_M*(1+adj_M), Total_M)) %>% 
  select(Year, Legal, Mature) -> cur_yr_biomass2

# Figure 1 ------------
# Now is calculated based on the current years model output.
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
  scale_x_continuous(breaks = seq(min(1994),max(cur_yr), by =2)) + #agr changed 1993 to 1994
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
  
# fig 1 with changes made according to regional staff requests
  
fig1_reg <- cur_yr_biomass %>% 
    gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
    ggplot(aes(Year, y = pounds/1000000, group = type)) +
    geom_line(aes(color = type, linetype = type))+
    geom_point(aes(fill = type, shape = type), size =3) +
    scale_fill_manual(name = "", values = c("black", "gray100")) + 
    scale_colour_manual(name = "", values = c("gray1", "grey48"))+
    scale_shape_manual(name = "", values = c(21, 21))+
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    ylab("Biomass (1,000,000 lb)") + 
    xlab("Survey Year") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1994),max(cur_yr), by =2)) + #agr changed 1993 to 1994
    scale_y_continuous(limits = c(0,max(cur_yr_biomass$Mature/1000000, 
                                        na.rm = TRUE) + 1.5), 
                       breaks= seq(min(0), max(max(cur_yr_biomass$Mature/1000000, 
                                                   na.rm = TRUE)+ 1.5), by = 1.0)) +
    theme(legend.position = c(0.65,0.80), 
          legend.text=element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          axis.title=element_text(size=14,face="bold"))
  
ggsave(paste0(here::here(), '/figures/tanner/',  cur_yr,'/', cur_yr,'_figure1_curyr_data_edited.png'), dpi = 800,
         width = 8, height = 5.75)
  


# Figure A1 for appendix --------------
# make sure you update this csv with current year values - or pull from above 
tail(hist_biomass)  #uh so I need 2024 values? AGR. oh damn, that I do. Below fixes it tho, and I had to adjust for consistency
  
tail(hist_biomass_update) #ok... does not align with hist biomass, soemthing is off from whatever creates hist_biomass_updte TK AGR
#do a quick fix below... or find what created hist_biomass_update and fix at the source

  
# these are projected biomass for each end year - old data/years here are NOT updated.
# add current year to this. #TK AGR - add year or something? why not automated?
cur_yr_biomass %>% 
  filter(Year == cur_yr) ->temp1 #%>%
  #rename(legal = Legal, mature= Mature)-> temp1 #agr added to quick fix a bug
hist_biomass %>% #had to do wrangle to fix inconsistency errr0r- agr
  dplyr::rename(Legal = legal, Mature= mature) %>% #fixed right here agr
  rbind(temp1) -> hist_biomass2
# added code to pull in current year from above here and then re-save from the following year

 hist_biomass2 %>% 
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
    ggtitle("Historic Point Estimates (Survey Areas Only)") + #agr 11/27/24 changed title at biologists' request
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1994),max(cur_yr), by =2)) + #1993 in odd years
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
  
  #Figure A2 - added 11/17/24 AGR - Figure A1 but expanded
  #ok step 1- how to expand...
  ###Get expanded df
  hist_biomass_exp <- hist_biomass2 %>%
    mutate(Regional_Legal = Legal/.66, Regional_Mature = Mature/0.66) #expanding to all areas based on our designated expansion factor
  
  ##Graph, same as A1 but with this new dataframe
  hist_biomass_exp %>% 
    gather(type, pounds, Regional_Legal:Regional_Mature, factor_key = TRUE) %>% #idk what this does but following the pattern
    ggplot(aes(Year, y = pounds/1000000, group = type)) +
    geom_line(aes(color = type, linetype = type))+
    geom_point(aes(fill = type, shape = type), size =3) +
    scale_fill_manual(name = "", values = c("black", "gray100")) + 
    scale_colour_manual(name = "", values = c("gray1", "grey48"))+
    scale_shape_manual(name = "", values = c(21, 21))+
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    ylab("Biomass (1,000,000 lbs)") + 
    xlab("Survey Year") +
    ggtitle("Historic Point Estimates (Expanded)") + #agr 11/27/24 changed title at biologists' request
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1994),max(cur_yr), by =2)) + #1993 in odd years
    scale_y_continuous(labels = comma, limits = c(0,max(hist_biomass_exp$Regional_Mature/1000000, 
                                                        na.rm = TRUE) + 1.5), 
                       breaks= seq(min(0), max(max(hist_biomass_exp$Regional_Mature/1000000, 
                                                   na.rm = TRUE)+ 1.5), by = 1.0)) +
    theme(legend.position = c(0.3,0.85), 
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title=element_text(size=14,face="bold"))
  
  ggsave(paste0('./figures/tanner/', cur_yr,'/', cur_yr, '_figureA2_historic_data_expanded.png'), dpi = 800,
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

annual_harvest_all %>% #only up to 2023.... that ok? TK AGR. Now it's up to 2024 cause I pulled from the correct(?) harvest file
  select(Year, pounds) %>% 
  filter(Year > 1991) %>% 
  left_join(std_cpue) -> figure2
# issues with current pull from OceanAK
#annual_harvest_cur %>% 
#  select(Year, pounds) %>% 
#  filter(Year > 1991) %>% 
#  left_join(std_cpue) -> figure2c

# add season label instead of year ---
figure2 %>% 
  mutate(season = paste0(Year-1, "/", Year)) -> figure2s

#breaks = seq(min(1991),max(cur_yr), by =2) #odd year analysis
breaks = seq(min(1992),max(cur_yr), by =2) #even year analysis
b_labels = paste0(breaks-1, "/", substr(breaks, 3, 4)) #hvae to update for every year agr tK (the line above tho)

substr(breaks, 3, 4)

# Figure 2a ----
ggplot(figure2, aes(x = Year, y = pounds/1000000)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black") +
  ggtitle("Commercial Tanner crab harvest") +
  ylab("Harvest (1,000,000 lbs)") + 
  xlab(NULL) +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(breaks = breaks,#change to 1991 in even years tk agr
                     labels = b_labels) + #I added this to make the labels not f-ed up
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
ggplot(figure2s, aes(x = Year, y = avg.cpue)) +
  geom_line(aes(x = Year, y = avg.cpue)) +
  geom_point(aes(x = Year, y = avg.cpue), size =3) +
  geom_ribbon(aes(ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), 
              alpha = 0.2) +
  #geom_errorbar(aes(x = Year, ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), #now displayed as confidence intervals
  #            width = 0.2, na.rm = TRUE) +
  expand_limits(y = 0) +
  ylab("Fishery CPUE (crab per pot)") + 
  xlab("Season") +
  scale_x_continuous(breaks = breaks, #was 1991 in odd year tk agr
                     labels = b_labels) +
  scale_y_continuous(labels = comma, limits = c(0, 40), 
                     breaks= seq(min(0), max(40), by = 10)) +
  theme(legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold")) -> fig2b #+
  #geom_hline(yintercept = mean(figure2$avg.cpue, na.rm = TRUE)) 


plot_grid(fig2a, fig2b, ncol = 1, align="v") #TK agr something looks off, not aligned...
library(patchwork)
fig2a/fig2b 
ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_figure2.png'), dpi = 800,
       width = 8, height = 9.0)

# fig 2 for RIR, changes made according to regional staff requests
fig2a_reg <- ggplot(figure2, aes(x = Year, y = pounds/1000000)) +
  geom_bar(stat = "identity", 
           fill = "grey75", colour = "black") +
  #ggtitle("Commercial Tanner crab harvest") +
  ylab("Harvest (1,000,000 lb)") + 
  xlab(NULL) +
  theme(plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(limits = c(1991, n_yr), breaks = seq(min(1992),max(cur_yr), by =2)) + #1991 in odd years
  scale_y_continuous(limits = c(0,max(figure2$pounds/1000000, 
                                      na.rm = TRUE) + 0.5), 
                     breaks= seq(min(0), max(max(figure2$pounds/1000000, 
                                                 na.rm = TRUE)+ 0.5), by = 1.0)) +
  theme(axis.text.x = element_blank(),
        legend.position = c(0.65,0.80), 
        axis.text = element_text(size = 12),
        #axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

fig2b_reg <- ggplot(figure2s, aes(x = Year, y = avg.cpue)) +
  geom_line(aes(x = Year, y = avg.cpue)) +
  geom_point(aes(x = Year, y = avg.cpue), size =3) +
  geom_ribbon(aes(ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), 
              alpha = 0.2) +
  #geom_errorbar(aes(x = Year, ymin = avg.cpue - 2*se, ymax = avg.cpue + 2*se), #now displayed as confidence intervals
  #            width = 0.2, na.rm = TRUE) +
  expand_limits(y = 0) +
  ylab("Fishery CPUE (crab per pot)") + 
  xlab("Season") +
  scale_x_continuous(limits = c(1991, n_yr), breaks = seq(min(1992),max(cur_yr), by =2), #1991 in odd years
                     labels = b_labels) + #shoot, I need a b_labels for even years too
  scale_y_continuous(labels = comma, limits = c(0, 40), 
                     breaks= seq(min(0), max(40), by = 10)) +
  theme(legend.position = c(0.65,0.80), 
        legend.text=element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title=element_text(size=14,face="bold"))

plot_grid(fig2a_reg, fig2b_reg, ncol = 1, align = 'v')
ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_figure2_edited.png'), dpi = 800,
       width = 8, height = 9.0)
#library(ggalign) #ok well. The plot alignment is messed up, and was messed up last year. Need to figure out ggalign to fix. agr tk
#aligned_plots <- ggalign::align_plots(fig2a, fig2b, align = 'v', axis = 'lr') 

# Biomass vs. harvest annual ------
# biomass is just regional and does not include non-surveyed areas. 
# for simplicity use current % survey/non here...note this does NOT reflect changes to survey areas historically
# i.e. those survey areas that were added or removed.
cur_yr_biomass %>% 
  mutate(Regional_Legal = Legal/.66, Regional_Mature = Mature/0.66) %>% 
  left_join(annual_harvest_all) %>% 
  mutate(hrate = pounds/Regional_Mature*100) -> biomass_harvest

#biomass_harvest %>% 
#  select(Year, Regional_Mature, Regional_Legal, harvest = pounds) %>% 
#  gather(type, pounds, Regional_Mature:harvest, factor_key = TRUE) %>% 
#  ggplot(aes(Year, y = pounds/1000000, group = type)) +
#  geom_line(aes(color = type, linetype = type))+
#  geom_point(aes(fill = type, shape = type), size =3) +
#  scale_fill_manual(name = "", values = c("black", "gray100", "white")) + 
#  scale_colour_manual(name = "", values = c("gray1", "grey48", "grey20"))+
#  scale_shape_manual(name = "", values = c(21, 21, 15))
  
  
#  geom_bar(stat = "identity", 
#           fill = "grey75", colour = "black")

## this version has survey year but this isn't matched with comm harvest year. 
  ## harvest 2021 is really 2020/2021 season #oh, so 2023 harvest is 2022/23 season.. so I'm ok tgen...
#biomass_harvest %>% #does not work- AGR tk, I think it is ok if it does not work
#    select(Year, Regional_Mature, Regional_Legal, harvest = pounds) %>% 
    #gather(type, pounds, Regional_Mature:Regional_Legal, factor_key = TRUE) %>% 
#    ggplot() +
#    geom_line(aes(x = Year, y = Regional_Mature/1000000), stat = "identity", color = "gray48", 
#              linetype = "dashed", size = 1.5) +
#    geom_line(aes(x = Year, y = Regional_Legal/1000000), stat = "identity", color = "black") +
#    geom_point(aes(x = Year, y = Regional_Legal/1000000), stat = "identity", shape = 21, 
#               fill = "black", size = 3) +
#    geom_bar(aes(x=Year, y=harvest/1000000),stat="identity", fill="gray",colour="black") +
#   labs(title= "Southeast Alaska Tanner crab regional biomass (survey and non areas)",
#     x="Survey Year",y="Biomass (1,000,000 lb)") +
 # geom_label(label = "Mature biomass", x = 2002, y = 4.5, color = "gray48") +
#  geom_label(label = "Legal biomass", x = 2002, y = 2.5, color = "black") +
#  geom_label(label = "Commercial harvest", x = 2005, y = 1.25, color = "black", fill = "gray") +
#ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_harvest_regional_bio_survey_yr.png'), dpi = 800,
#       width = 8.5, height = 6.0)

# NEW Figure 1 - regional bio with harvest matching survey year and harvest year - need to lag harvest by one ---------------
annual_harvest_all %>% #tk agr this is fishy. This is why I have only up to 2022 for harvest. I dont understand why we want this lag...
  mutate(Survey_year = Year -1) %>%  ##compare to last years RIR, perhaps
  select(Survey_year, pounds, numbers, permits) -> annual_harvest_all_lag

cur_yr_biomass %>% 
  mutate(Regional_Legal = Legal/.66, Regional_Mature = Mature/0.66, Survey_year = Year) %>% 
  left_join(annual_harvest_all_lag) %>% 
  mutate(hrate = pounds/Regional_Mature*100) -> biomass_harvest2

reg_harvest_comm_catch <- biomass_harvest2 %>% 
  select(Survey_year, Regional_Mature, Regional_Legal, harvest = pounds) %>% 
  #gather(type, pounds, Regional_Mature:Regional_Legal, factor_key = TRUE) %>% 
  ggplot() +
  geom_line(aes(x = Survey_year, y = Regional_Mature/1000000), stat = "identity", color = "gray48", 
            linetype = "dashed", size = 1.5) +
  geom_line(aes(x = Survey_year, y = Regional_Legal/1000000), stat = "identity", color = "black") +
  geom_point(aes(x = Survey_year, y = Regional_Legal/1000000), stat = "identity", shape = 21, 
             fill = "black", size = 3) +
  geom_bar(aes(x=Survey_year, y=harvest/1000000),stat="identity", fill="gray",colour="black") +
  labs(title= "Southeast Alaska Tanner crab regional biomass (survey and non survey areas)",
       x="Survey Year",y="Biomass (1,000,000 lb)") +
  geom_label(label = "Mature biomass", x = 2002, y = 4.5, label.size = NA, color = "gray48") +
  geom_label(label = "Legal biomass", x = 2002, y = 2.65, label.size = NA, color = "black") +
  geom_label(label = "Commercial harvest", x = 2005, y = 1.25, color = "black", fill = "gray") +
  geom_hline(yintercept = 2.3, color = "#D55E00", 
             linetype = "longdash", lwd = 0.75) +
  geom_label(label = "Lower threshold", x = 2013, y = 2.3, label.size = NA, vjust = +1.25, color = "#D55E00")+
  geom_hline(yintercept = 5.5, color = "chartreuse4", 
             linetype = "longdash", lwd = 0.75) +
  geom_label(label = "Upper threshold", x = 2013, y = 5.5, label.size = NA, vjust = +1.15, color = "chartreuse4")+
  scale_y_continuous(limits = c(0,max(biomass_harvest2$Regional_Mature/1000000, 
                                      na.rm = TRUE) + .5), 
                     breaks= seq(min(0), max(max(biomass_harvest2$Regional_Mature/1000000, 
                                                 na.rm = TRUE)+ .5), by = 1.0)) +
  scale_x_continuous(limits = c(1996, cur_yr), breaks = seq(min(1994),max(cur_yr), by =2)) #was 1993 in odd years
  
reg_harvest_comm_catch #tk agr - looks fishy and I dont have the 2023 comm harvest year...? also I need 2024 on x axis

  ggsave(paste0('./figures/tanner/', cur_yr, '/', cur_yr,'_harvest_regional_bio_comm_catch_yr.png'), dpi = 800,
         width = 8.5, height = 6.0)
  
  
  
  
  #Figure A2 edited: added 12/5/24 at Zane's request
  ##Zane wants (1) harvest geom_bar like the updated figure 1, (2) upper and lower threshold lines like figure 1
  harvest <- biomass_harvest2 %>% select(Year, pounds)%>%
    rename(Harvest=pounds)
  
  #hist_biomass_exp_2 <- left_join(hist_biomass_exp, harvest)
  
 gathered<- hist_biomass_exp_2 %>% 
    gather(type, pounds, Regional_Legal:Regional_Mature, factor_key = TRUE)# %>% #idk what this does but following the pattern
   A2<- ggplot(data=gathered,aes(Year, y = pounds/1000000, group = type)) +
    geom_line(aes(color = type, linetype = type))+
    geom_point(aes(fill = type, shape = type), size =3) +
    scale_fill_manual(name = "", values = c("black", "gray100")) + 
    scale_colour_manual(name = "", values = c("gray1", "grey48"))+
    scale_shape_manual(name = "", values = c(21, 21))+
    scale_linetype_manual(name = "", values = c("solid", "dashed")) +
    ylab("Biomass (1,000,000 lbs)") + 
    xlab("Survey Year") +
    ggtitle("Historic point estimates used for annual management (not updated with current year's data)") + 
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1994),max(cur_yr), by =2)) + #1993 in odd years
    scale_y_continuous(labels = comma, limits = c(0,max(hist_biomass_exp$Regional_Mature/1000000, 
                                                        na.rm = TRUE) + 1.5), 
                       breaks= seq(min(0), max(max(hist_biomass_exp$Regional_Mature/1000000, 
                                                   na.rm = TRUE)+ 1.5), by = 1.0)) +
    theme(legend.position = c(0.3,0.85), 
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title=element_text(size=14,face="bold"))
  
  A2.2 <- A2 +   
    geom_bar(data=harvest, aes(x=Year, y=Harvest/1000000),stat="identity", fill="gray",colour="black", inherit.aes = FALSE)+
    geom_label(label = "Commercial harvest", x = 2005, y = 1.25, color = "black", fill = "gray") +
    geom_hline(yintercept = 2.3, color = "#D55E00", 
               linetype = "longdash", lwd = 0.75) +
    geom_label(label = "Lower threshold", x = 2013, y = 2.3, label.size = NA, vjust = +1.25, color = "#D55E00")+
    geom_hline(yintercept = 5.5, color = "chartreuse4", 
               linetype = "longdash", lwd = 0.75) +
    geom_label(label = "Upper threshold", x = 2013, y = 5.5, label.size = NA, vjust = +1.15, color = "chartreuse4")
  
  
  A2.2
  
  ggsave(paste0('./figures/tanner/', cur_yr,'/', cur_yr, '_figureA2_historic_data_expanded_edited.png'), dpi = 800,
         width = 8, height = 5.75)  
  
  
  
  
  
  
# Old with point estimates Figure 1 ------------
##????? #tk agr ok that this does not work I think
#survey_biomass %>% 
#  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
#  ggplot(aes(Year, y = pounds/1000000, group = type)) +
#  geom_line(aes(color = type, linetype = type))+
#  geom_point(aes(fill = type, shape = type), size =3) +
#  scale_fill_manual(name = "", values = c("black", "gray100")) + 
#  scale_colour_manual(name = "", values = c("gray1", "grey48"))+
#  scale_shape_manual(name = "", values = c(21, 21))+
#  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
#  ylab("Biomass (1,000,000 lbs)") + 
#  xlab("Survey Year") +
#  theme(plot.title = element_text(hjust =0.5)) + 
#  scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
#  scale_y_continuous(labels = comma, limits = c(0,max(survey_biomass$Mature/1000000, 
  #                                                    na.rm = TRUE) + 1.5), 
  #                   breaks= seq(min(0), max(max(survey_biomass$Mature/1000000, 
   #                                              na.rm = TRUE)+ 1.5), by = 1.0)) +
  #theme(legend.position = c(0.65,0.80), 
   #     axis.text = element_text(size = 12),
    #    axis.text.x = element_text(angle = 45, vjust = 0.5),
     #   axis.title=element_text(size=14,face="bold"))

#ggsave(paste0('./figures/tanner/', cur_yr,'_figure1.png'), dpi = 800,
#       width = 8, height = 5.75)