# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Current figures are in SigmaPlot. 
#     Figure 2 is regional biomass from CSA estimate - use current year's model

# K.Palof
# katie.palof@alaska.gov
# 08/03/2018

rm(list = ls())# clear workspace - helpful when jumping between files 
# load -----
source('./code/functions.R')

# data -----
cur_yr <- 2018
mr_adjust <- read.csv('./data/redcrab/adj_final_stock_assessment.csv')
fishery.status <- read.csv('./data/redcrab/Juneau/hind_fore_cast_2018.csv') # has fishery status
#                     may want to save this information somewhere else in the future
biomass <- read.csv("./data/redcrab/biomass.csv") 
# file for all locations.  Has legal and mature biomass from current year CSA & harvest
# mr adjustments can be made in the function using mr_adjust file.
exploit_rate <- read.csv("./data/redcrab/table3.csv")

## clean up figure 2-------
# add in mr adjustments
mr_adjust %>% 
  select(-X) %>% 
  mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
  select(-area) -> mr_adjust2
biomass %>% 
  left_join(mr_adjust2) %>% 
  mutate(adj.legal = ifelse(Location == "Juneau", legal.biomass, 
                            legal.biomass*weighted_ADJ), 
         adj.mature = ifelse(Location == "Juneau", mature.biomass, 
                            mature.biomass*weighted_ADJ))-> biomass
write.csv(biomass, paste0('./results/redcrab/biomass_', cur_yr, '.csv'))
# use these values for Table A2 in stock health document

# regional biomass ----
biomass %>% 
  group_by(Year) %>% 
  summarise(legal = sum(legal.biomass), mature = sum(mature.biomass), 
            adj_legal = sum(adj.legal), adj_mature = sum(adj.mature)) %>% 
  as.data.frame() -> regional.b
fishery.status %>% 
  select(Year = year, status) %>% 
  mutate(status = ifelse(status == "PU only", "closed", as.character(status))) %>% 
  # add next line to deal with current year which is TBD in file but will most 
  # likely be closed in current year (2018)
  mutate(status = ifelse(status == "TBD", "closed", as.character(status)))-> fishery.status.update


regional.b %>% 
  left_join(fishery.status.update) -> regional.b
write.csv(regional.b, paste0('./results/redcrab/regional_biomass_', cur_yr, '.csv'))
# use these values for table A1 in stock health document 

# change in biomass estimation ----
regional.b %>% 
  filter(Year > cur_yr-2) %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) %>% 
  select(-status) %>% 
  spread(key = Year, value = pounds) %>% 
  mutate(change = (`2018`-`2017`)/`2017`) # report these values in stock health doc

# baseline ---
# 1993 - 2007 
regional.b %>% 
  filter(Year >= 1993 & Year <= 2007) %>% 
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature), 
            adj.legal_base = mean(adj_legal), adj.mature.base = mean(adj_mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("CSA Legal (1993-2007)", "CSA Mature (1993-2007)", 
                                 "Legal (1993-2007)", "Mature (1993-2007)")) ->reg_baseline

reg_baseline[1:2, ] ->  reg_baseline_CSA
reg_baseline[3:4, ] ->  reg_baseline_MR
# for graphing
#regional.b %>% 
#  gather(type, pounds, legal:mature, factor_key = TRUE) -> regional.


# Figure 2 TBD regional biomass CSA biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  select(Year, legal, mature, status) %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_CSA, aes(yintercept = pounds, linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "black", "grey60", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "solid", "dashed", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$mature,
                                                na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$mature, na.rm = TRUE) +100000), 
                                 by = 200000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ggtitle("CSA biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
      legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45), 
      axis.title = element_text(size = 14, face = "bold"), 
      axis.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_CSA, aes(x = st_yr, y = pounds, label = label), 
          hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggsave(paste0('./figures/redcrab/CSAregional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


# Figure 2 TBD regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  select(Year, adj_legal, adj_mature, status) %>%
  gather(type, pounds, adj_legal:adj_mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_MR, aes(yintercept = pounds, 
                                         linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$adj_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$adj_mature, na.rm = TRUE) +100000), 
                                 by = 500000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds, label = label), 
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggsave(paste0('./figures/redcrab/MRregional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

# Figure 2 **CLOSED** regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b %>% 
  select(Year, adj_legal, adj_mature, status) %>%
  gather(type, pounds, adj_legal:adj_mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_MR, aes(yintercept = pounds, 
                                         linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b$adj_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b$adj_mature, na.rm = TRUE) +100000), 
                                 by = 500000)) +
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b$Year) + 1), by = 2)) +
  ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds, label = label), 
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  ggsave(paste0('./figures/redcrab/MRregional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)



# clean up tables --------
# equlibrium exploitation rate -----
exploit_rate %>%  # exploitation rats for other areas as weighted means from surveyed areas
  summarise(equ.er.adj = weighted.mean(equ.er.adj, mature.lb.avg), 
            avg.inc.hr = weighted.mean(avg.inc.hr, mature.lb.avg), 
            alt.equ.hr = weighted.mean(alt.equ.hr, mature.lb.avg)) %>% 
  mutate(Location = "other.areas") -> exploit_other

exploit_rate %>% 
  dplyr::select(area, equ.er.adj, avg.inc.hr, alt.equ.hr) %>% 
  mutate(Location = case_when(area == 'pybus' ~ 'Pybus', 
                              area == 'gambier' ~ 'Gambier', 
                              area == 'seymour' ~ 'Seymour', 
                              area == 'peril' ~ 'Peril', 
                              area == 'lynn' ~ 'LynnSisters', 
                              area == 'excursion' ~ 'Excursion', 
                              area == 'juneau' ~ 'Juneau')) %>% 
  dplyr::select(-area) %>% 
  bind_rows(exploit_other) -> equ_rate

mr_adjust %>% 
  select(-X) %>% 
  mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
  select(-area) -> mr_adjust2

# setup blue king crab and other areas 
bkc <- 0.0106
expasion <- 0.628

biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(-harvest) %>%  # add mr_adjust2 so that I can calculate biomass
  # that is expanded from surveyed and adjusted biomass values 
  left_join(mr_adjust2) %>% 
  replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  mutate(legal.adj = legal.biomass*weighted_ADJ, 
         mature.adj = mature.biomass*weighted_ADJ) %>% 
  group_by(Year) %>% 
  summarise(legal.biomass = sum(legal.biomass), mature.biomass = sum(mature.biomass), 
            legal.adj = sum(legal.adj), mature.adj = sum(mature.adj)) %>% 
  gather(type, surveyed, legal.biomass:mature.adj, factor_key = TRUE) %>% 
  mutate(other.areas = surveyed/expasion - surveyed, 
         bkc = surveyed*bkc, 
         total = surveyed + other.areas + bkc) %>% 
  gather(Location, pounds, surveyed:total) %>% 
  cast(Year+Location~type) -> regional_totals

# data frame with biomass, adjusted biomass, er possiblities
biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(-harvest) %>% 
  
  left_join(mr_adjust2) %>%
  replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  mutate(legal.adj = legal.biomass*weighted_ADJ, 
         mature.adj = mature.biomass*weighted_ADJ) %>% 
  select(-weighted_ADJ) %>% 
  bind_rows(regional_totals) %>% 
  left_join(equ_rate) %>% 
  write.csv(paste0('./results/redcrab/regional_', cur_yr, '.csv'), row.names = FALSE) -> biomass_rate

# Table 3 - bioamss, adj, Equ.er.adj -----------
#biomass_rate %>% 
#  mutate(total.GHL = mature.adj*equ.er.adj)
  

