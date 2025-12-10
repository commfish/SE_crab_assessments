# K.Palof - now A.Reich runs this
# alex.reich@alaska.gov
# 08/03/2018 / 9-4-19 / 8-30-20/ 8-29-21 / 7-26-22 / 8-23-2023/ 8-20-24 AGR/ 8-15-25 AGR

# notes ----
# This script is a work in progress to develop figures like those currently used to view the 
#     stock health of crab species in Southeast.  Previous figures were in SigmaPlot. 
#     Figure 2 is regional biomass from CSA estimate - use current year's model

cur_yr <- 2025

# load -----
source('./code/functions.R')
dir.create(file.path(paste0('results/rkc/Region1'), cur_yr))

# data -----
cur_yr <- 2025
pry_yr <- cur_yr-1
mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv') 
fishery.status <- read.csv('./data/rkc/Juneau/hind_fore_cast_JNU_current.csv') # has fishery status
#                     may want to save this information somewhere else in the future
biomass <- read.csv("./data/rkc/biomass.csv") 
# file for all locations.  Has legal and mature biomass from current year CSA & harvest
# mr adjustments can be made in the function using mr_adjust file.
exploit_rate <- read.csv("./data/rkc/table3.csv") #where did thia come from- AGR 25. Copying over from last year, idk if right....FLAG!

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
write.csv(biomass, paste0('./results/rkc/Region1/', cur_yr, '/biomass_', cur_yr, '.csv'))
# use these values for Table A2 in stock health document

# regional biomass ----
biomass %>% 
  group_by(Year) %>% 
  summarise(legal = sum(legal.biomass), mature = sum(mature.biomass), 
            adj_legal = sum(adj.legal), adj_mature = sum(adj.mature)) %>% 
  as.data.frame() -> regional.b
fishery.status %>% 
  select(Year = year, status) %>% 
  mutate(status = ifelse(status == "PU only", "closed", as.character(status))) -> fishery.status.update
  # add next line to deal with current year which is TBD in file but will most 
  # likely be closed in current year (2018)
  # %>% mutate(status = ifelse(status == "TBD", "closed", as.character(status))) -> fishery.status.update
   
regional.b %>% 
  left_join(fishery.status.update) -> regional.b
write.csv(regional.b, paste0('./results/rkc/Region1/', cur_yr, '/regional_biomass_', cur_yr, '.csv'), 
          row.names = FALSE)
# use these values for table A1 in stock health document 

# percent change in biomass since 2017 ----
percent_change <- regional.b %>%
  mutate(leg_pct_change = (adj_legal/lag(adj_legal) - 1) * 100,
         mat_pct_change = (adj_mature/lag(adj_mature) - 1) * 100) %>%
  # calculate average percent change since 2017
  filter(Year > 2017) %>%
  summarise(mean_leg_pct_change = mean(leg_pct_change), mean_mat_pct_change = mean(mat_pct_change))

write.csv(percent_change, paste0('./results/rkc/Region1/', cur_yr, '/percent_biomass_change_2017_', cur_yr, '.csv'), 
          row.names = FALSE)

# change in biomass estimation ----
regional.b %>% 
  filter(Year > cur_yr-2) %>% 
  gather(type, pounds, legal:adj_mature, factor_key = TRUE) %>% 
  select(-status) %>% 
  spread(key = Year, value = pounds) %>% 
  # Update annually ###
  mutate(change = 100*(`2025`-`2024`)/`2024`) -> change # report these values in stock health doc #UPDATE #'s each year??
write.csv(change, paste0('./results/rkc/Region1/', cur_yr, '/change_in_modeled_regional_biomass_', cur_yr, '.csv'), 
          row.names = FALSE)
# these values go in regional overview section, other values from last years forecast
#     come from excel sheet "Figure 1, table 2019"

biomass %>% 
  filter(Year > cur_yr-2) %>% 
  select(-harvest, -weighted_ADJ) %>% 
  gather(type, pounds, legal.biomass:adj.mature, factor_key = TRUE) %>% 
  spread(key = Year, value = pounds) %>% 
  mutate(change = 100*(`2025`-`2024`)/`2024`) -> change2 #update annually AGR
write.csv(change2, paste0('./results/rkc/Region1/', cur_yr, '/change_in_modeled_area_biomasses_', cur_yr, '.csv'))
#

# baseline ---
# 1995 - 2007 
regional.b %>% 
  filter(Year >= 1995 & Year <= 2007) %>% 
  summarise(legal_baseline = mean(legal), mature_baseline = mean(mature), 
            adj.legal_base = mean(adj_legal), adj.mature.base = mean(adj_mature)) %>% 
  as.data.frame() %>% 
  gather(type, pounds, factor_key = TRUE) %>% 
  mutate(st_yr = 2007, label = c("CSA Legal (1995-2007)", "CSA Mature (1995-2007)",  #agr updated 1993 to 1995
                                 "Legal (1995-2007)", "Mature (1995-2007)")) -> reg_baseline

reg_baseline[1:2, ] ->  reg_baseline_CSA
reg_baseline[3:4, ] ->  reg_baseline_MR
# for graphing
#regional.b %>% 
#  gather(type, pounds, legal:mature, factor_key = TRUE) -> regional.


# Figure 2 TBD regional biomass CSA biomass---------
# should have 2018 model with longterm baselines (1995-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
regional.b.2 <- regional.b
temp1 <- length(regional.b$Year)
regional.b.2[temp1, 6] <- "TBD"

#this graph is being difficult, I think I'll need it llater AGR 25 tk
regional.b.2 %>% 
  select(Year, legal, mature, status) %>% 
  gather(type, pounds, legal:mature, factor_key = TRUE) %>% 
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_CSA, aes(yintercept = pounds, linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "black", "grey60", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(0, 1, 8))+
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
          hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) -> fig2_regional_biomass

ggsave(fig2_regional_biomass, filename = paste0('./figures/rkc/', cur_yr, '/CSAregional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

#CSA but expanded

# Figure 2 TBD regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1995-2007) and closure status. #agr we use 1995-2007 as the baseline now 2024 TK
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
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) -> fig2_regional_mr_biomass

ggsave(fig2_regional_mr_biomass, filename = paste0('./figures/rkc/', cur_yr, '/MRregional_biomass', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

# Figure 2 **CLOSED** regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. #AGR 1995-2007 baseline now
#   also show 2018 forecast as distinct from model output


regional.b %>% 
  select(Year, adj_legal, adj_mature, status) %>%
  gather(type, pounds, adj_legal:adj_mature, factor_key = TRUE) %>%
  mutate(status = replace(status, which(status == "TBD"), "closed")) %>% # can replace the TBD with open or closed
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
  #ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") +
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 14), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds, label = label), 
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) -> fig2_closed_regional_mr_biomass

ggsave(fig2_closed_regional_mr_biomass, filename = paste0('./figures/rkc/', cur_yr, '/MRregional_biomass2_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

# Figure 2 **CLOSED** EXPANDED regional biomass M/R adjusted biomass---------
# should have 2018 model with longterm baselines (1993-2007) and closure status. 
#   also show 2018 forecast as distinct from model output
expansion <- 0.528
regional.b %>% 
  mutate(expanded_legal = adj_legal/expansion, 
         expanded_mature = adj_mature/expansion) -> regional.b.expand
#AGR add 2025. Add TBD in there.WILL NEED TO CHANGE WHEN WE DECIDE TO OPEN OR CLOSE THE FISHERY
temp <- length(regional.b.expand$Year)
regional.b.expand[temp, 6] <- "TBD"


write.csv(regional.b.expand, paste0('./results/rkc/Region1/', cur_yr, '/regional_biomass_', cur_yr, '.csv'), 
          row.names = FALSE)

regional.b.expand %>% 
  select(Year, expanded_legal, expanded_mature, status) %>%
  gather(type, pounds, expanded_legal:expanded_mature, factor_key = TRUE) %>%
  mutate(status = replace(status, which(status == "TBD"), "closed")) %>% # can replace the TBD with open or closed
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  geom_point(aes(colour = type, shape = status, fill = type), size =3) +
  geom_hline(data = reg_baseline_MR, aes(yintercept = pounds/expansion, 
                                         linetype = type, colour = type)) +
  scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        guide = FALSE) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b.expand$expanded_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b.expand$expanded_mature, na.rm = TRUE) +100000), 
                                 by = 1000000)) +
  scale_x_continuous(breaks = seq(min(1976),max(max(regional.b.expand$Year)), by = 2)) + #seq(min(1975)) ...# max(max(regional.b.expand$Year)+ 1
  #ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  ggtitle("Regional biomass estimates for Southeast Alaska red king crab") +
  theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 14), axis.text.x = element_text(angle = 45), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds/expansion, label = label), 
            hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 3.5) -> fig2_expand_mr_regional_biomass

ggsave(fig2_expand_mr_regional_biomass, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_MRregional_biomass2_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

# version of the figure with edits requested by regional staff AGR 25 updated
reg_baseline_MR$pounds_expanded <- reg_baseline_MR$pounds/expansion

fig2_expand_mr_regional_biomass_edited <- regional.b.expand %>% 
  select(Year, expanded_legal, expanded_mature, status) %>%
  mutate(status = case_when(
    status == "open" ~ "Open",
    status == "closed" ~ "Closed",
    TRUE ~ status
  )) %>%
  gather(type, pounds, expanded_legal:expanded_mature, factor_key = TRUE) %>%
  #mutate(status = replace(status, which(status == "TBD"), "Closed")) %>% # can replace the TBD with open or closed - EDIT AFTER DECISION!!!
  dplyr::rename(`Fishery Status` = status) %>%
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(colour = type, group = type, linetype = type))+
  #geom_point(aes(colour = type, shape = `Fishery Status`, fill = type), size =3) +
  geom_point(aes(colour = type, shape = `Fishery Status`), size =3) +
  geom_hline(data = reg_baseline_MR, aes(yintercept = pounds/expansion, 
                                         linetype = type, colour = type)) +
  #scale_colour_manual(name = "", values = c("black", "grey60", "black", "grey60"), 
                      #guide = FALSE)+
  scale_colour_manual(name = "", values = c("black", "grey60"))+
  #scale_shape_manual(name = "Fishery Status", values = c(25, 21, 8))+
  #scale_shape_manual(values = c(25, 21, 8))+
  scale_shape_manual(values = c(16, 0, 2))+
  #scale_linetype_manual(name = "", values = c("solid", "dashed", "solid", "dashed"), 
                        #guide = FALSE) +
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_fill_manual(name = "", values = c("black", "gray75"), 
                    guide = FALSE) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b.expand$expanded_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b.expand$expanded_mature, na.rm = TRUE) +100000), 
                                 by = 1000000)) +
  #scale_x_continuous(breaks = seq(min(1975),max(max(regional.b.expand$Year)), by = 2)) + #even years
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b.expand$Year) + 1), by = 2)) + #odd years
  #ggtitle("Biomass of surveyed areas for Southeast Alaska red king crab") + 
  ylab("Biomass (lb)") + 
  #ggtitle("Regional biomass estimates for Southeast Alaska red king crab") +
  #theme(plot.title = element_text(hjust =0.5)) +
  theme(legend.position = c(0.825,0.793), legend.title = element_text(size = 14), 
        legend.text = element_text(size = 14), axis.text.x = element_text(angle = 90), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 14)) +
  theme(axis.text.x = element_text(vjust = 0.50)) +
  #geom_text(data = reg_baseline_MR, aes(x = st_yr, y = pounds/expansion, label = label), 
            #hjust = -0.05, vjust = 1.5, nudge_y = 0.05, size = 4)
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2)))



fig2_expand_mr_regional_biomass_edited <- regional.b.expand %>% #EYES HERE! THIS ONE! 25
  select(Year, expanded_legal, expanded_mature, status) %>%
  mutate(status = case_when(
    status == "open" ~ "Open",
    status == "closed" ~ "Closed",
    TRUE ~ status
  )) %>%
  dplyr::rename("Legal" = expanded_legal, "Mature" = expanded_mature) %>%
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>%
  ##BELOW  LINE NEEDS MANUAL EDIT IN ACCORDANCE WITH WHEN YOU RUN THIS CODE!! TURN THE 
  ##FOLLOWING MUTATE LINE OFF WHEN RUNNING THIS FOR THE STOCK ASSESSMENT. tURN BACK ON AND RUN AFTER THE CRAB TEAM
  ##MAKES  A dECISION! FLAG FLAG FLAg
  #mutate(status = replace(status, which(status == "TBD"), "Open")) %>% # can replace the TBD with open or closed #TUrN ON AFTER WE DECIDE!!!- EDIT YEARLY
  dplyr::rename(`Fishery Status` = status) %>%
  ggplot(aes(Year, pounds, group = type)) +
  geom_line(aes(color = type, group = type, linetype = type))+
  geom_point(aes(color = type, shape = `Fishery Status`), size =3) +
  scale_colour_manual(name = "", values = c("black", "grey60"))+
  scale_shape_manual(values = c(0, 16, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b.expand$expanded_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b.expand$expanded_mature, na.rm = TRUE) +100000), 
                                 by = 1000000)) +
  #ggtitle(paste0("Juneau ", cur_yr," model")) + 
  ylab("Biomass (lb)")+ xlab("Year") +
  #scale_x_continuous(breaks = seq(min(1976),max(max(regional.b.expand$Year)+1), by = 2)) + #even years
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b.expand$Year) + 1), by = 2)) + #odd years
  #theme(plot.title = element_text(hjust =0.5)) +
  #scale_x_continuous(breaks = seq(min(1975),max(cur_yr), by = 5)) +
  geom_hline(yintercept =  reg_baseline_MR$pounds_expanded[1], color = "grey1")+
  geom_hline(yintercept = reg_baseline_MR$pounds_expanded[2], color = "grey60", linetype = "dashed") +
  theme(legend.position = c(0.9,0.75), legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11), axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90)) +
  #geom_text(data = baseline_mean_curyr, aes(x = start_yr, y = baseline, label = label), 
  #hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 4) +
  guides(shape = guide_legend(ncol = 1), group = guide_legend((ncol = 1)))

#CHOOSE THE SAVE OPTION BELOW BASED ON IF ITS BEFORE OR AFTER THE CRAB TEAM MADE THE OPEN CLOSE DECISION!!
ggsave(fig2_expand_mr_regional_biomass_edited, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_MRregional_biomass2_', cur_yr, '_edited.png'), dpi = 800, width = 7.5, height = 5.5)
#ggsave(fig2_expand_mr_regional_biomass_edited, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_MRregional_biomass2_', cur_yr, '_edited_POST_DECISION.png'), dpi = 800, width = 7.5, height = 5.5)


###AGR December 2025 - creating the above fig but after they make a fishery decision. This one is for use in publication, 
##because the decision will have been made by then...



#AGR 2025- the fig2_expand_mr_regional_biomass_edited - with medians!
##what do we care about??
###the long-term median
####long term mean is 1995-2007??
####how we define the long term median
###50% of the logn term median
#fig2_expand_mr_regional_biomass_edited +geom_hline()

med_calc_95 <- regional.b.expand %>% filter(Year > 1994 & Year < 2008)
median_95 <- median(med_calc_95$expanded_legal)
half_med_95 <- median_95/2
med_calc_all <- regional.b.expand %>% filter(Year < cur_yr)
median_all <- median(med_calc_all$expanded_legal)
half_med_all <- median_all/2

med1 <- fig2_expand_mr_regional_biomass_edited +
  geom_hline(yintercept=median_95, color="orange", linetype = "dotdash", size=1) +
  geom_hline(yintercept=half_med_95, color="orange", linetype = "dotted", size=1)+
  labs(title = "Median 1995-2007")

med2 <- fig2_expand_mr_regional_biomass_edited +
  geom_hline(yintercept=median_all, color="purple", linetype = "dotdash", size=1) +
  geom_hline(yintercept=half_med_all, color="purple", linetype = "dotted", size=1) +
  labs(title = "Median 1977-2024")

ggsave(med1, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_biomass_with_1995_2007_median', cur_yr, '_edited.png'), dpi = 800, width = 7.5, height = 5.5)
ggsave(med2, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_biomass_with_1977_2024_median', cur_yr, '_edited.png'), dpi = 800, width = 7.5, height = 5.5)

##a cleaner median graph AGR 25
fig_median_simple <- regional.b.expand %>% 
  select(Year, expanded_legal, status) %>%
  mutate(status = case_when(
    status == "open" ~ "Open",
    status == "closed" ~ "Closed",
    TRUE ~ status
  )) %>%
  dplyr::rename("Legal" = expanded_legal) %>%
  #gather(type, pounds, Legal:Mature, factor_key = TRUE) %>%
  #mutate(status = replace(status, which(status == "TBD"), "Closed")) %>% # can replace the TBD with open or closed #TUrN ON AFTER WE DECIDE!!!
  dplyr::rename(`Fishery Status` = status) %>%
  ggplot(aes(Year, Legal)) +
  geom_line()+
  geom_point(aes(shape = `Fishery Status`), size =3) +
  #scale_colour_manual(name = "", values = c("black", "grey60"))+
  scale_shape_manual(values = c(0, 16, 8))+
  #scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,(max(regional.b.expand$expanded_mature,
                                                       na.rm = TRUE) + 100000)),
                     breaks= seq(min(0), max(max(regional.b.expand$expanded_mature, na.rm = TRUE) +100000), 
                                 by = 1000000)) +
  #ggtitle(paste0("Juneau ", cur_yr," model")) + 
  ylab("Legal biomass (lb)")+ xlab("Year") +
  #scale_x_continuous(breaks = seq(min(1976),max(max(regional.b.expand$Year)+1), by = 2)) + #even years
  scale_x_continuous(breaks = seq(min(1975),max(max(regional.b.expand$Year) + 1), by = 2)) + #odd years
  #theme(plot.title = element_text(hjust =0.5)) +
  #scale_x_continuous(breaks = seq(min(1975),max(cur_yr), by = 5)) +
  #geom_hline(yintercept =  reg_baseline_MR$pounds_expanded[1], color = "grey1")+
  #geom_hline(yintercept = reg_baseline_MR$pounds_expanded[2], color = "grey60", linetype = "dashed") +
  theme(legend.position = c(0.9,0.75), legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11), axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90)) +
  #geom_text(data = baseline_mean_curyr, aes(x = start_yr, y = baseline, label = label), 
  #hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 4) +
  guides(shape = guide_legend(ncol = 1), group = guide_legend((ncol = 1)))+
  geom_hline(yintercept=median_95, color="orange", linetype = "dotdash", size=1) +
  geom_hline(yintercept=half_med_95, color="orange", linetype = "dotted", size=1)

ggsave(fig_median_simple, filename = paste0('./figures/rkc/', cur_yr, '/Expanded_biomass_with_1995_2007_median_simple', cur_yr, '_edited.png'), dpi = 800, width = 7.5, height = 5.5)


# clean up tables --------
# equlibrium exploitation rate -----
exploit_rate %>%  # exploitation rats for other areas as weighted means from surveyed areas
  summarise(equ.er.adj = round(weighted.mean(equ.er.adj, mature.lb.avg),2), 
            avg.inc.hr = round(weighted.mean(avg.inc.hr, mature.lb.avg), 2), 
            alt.equ.hr = round(weighted.mean(alt.equ.hr, mature.lb.avg), 2)) %>% 
  mutate(Location = "other.areas") -> exploit_other

equ_rate <- exploit_rate %>% 
  dplyr::select(area, equ.er.adj, avg.inc.hr, alt.equ.hr) %>% 
  #mutate(Location = case_when(area == 'pybus' ~ 'Pybus', 
  #                            area == 'gambier' ~ 'Gambier', 
  #                            area == 'seymour' ~ 'Seymour', 
  #                            area == 'peril' ~ 'Peril', 
  #                            area == 'lynn' ~ 'LynnSisters', 
  #                            area == 'excursion' ~ 'Excursion', 
  #                            area == 'juneau' ~ 'Juneau')) %>% 
  mutate(Location = area, 
         equ.er.adj = round(equ.er.adj, 2), avg.inc.hr = round(avg.inc.hr, 2), 
         alt.equ.hr = round(alt.equ.hr, 2)) %>% 
  dplyr::select(Location, equ.er.adj, avg.inc.hr, alt.equ.hr) %>% 
  bind_rows(exploit_other) %>% 
  mutate(hr_cur_yr = case_when(
    Location == "Juneau" ~ 0.10, # Juneau area HR set in July, UPDATE in each year
    Location == "other.areas" ~ 0.10,
    .default = 0
  ))

mr_adjust %>% 
  select(-X) %>% 
  mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
  select(-area) -> mr_adjust2

# setup blue king crab and other areas 
bkc <- 0.0106
expasion <- 0.528

biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(Year, Location, adj.legal, adj.mature) %>%  # add mr_adjust2 so that I can calculate biomass
  # that is expanded from surveyed and adjusted biomass values 
  #left_join(mr_adjust2) %>% 
  #replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  #mutate(legal.adj = legal.biomass*weighted_ADJ, 
  #       mature.adj = mature.biomass*weighted_ADJ) %>% 
  group_by(Year) %>% 
  summarise(adj.legal = sum(adj.legal), adj.mature = sum(adj.mature)) %>% 
  gather(type, surveyed, adj.legal:adj.mature, factor_key = TRUE) %>% 
  mutate(other.areas = surveyed/expasion - surveyed, 
         bkc = surveyed*bkc, 
         total = surveyed + other.areas + bkc) %>% 
  gather(Location, pounds, surveyed:total) %>% 
  cast(Year+Location~type) -> regional_totals

# data frame with biomass, adjusted biomass, er possiblities
biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(Year, Location, adj.legal, adj.mature) %>% 
  #left_join(mr_adjust2) %>%
  #replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  #mutate(legal.adj = legal.biomass*weighted_ADJ, 
  #       mature.adj = mature.biomass*weighted_ADJ) %>% 
  #select(-weighted_ADJ) %>% 
  bind_rows(regional_totals) %>% 
  left_join(equ_rate) -> biomass_rate
write.csv(biomass_rate, paste0('./results/rkc/Region1/', cur_yr, '/regional_', cur_yr, '.csv'), row.names = FALSE) 

# Table 2 to 4 - bioamss, adj, Equ.er.adj -----------
survey.locations <- c("Pybus", "Excursion", "Gambier", "Juneau", 
                      "LynnSisters", "Peril", "Seymour") #um what about no peril this year, how do we cope? What did C do in 2023? AGR flag 25
# Table 2 -- #TK AGR - error here, total PU is wrong. Also, use legal to estimate GHL, also. Maybe make this a new table- table 2.5
biomass_rate %>% 
  #mutate(Equilibrium.HR = ifelse(Location == "Juneau", hr_cur_yr, equ.er.adj)) %>% 
  mutate(Equilibrium.HR = equ.er.adj) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.mature*Equilibrium.HR, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                  1000, 0)), 0), 
         Comm.GHL = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                           ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL = ifelse(Location == "bkc", bkc_temp, Comm.GHL), 
         total_temp = sum(Comm.GHL, na.rm = TRUE), 
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL)) %>% 
  dplyr::select(-bkc_temp, - total_temp) -> table2_csv
write.csv(table2_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table2_regional_', cur_yr, '.csv'), row.names = FALSE) 

# Table 2.5 -- #AGR's creation to add the legal GHL in there, maybe add as new table in doc?
#biomass_rate %>% 
  #mutate(Equilibrium.HR = ifelse(Location == "Juneau", hr_cur_yr, equ.er.adj)) %>% 
 # mutate(Equilibrium.HR = equ.er.adj) %>% 
  #dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  #mutate(GHL_mat = round(adj.mature*Equilibrium.HR, 0), 
   #      GHL_leg = round(adj.legal*Equilibrium.HR, 0), 
    #     Legal.HR = round(GHL/adj.legal, 2), 
     #    PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
      #                                                                  1000, 0)), 0), 
       #  Comm.GHL.mat = ifelse(Location == "Juneau"|Location == "other.areas", (GHL_mat - PU.catch),
        #                   ifelse(Legal.HR <= 0.40, GHL, 0)), 
  #       Comm.GHL.leg = ifelse(Location == "Juneau"|Location == "other.areas", (GHL_leg - PU.catch),
   #                            ifelse(Legal.HR <= 0.40, GHL, 0)),
    #     bkc_temp = round(sum(Comm.GHL[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
     #    Comm.GHL.mat = ifelse(Location == "bkc", bkc_temp, Comm.GHL.mat),
      #   Comm.GHL.leg = ifelse(Location == "bkc", bkc_temp, Comm.GHL.leg),
       #  total_temp = sum(Comm.GHL, na.rm = TRUE), 
        # Comm.GHL.mat = ifelse(Location == "total", total_temp, Comm.GHL.mat),
         #Comm.GHL.leg = ifelse(Location == "total", total_temp, Comm.GHL.leg)
         #) %>% 
  #dplyr::select(-bkc_temp, - total_temp) -> table2.5_csv
#write.csv(table2_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table2.5_regional_', cur_yr, '.csv'), row.names = FALSE) 

#table 2.6 - I gave up on above and just replaceing mature with the legal biomass calcs
biomass_rate %>% 
  #mutate(Equilibrium.HR = ifelse(Location == "Juneau", hr_cur_yr, equ.er.adj)) %>% 
  mutate(Equilibrium.HR = equ.er.adj) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.legal*Equilibrium.HR, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                        1000, 0)), 0), 
         Comm.GHL = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                           ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL = ifelse(Location == "bkc", bkc_temp, Comm.GHL), 
         total_temp = sum(Comm.GHL, na.rm = TRUE), 
         total_temp2 = sum(GHL, na.rm = TRUE),
         total_temp3 = sum(PU.catch),
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL),
         GHL = ifelse(Location == "total", total_temp2, GHL),
         PU.catch = ifelse(Location == "total", total_temp3, PU.catch)
         ) %>% 
  dplyr::select(-bkc_temp, - total_temp, -total_temp2, -total_temp3) -> table2.6_csv
write.csv(table2.6_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table2.6_regional_', cur_yr, '.csv'), row.names = FALSE) 


#9/6/24 add - we want table 2.6 BUT juneau HR needs to be 0.08
#table 2.7 - 
biomass_rate_J <- biomass_rate %>%
  mutate(
  equ.er.adj = ifelse(Location=="Juneau", 0.10, equ.er.adj), #adding in the Juneau harvset rate for 2024, now updated for 2025
  avg.inc.hr = ifelse(Location=="Juneau", 0.10, avg.inc.hr)
  )
  #biomass_rate$equ.er.adj #get these values and copy-pasted below
  #mutate(equ.er.adj.J = c(0.12, 0.04, 0.01, 0.04, 0.17 0.09 0.06   NA 0.10   NA   NA))

biomass_rate_J %>% 
  #mutate(Equilibrium.HR = ifelse(Location == "Juneau", hr_cur_yr, equ.er.adj)) %>% 
  mutate(Equilibrium.HR = equ.er.adj) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.legal*Equilibrium.HR, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                        1000, 0)), 0), 
         Comm.GHL = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                           ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL = ifelse(Location == "bkc", bkc_temp, Comm.GHL), 
         total_temp = sum(Comm.GHL, na.rm = TRUE), 
         total_temp2 = sum(GHL, na.rm = TRUE),
         total_temp3 = sum(PU.catch),
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL),
         GHL = ifelse(Location == "total", total_temp2, GHL),
         PU.catch = ifelse(Location == "total", total_temp3, PU.catch)
  ) %>% 
  dplyr::select(-bkc_temp, - total_temp, -total_temp2, -total_temp3) -> table2.7_csv
write.csv(table2.7_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table2.7_regional_', cur_yr, '.csv'), row.names = FALSE) 

# Table 3 --
table3_csv <- biomass_rate %>% 
  mutate(Avg.Inc.HR = ifelse(Location == "Juneau", hr_cur_yr, 
                             ifelse(Location == "Seymour", 0.005, avg.inc.hr))) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.mature*Avg.Inc.HR, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                        1000, 0)), 0), 
         Comm.GHL1 = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                           ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL1[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL2 = ifelse(Location == "bkc", bkc_temp, Comm.GHL1), 
         total_temp = sum(Comm.GHL2, na.rm = TRUE), 
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL2)) %>% 
          dplyr::select(-bkc_temp, - total_temp, -Comm.GHL1, -Comm.GHL2)
write.csv(table3_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table3_regional_', cur_yr, '.csv'), row.names = FALSE) 

#table 3.6 - an agr add to use legal insetead of mature biomass for GHL calcs
table3.6_csv <- biomass_rate %>% 
  mutate(Avg.Inc.HR = ifelse(Location == "Juneau", hr_cur_yr, #adds the current harvest rate for juneau
                             ifelse(Location == "Seymour", 0.005, avg.inc.hr))) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.legal*Avg.Inc.HR, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                        1000, 0)), 0), 
         Comm.GHL1 = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                            ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL1[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL2 = ifelse(Location == "bkc", bkc_temp, Comm.GHL1), 
         total_temp = sum(Comm.GHL2, na.rm = TRUE), 
         total_temp2 =sum(GHL, na.rm = TRUE),
         total_temp3 = sum(PU.catch, na.rm = TRUE),
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL2),
         GHL = ifelse(Location == "total", total_temp2, GHL),
         PU.catch = ifelse(Location == "total", total_temp3, PU.catch)
         ) %>% 
  dplyr::select(-bkc_temp, - total_temp, -total_temp2, -total_temp3, -Comm.GHL1, -Comm.GHL2)
write.csv(table3.6_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table3.6_regional_', cur_yr, '.csv'), row.names = FALSE) 


# Table X -- table 4 removed from 2021 doc.
biomass_rate %>% 
  mutate(Avg.HR_Avg.change = ifelse(Location == "Juneau", hr_cur_yr, 
                             ifelse(Location == "Seymour", 0.04, alt.equ.hr))) %>% 
  dplyr::select(-avg.inc.hr, -alt.equ.hr, -equ.er.adj, -hr_cur_yr) %>% 
  mutate(GHL = round(adj.mature*Avg.HR_Avg.change, 0), 
         Legal.HR = round(GHL/adj.legal, 2), 
         PU.catch = round(ifelse(Location == "Juneau", (GHL*.6), ifelse(Location == "other.areas", 
                                                                        1000, 0)), 0), 
         Comm.GHL = ifelse(Location == "Juneau"|Location == "other.areas", (GHL - PU.catch),
                           ifelse(Legal.HR <= 0.40, GHL, 0)), 
         bkc_temp = round(sum(Comm.GHL[Location%in%survey.locations], na.rm = TRUE)*0.0106, 0), 
         Comm.GHL = ifelse(Location == "bkc", bkc_temp, Comm.GHL), 
         total_temp = sum(Comm.GHL, na.rm = TRUE), 
         Comm.GHL = ifelse(Location == "total", total_temp, Comm.GHL)) %>% 
  dplyr::select(-bkc_temp, - total_temp) -> table4_csv
write.csv(table4_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table4_regional_', cur_yr, '.csv'), row.names = FALSE) 

# Table A3 -------
biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(-harvest) %>%  # add mr_adjust2 so that I can calculate biomass
  # that is expanded from surveyed and adjusted biomass values 
  #left_join(mr_adjust2) %>% 
  #replace_na(list(legal.biomass = 0, mature.biomass = 0, weighted_ADJ = 1)) %>% 
  #mutate(legal.adj = legal.biomass*weighted_ADJ, 
  #       mature.adj = mature.biomass*weighted_ADJ) %>% 
  group_by(Year) %>% 
  summarise(legal.biomass = sum(legal.biomass), 
            mature.biomass = sum(mature.biomass), 
            adj.legal = sum(adj.legal), adj.mature = sum(adj.mature)) %>% 
  gather(type, surveyed, legal.biomass:adj.mature, factor_key = TRUE) %>% 
  mutate(other.areas = surveyed/expasion - surveyed, 
         bkc = surveyed*bkc, 
         total = surveyed + other.areas + bkc) %>% 
  gather(Location, pounds, surveyed:total) %>% 
  cast(Year+Location~type) -> regional_totals2

biomass %>% 
  filter(Year == cur_yr) %>% 
  dplyr::select(Year, Location, legal.biomass, mature.biomass, weighted_ADJ, adj.legal, adj.mature) %>% 
  mutate(weighted_ADJ = round(weighted_ADJ, 2)) %>% 
  bind_rows(regional_totals2) %>% 
  mutate_at(3:4, round, 0) %>% 
  mutate_at(6:7, round, 0) -> tableA3
write.csv(tableA3, paste0('./results/rkc/Region1/', cur_yr, '/TableA3_regional_', cur_yr, '.csv'), row.names = FALSE) 


# Table 7 RIR ---------
# raw sample sizes
#cur_yr <- 2021

files <- c(paste0(here::here(),"/results/rkc/Pybus/", cur_yr, "/raw_sample.csv"), 
           paste0(here::here(),"/results/rkc/Excursion/", cur_yr, "/raw_sample.csv"),
           paste0(here::here(), "/results/rkc/Gambier/", cur_yr, "/raw_sample.csv"), 
           paste0(here::here(), "/results/rkc/Juneau/", cur_yr, "/raw_sample.csv"), 
           paste0(here::here(), "/results/rkc/LynnSisters/", cur_yr, "/raw_sample.csv"), 
          # paste0(here::here(), "/results/rkc/Peril/", cur_yr, "/raw_sample.csv"), #off in no peril years???
           paste0(here::here(), "/results/rkc/Seymour/", cur_yr, "/raw_sample.csv"))

#files <- files[2:7] #AGR here!
raw_samp <- files %>%
  map(read.csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind)        # reduce with rbind into one dataframe
raw_samp

raw_samp %>% 
  filter(Year == cur_yr) %>%
  select(Year, Location, effective_no_pots) %>% 
  filter(Location != "Barlow Cove") %>% 
  gather(recruit.class, numbers, effective_no_pots, factor_key = TRUE) %>% 
  spread(key = Location, value = numbers)-> effect_pots

raw_samp %>% 
  filter(Year == cur_yr) %>% 
  select(Year, Location, Juvenile, Small.Females, 
         Large.Females, Pre_Recruit, Recruit, Post_Recruit) %>% 
  gather(recruit.class, numbers, Juvenile:Post_Recruit, factor_key = TRUE) %>% 
  spread(key = Location, value = numbers) %>%#-> test # %>% 
  mutate(Juneau = `Barlow Cove` + `Juneau`) %>% 
  select(-`Barlow Cove`) %>% 
  bind_rows(effect_pots) -> table7_csv
write.csv(table7_csv, paste0('./results/rkc/Region1/', cur_yr, '/Table7_raw_samp_', cur_yr, '.csv'), row.names = FALSE) 


## area biomass compared to baseline ---------
biomass %>% 
  filter(Year >= 1995 & Year <= 2007) %>% #agr updated 1993 to 1995
  group_by(Location) %>% 
  summarise(legal_baseline = mean(legal.biomass), mature_baseline = mean(mature.biomass), 
            adj.legal_base = mean(adj.legal), adj.mature.base = mean(adj.mature)) %>% 
  as.data.frame() %>% 
  select(Location, mature_baseline, adj.mature.base) -> baseline.bay

biomass %>% 
  filter(Year == cur_yr) %>% 
  select(Location, mature_cur = mature.biomass, adj.mature_cur = adj.mature) -> curyr.area.mature

baseline.bay %>% 
  left_join(curyr.area.mature) %>% 
  mutate(pct.cur = 100*(mature_cur - mature_baseline)/mature_baseline, 
         pct.adj.cur = 100*(adj.mature_cur - adj.mature.base)/adj.mature.base) %>% 
  write.csv(paste0('./results/rkc/Region1/', cur_yr, '/relative_to_baseline_', cur_yr, '.csv'), row.names = FALSE) -> biomass_rate

#AGR creates legal relative to baseline table because why do we not have one?!?! 8/22/24
biomass %>% 
  filter(Year >= 1995 & Year <= 2007) %>%
  group_by(Location) %>% 
  summarise(legal_baseline = mean(legal.biomass), mature_baseline = mean(mature.biomass), 
            adj.legal.base = mean(adj.legal), adj.mature.base = mean(adj.mature)) %>% 
  as.data.frame() %>% 
  select(Location, legal_baseline, adj.legal.base) -> baseline.bay2

biomass %>% 
  filter(Year == cur_yr) %>% 
  select(Location, legal_cur = legal.biomass, adj.legal_cur = adj.legal) -> curyr.area.legal

baseline.bay2 %>% 
  left_join(curyr.area.legal) %>% 
  mutate(pct.cur = 100*(legal_cur - legal_baseline)/legal_baseline, 
         pct.adj.cur = 100*(adj.legal_cur - adj.legal.base)/adj.legal.base) %>% 
  write.csv(paste0('./results/rkc/Region1/', cur_yr, '/LEGAL_relative_to_baseline_', cur_yr, '.csv'), row.names = FALSE) -> biomass_rate_legal

           
