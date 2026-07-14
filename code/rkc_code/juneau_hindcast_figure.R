# K.Palof
# 7-10-19n updated / 7-15-2020 / 7-14-2021/7-7-22
# AGR 7-14-26

# Juneau area RKC forecast / hindcast figures
# Current figures used for stock health memo 

#AGR 26, some of the end graphs did not run, but I suspect they may be obsolete.

# Load packages -------------
library(tidyverse)
library(readxl)
library(extrafont)
library(grid)
library(gridExtra)
library(scales)
#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

# global --------
# update each year
cur_yr = 2026

#Load data ----------------
biomass <- read.csv("./data/rkc/biomass.csv")#  no record of historic mature biomass point estimates

#take the final year
cur_yr_legal <- biomass %>%
  filter(Location == "Juneau") %>%
  filter(Year == cur_yr) %>% 
  select(legal.biomass) %>% 
  pull()

cur_yr_mature <- biomass %>%
  filter(Location == "Juneau") %>%
  filter(Year == cur_yr) %>% 
  select(mature.biomass) %>% 
  pull()

biomass_JNU_curyr <- biomass %>% filter(Location == "Juneau") %>%
  select(year = Year,legal.biomass, mature.biomass)

status_pryr <- "open" #UPDATE with "closed", "open" or "PU only"
pryr =  cur_yr-1 #half baked wrangle, return

hindcast <- read.csv("./data/rkc/Juneau/hind_fore_cast_JNU_current.csv")
    # "these "forecast" are historic estimates in the "forecast" columns 
    # "cur_yr" needs to be updated with current years CSA model output for all years  
    # and only update the current year in the "forecast" columns
#does automatically below

#add in fishery status to the previus year
hindcast_1 <- hindcast %>%
  mutate(status = if_else(year == pryr, status_pryr, status))

#make hindcast one section longer
vector_temp <- data.frame(year=cur_yr, legal_curyr=cur_yr_legal, mature_curyr=cur_yr_mature, legal_forecast=cur_yr_legal, mature_forecast=cur_yr_mature, status="TBD")

hindcast_2 <- rbind(hindcast_1, vector_temp)

#replace the current year columns with the CSA model output
hindcast_3 <- biomass_JNU_curyr %>% left_join(hindcast_2) %>%
  select(-legal_curyr, -mature_curyr)

hindcast_4 <- hindcast_3 %>% dplyr::rename(legal_curyr=legal.biomass,
                                           mature_curyr = mature.biomass)
hindcast <- hindcast_4


hindcast_long <- gather(hindcast, type, pounds, legal_curyr:mature_forecast, factor_key = TRUE)

# Baseline ----
hindcast %>%
  filter(year > 1994 & year < 2008) %>% #updated to remove 93, 94 from these calcs due to spatial inconsistancies in sampling
  gather(type, pounds, legal_curyr:mature_forecast, factor_key = TRUE) %>%
  group_by(type) %>% 
  summarise(baseline = mean(pounds)) %>% 
  mutate(label = c("Legal (1995-2007)", "Mature (1995-2007)", "Legal (1995-2007)", "Mature (1995-2007)"), 
         start_yr = c(1979, 1979, 1979, 1979)) -> baseline_mean
baseline_mean_curyr <- as.data.frame(baseline_mean[1:2,])
baseline_mean_forecast <- as.data.frame(baseline_mean[3:4,])  

# when status for current year confirmed
#hindcast_updated <- hindcast %>%
 # mutate(status = case_when(status == "TBD" ~ "PU only",
  #                          status != "TBD" ~ status))

hindcast_updated <- hindcast

# Figure 1 redo ---------
    # should have current year's model with longterm baselines (1993-2007) and closure status. 
    #   also show current year's forecast as distinct from model output 
jnu_rkc_fig1 <- #hindcast %>% 
  hindcast_updated %>% # use this when know decision for current year
  dplyr::rename(legal_lb = legal_curyr, mature_lb = mature_curyr) %>% 
  select(-legal_forecast, -mature_forecast) %>% 
  gather(type, pounds, legal_lb:mature_lb, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  scale_shape_manual(name = "Fishery Status", values = c(0, 16, 2, 8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,750000),
                     breaks= seq(min(0), max(750000), by = 100000)) +

  ggtitle(paste0("Juneau ", cur_yr," model")) + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(cur_yr), by = 5)) +
  geom_hline(yintercept =  baseline_mean_curyr$baseline[1], color = "grey1")+
  geom_hline(yintercept = baseline_mean_curyr$baseline[2], color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.125,0.793), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8)) +
  geom_text(data = baseline_mean_curyr, aes(x = start_yr, y = baseline, label = label), 
            hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2)))

# this is the version when the current year has an asterisk symbol for TBD
ggsave(filename = "jnu_rkc_fig1.png", path = paste0(here::here(), '/figures/rkc/', cur_yr, '/juneau_fig1_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

# this is for the version when the current year has decision
ggsave(filename = "jnu_rkc_fig1_2.png", path = paste0(here::here(), '/figures/rkc/', cur_yr, '/juneau_fig1_', cur_yr, '_2.png'), dpi = 800, width = 7.5, height = 5.5)

# figure 1 with edits requested by regional staff #THIS FIGURE IS THE ONE WE USE IN MEMO-AGR!!!!!!

jnu_rkc_fig1_edited <- hindcast %>% 
  #hindcast_updated %>% # use this when know decision for current year
  mutate(status = case_when(
    status == "" ~ "TBD",
    status == "open" ~ "Open",
    status == "closed" ~ "Closed",
    #level the status factor so I have an intentional order
    TRUE ~ status
  )) %>%
  dplyr::rename(Legal = legal_curyr, Mature = mature_curyr, `Fishery Status` = status) %>% 
  select(-legal_forecast, -mature_forecast) %>% 
  gather(type, pounds, Legal:Mature, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_line(aes(color = type, group = type, linetype = type))+
  geom_point(aes(color = type, shape = `Fishery Status`), size =3) +
  scale_colour_manual(name = "", values = c("black", "grey60"))+
  scale_shape_manual(values = c(0, 16, 2, 8))+
  #scale_shape_manual(values = c(16, 2, 8, 0))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,750000),
                     breaks= seq(min(0), max(750000), by = 100000)) +
  
  #ggtitle(paste0("Juneau ", cur_yr," model")) + 
  ylab("Biomass (lb)")+ xlab("Year") +
  scale_x_continuous(breaks = seq(min(1974), max(max(hindcast$year)), by = 2)) +
  #scale_x_continuous(breaks = seq(min(1975), max(max(hindcast$year) + 1), by = 2)) + #I dont like this so I did it differently (line immediately above) - AGR
  #theme(plot.title = element_text(hjust =0.5)) +
  #scale_x_continuous(breaks = seq(min(1975),max(cur_yr), by = 5)) +
  geom_hline(yintercept =  baseline_mean_curyr$baseline[1], color = "grey1")+
  geom_hline(yintercept = baseline_mean_curyr$baseline[2], color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.15,0.793), legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11), axis.text = element_text(size = 14), 
        axis.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 90)) +
  #geom_text(data = baseline_mean_curyr, aes(x = start_yr, y = baseline, label = label), 
            #hjust = -0.45, vjust = 1.5, nudge_y = 0.05, size = 4) +
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2)))

ggsave(filename = "jnu_rkc_fig1_edited.png", path = paste0(here::here(), '/figures/rkc/', cur_yr), dpi = 800, width = 7.5, height = 5.5)

# Figure A1 ---old Figure 1 - move to Appendix --------
# forecast for each year 
jnu_rkc_annual_fore <- hindcast %>% 
  select(-legal_curyr, -mature_curyr) %>% 
  gather(type, pounds, legal_forecast:mature_forecast, factor_key = TRUE) %>% 
  mutate(status = case_when(
    status == "" ~ "TBD",
    status == "open" ~ "Open",
    status == "closed" ~ "Closed",
    #level the status factor so I have an intentional order
    TRUE ~ status
  )) %>%
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  #scale_shape_manual(name = "Fishery Status", values = c(0, 8, 2, 4))+
  scale_shape_manual(name = "Fishery Status", values = c(0, 16, 2, 8))+  
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,750000),
                     breaks= seq(min(0), max(750000), by = 100000)) +
  
  ggtitle("Juneau annual forecast reported") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(cur_yr), by = 5)) +
  geom_hline(yintercept = baseline_mean_forecast$baseline[1], color = "grey1")+
  geom_hline(yintercept = baseline_mean_forecast$baseline[2], color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.125,0.798), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8), 
        legend.spacing = unit(0.00005, "cm")) +
  geom_text(data = baseline_mean_forecast, aes(x = start_yr, y = baseline, label = label), 
            hjust = -0.55, vjust = 1.5, nudge_y = 0.05, size = 3.5) +
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2)))

ggsave(filename = "jnu_rkc_annual_fore.png", path = paste0(here::here(), '/figures/rkc/', cur_yr), dpi = 800, width = 7.5, height = 5.5)


#  select(year, legal_2023)figure of 2023 model with forecast in each year ----- #ar- this one is not working. It also does not seem importabt?
ggplot(hindcast_long, aes(year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey18", "grey1"))+
  scale_shape_manual(name = "", values = c(32,32,8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "blank")) +
  scale_y_continuous(labels = comma) +
  #ylim(0,700000) +
  ggtitle("Juneau 2023 model with annual forecast") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2024), by = 5)) #ar- sigh, have to update to 2024
#  theme(legend.position = c(0.8,0.7)) + 


# Figure 1 PRESENTATION  ---------
# legal only
# should have current year's model with longterm baselines (1993-2007) and closure status. 
#   also show current year's forecast as distinct from model output 
fig1_pres <- hindcast %>% 
  select(year, legal_curyr, legal_forecast, status) %>% 
  dplyr::rename(legal_lb = legal_curyr) %>% 
  select(-legal_forecast) %>% 
  gather(type, pounds, legal_lb, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type), show.legend = FALSE)+
  scale_colour_manual(name = "", values = c("black", "grey44"), guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(0, 16, 2, 8), 
                     labels = c("closed to comm and PU", "open to comm and PU", 
                                "PU open only"))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,600000),
                     breaks= seq(min(0), max(600000), by = 100000)) +
  ggtitle(paste0("Juneau Area Legal Biomass")) + 
  ylab("Estimated Legal Biomass (lbs)") +
  xlab("Year") +
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5)) +
  geom_hline(yintercept =  baseline_mean_curyr$baseline[1], color = "grey1")+
  #geom_hline(yintercept = baseline_mean_curyr$baseline[2], color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.845,0.875), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8)) +
  geom_text(data = baseline_mean_curyr[1, ], aes(x = start_yr, y = baseline, label = label), 
            hjust = -0.45, vjust = -1.0, nudge_y = 0.05, size = 3.5, show.legend = FALSE) +
  guides(shape = guide_legend(ncol = 1), group = guide_legend((ncol =2)))

ggsave(filename = "fig1_pres.png", plot=fig1_pres, paste0(here::here(), '/figures/rkc/', cur_yr,'/juneau_fig1_presentation_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)
##this one isnt working but I don't think I need it? - AR

# Figure 1 BOF doc  ---------
# legal and matureonly
# should have current year's model with longterm baselines (1993-2007) and closure status. 
#   also show current year's forecast as distinct from model output 
fig1BOF <- hindcast %>% 
  select(year, legal_curyr, mature_curyr, status) %>% 
  dplyr::rename(legal_lb = legal_curyr, mature_lb = mature_curyr) %>% 
  mutate(status = ifelse(status == "TBD", "PU only", as.character(status))) %>% 
  gather(type, pounds, legal_lb:mature_lb, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type), show.legend = FALSE)+
  scale_colour_manual(name = "", values = c("black", "grey44"), guide = FALSE)+
  scale_shape_manual(name = "Fishery Status", values = c(0, 16, 2, 8), 
                     labels = c("closed to comm and PU", "open to comm and PU", 
                                "PU open only"))+
  scale_linetype_manual(name = "", values = c("solid", "dashed")) +
  scale_y_continuous(labels = comma, limits = c(0,600000),
                     breaks= seq(min(0), max(600000), by = 100000)) +
  ggtitle(paste0("Juneau Area Legal Biomass")) + 
  ylab("Estimated Legal Biomass (lbs)") +
  xlab("Year") +
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5)) +
  geom_hline(yintercept =  baseline_mean_curyr$baseline[1], color = "grey1")+
  geom_hline(yintercept = baseline_mean_curyr$baseline[2], color = "grey44", linetype = "dashed") +
  theme(legend.position = c(0.845,0.875), legend.title = element_text(size = 9), 
        legend.text = element_text(size = 8)) +
  geom_text(data = baseline_mean_curyr, aes(x = start_yr, y = baseline, label = label), 
            hjust = -0.45, vjust = -1.0, nudge_y = 0.05, size = 3.5, show.legend = FALSE) +
  guides(shape = guide_legend(ncol = 1), group = guide_legend((ncol =2)))

ggsave(fig1BOF, paste0('./figures/rkc/2024/juneau_fig1_presentation2_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5) #ar updated year to 2024

  
 