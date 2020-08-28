# K.Palof
# 7-10-19n updated / 7-15-2020

# Juneau area RKC forecast / hindcast figures
# Current figures used for stock health memo 

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
cur_yr = 2020

#Load data ----------------
#biomass <- read.csv("./data/redcrab/biomass.csv") no record of historic mature biomass point estimates
# in each year so using 2017 model output

hindcast <- read.csv("./data/rkc/Juneau/hind_fore_cast_JNU_current.csv") 
    # **FIX** currently needs manual updating...fix this.
    # these are historic estimates while "forecast" columns are the current years model estimates
    # needs to be updated with current years CSA model ouput for all years in "forecast columns" and current year in other columns


hindcast_long <- gather(hindcast, type, pounds, legal_curyr:mature_forecast, factor_key = TRUE)

# Baseline ----
hindcast %>%
  filter(year > 1992 & year < 2008) %>% 
  gather(type, pounds, legal_curyr:mature_forecast, factor_key = TRUE) %>%
  group_by(type) %>% 
  summarise(baseline = mean(pounds)) %>% 
  mutate(label = c("Legal (1993-2007)", "Mature (1993-2007)", "Legal (1993-2007)", "Mature (1993-2007)"), 
         start_yr = c(1979, 1979, 1979, 1979)) -> baseline_mean
baseline_mean_curyr <- as.data.frame(baseline_mean[1:2,])
baseline_mean_forecast <- as.data.frame(baseline_mean[3:4,])  

# Figure 1 redo ---------
    # should have current year's model with longterm baselines (1993-2007) and closure status. 
    #   also show current year's forecast as distinct from model output 
jnu_rkc_fig1 <- hindcast %>% 
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
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2))) +
  ggsave(paste0('./figures/rkc/2020/juneau_fig1_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


# Figure A1 ---old Figure 1 - move to Appendix --------
# forecast for each year 
jnu_rkc_annual_fore <- hindcast %>% 
  select(-legal_curyr, -mature_curyr) %>% 
  gather(type, pounds, legal_forecast:mature_forecast, factor_key = TRUE) %>% 
  ggplot(aes(year, pounds, group = type)) +
  geom_point(aes(color = type, shape = status), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey44"))+
  #scale_shape_manual(name = "Fishery Status", values = c(0, 8, 2, 4))+
  scale_shape_manual(name = "Fishery Status", values = c(0, 8, 2, 4))+  
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
  guides(shape = guide_legend(ncol = 2), group = guide_legend((ncol =2))) +
  ggsave(paste0('./figures/rkc/2020/juneau_figA1_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


#  select(year, legal_2018)figure of 2018 model with forecast in each year -----
ggplot(hindcast_long, aes(year, pounds, group = type))+ 
  geom_point(aes(color = type, shape = type), size =3) +
  geom_line(aes(color = type, group = type, linetype = type))+
  scale_colour_manual(name = "", values = c("black", "grey18", "grey1"))+
  scale_shape_manual(name = "", values = c(32,32,8))+
  scale_linetype_manual(name = "", values = c("solid", "dashed", "blank")) +
  scale_y_continuous(labels = comma) +
  #ylim(0,700000) +
  ggtitle("Juneau 2018 model with annual forecast") + ylab("Estimated Biomass (lbs)")+ xlab("Year")+
  theme(plot.title = element_text(hjust =0.5)) +
  scale_x_continuous(breaks = seq(min(1975),max(2019), by = 5))
#  theme(legend.position = c(0.8,0.7)) + 


# Figure 1 PRESENTATION  ---------
# legal only
# should have current year's model with longterm baselines (1993-2007) and closure status. 
#   also show current year's forecast as distinct from model output 
hindcast %>% 
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
  guides(shape = guide_legend(ncol = 1), group = guide_legend((ncol =2))) +
  ggsave(paste0('./figures/rkc/juneau_fig1_presentation_', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)

