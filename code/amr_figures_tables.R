# K. Palof     10-13-2020

# Purpose: Figures and Tables for AMRs - BOF documents produced every 3 years


## load -------------------------
#source('./code/functions.R')
#font_import()
library(tidyverse)
library(cowplot)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

#theme_set(theme_sleek())
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))


cur_yr = 2019
options(scipen = 10L)

## data -------------------
tanner <- read.csv(paste0('./data/tanner/tanner_', cur_yr, '_biomassmodel.csv'))# file name will change annually
# this is input from OceanAK - set up as red crab survey data for CSA


# tanner AMR biomass figure -------
head(tanner)

tanner %>% 
  mutate(group = ifelse(Area == "Stephens Passage" | Area == "Icy Strait" | Area == "Holkham Bay", 1, 
                        ifelse(Area == "Glacier Bay"|Area == "Thomas Bay", 2, 
                               ifelse(Area == "Seymour Canal"|Area == "North Juneau"|Area == "Excursion Inlet", 3,
                                      ifelse(Area == "Pybus Bay"|Area == "Gambier Bay"|
                                               Area == "Lynn Sisters"|Area == "Peril Strait", 4, 0))))) -> tanner

tanner %>% 
  ggplot(aes(Year, Mature, group = Area)) +
  geom_line(aes(group = Area, colour = Area)) +
  geom_point(aes(colour = Area, shape = Area), size =3) +
  scale_shape_manual(name = "", values = c(15, 17, 17, 18, 15, 16, 17, 18, 15, 16, 17, 18)) +
  facet_wrap(~group, scales = "free") +
  theme(strip.text.x = element_blank()) +
  guides(shape = FALSE) 
ggsave(paste0('./figures/tanner/',cur_yr,'/tanner_biomass_AMR', cur_yr, '.png'), dpi = 800, width = 7.5, height = 5.5)


# create 4 panels and put them together ----
tanner %>% 
  filter(group == 1) %>% 
  ggplot(aes(Year, Mature/10000, group = Area)) +
  geom_line(aes(group = Area, colour = Area)) +
  geom_point(aes(colour = Area, shape = Area), size =3) +
  scale_shape_manual(name = "", values = c(18, 15, 17), guide = guide_legend(nrow = 1)) +
  ylab("Mature Biomass (10,000 lb)") +
  scale_x_continuous(breaks = seq(min(1996),max(cur_yr), by =2)) +
  scale_y_continuous(breaks = seq(min(0),max((tanner$Mature)), by = 10)) +
  expand_limits(y = 0) +
  theme(strip.text.x = element_blank(), 
        legend.position = c(0.50,0.9), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x=element_text(angle = -45, vjust = 0.5)) +
  xlab(NULL) +
  guides(colour = FALSE) -> p1

tanner %>% 
  filter(group == 2) %>% 
  ggplot(aes(Year, Mature/10000, group = Area)) +
  geom_line(aes(group = Area, colour = Area)) +
  geom_point(aes(colour = Area, shape = Area), size =3) +
  scale_shape_manual(name = "", values = c(18, 15, 17), guide = guide_legend(nrow = 1)) +
  ylab("Mature Biomass (10,000 lb)") +
  scale_x_continuous(breaks = seq(min(1996),max(cur_yr), by =2)) +
  scale_y_continuous(breaks = seq(min(0),max((tanner$Mature)), by = 10)) +
  expand_limits(y = 0) +
  theme(strip.text.x = element_blank(), 
        legend.position = c(0.35,0.9), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x=element_text(angle = -45, vjust = 0.5)) +
  xlab(NULL) +
  guides(colour = FALSE) -> p2

tanner %>% 
  filter(group == 3) %>% 
  ggplot(aes(Year, Mature/10000, group = Area)) +
  geom_line(aes(group = Area, colour = Area)) +
  geom_point(aes(colour = Area, shape = Area), size =3) +
  scale_shape_manual(name = "", values = c(18, 15, 17), guide = guide_legend(nrow = 2)) +
  ylab("Mature Biomass (10,000 lb)") +
  scale_x_continuous(breaks = seq(min(1996),max(cur_yr), by =2)) +
  expand_limits(y = c(0, 70)) +
  scale_y_continuous(breaks = seq(min(0),max(max(tanner$Mature + 100000)), by = 10)) +
  theme(strip.text.x = element_blank(), 
        legend.position = c(0.45,0.85), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x=element_text(angle = -45, vjust = 0.5)) +
  xlab(NULL) +
  guides(colour = FALSE) -> p3

tanner %>% 
  filter(group == 4) %>% 
  ggplot(aes(Year, Mature/10000, group = Area)) +
  geom_line(aes(group = Area, colour = Area)) +
  geom_point(aes(colour = Area, shape = Area), size =3) +
  scale_shape_manual(name = "", values = c(18, 15, 17, 16), guide = guide_legend(nrow = 2)) +
  ylab("Mature Biomass (10,000 lb)") +
  scale_x_continuous(breaks = seq(min(1996),max(cur_yr), by =2)) +
  expand_limits(y = c(0, 30)) +
  scale_y_continuous(breaks = seq(min(0),max(max(tanner$Mature + 100000)), by = 10)) +
  theme(strip.text.x = element_blank(), 
        legend.position = c(0.35,0.88), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        axis.text.x=element_text(angle = -45, vjust = 0.5)) +
  xlab(NUtannerLL) +
  guides(colour = FALSE) -> p4


panel <- plot_grid(p1, p2, p3, p4, ncol = 2, align = 'v')
ggsave(paste0('./figures/tanner/',cur_yr, '/AMR_biomass_', cur_yr, '.png'), panel,  
       dpi = 800, width = 11, height = 9.5)
