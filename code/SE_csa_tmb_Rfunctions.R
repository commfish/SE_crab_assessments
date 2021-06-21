# notes ----
## functions for TMB SE crab CSA
## Tyler Jackson
## 6/21/2021

# libraries ----
library(tidyverse)
library(lubridate)
library(FNGr); theme_set(theme_sleek())
library(TMB)

# tau ----

## args: catch_mid and index_mid are dates at the midpoint of fishery and catch respective, as character vectors in the format "mm/dd/yyyy"

## fraction of year between catch t and survey t
f_tau_cs <- function(catch_mid, index_mid){
  as.numeric((yday(mdy(index_mid)) - lag(yday(mdy(catch_mid)), 1)) / 365)
}
## fraction of year between survey t and t-1
f_tau_s <- function(index_mid){
  as.numeric((mdy(index_mid) - lag(mdy(index_mid), 1)) / 365)
}
# output ----

## args: in_data - data frame of raw input data
##       report - list of TMB report objects (see TMB::report())
##       stage - name of size stage cpue as it appears in in_data ("pre_recruit", "recruit", "post_recruit")
##       name_path - file path, name and extension for saving png of plot. example: "./figures/plot.png"
f_plot_cpue_fit <- function(in_data, report, stage, name_path) {
  
  # x axis breaks and labels
  x_axis = FNGr::tickr(in_data, survey_year, 5)
  
  # y axis label
  y_axis = case_when(stage == "pre_recruit" ~ "Pre-Recruit",
                     stage == "recruit" ~ "Recruit",
                     stage == "post_recruit" ~ "Post-Recruit")
  
  # extract fitted values
  report[[1]] %>%
    as_tibble() %>%
    rename_all(~c("pre_recruit", "recruit", "post_recruit")) %>%
    pull(stage) -> fitted
  
  # pull observed data and plot
  in_data %>%
    dplyr::select("survey_year", stage) %>%
    rename_all(~c("year", "cpue")) %>%
    filter(!is.na(cpue)) %>%
    ggplot()+
    geom_point(aes(x = year, y = cpue), shape = 21)+
    geom_line(aes(x = year, y = fitted), color = "blue")+
    scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
    labs(x = NULL, y = paste0(y_axis, " Survey CPUE")) -> p1
  
  if(missing(name_path)) {return(p1)} else {
    ggsave(name_path, plot = p1, height = 3, width = 5, units = "in")
  }
}

## args: in_data - data frame of raw input data
##       par_est - data frame of parameter estimates and standard errors (see TMB::summary.summary.sdreport())
##       report - list of TMB report objects (see TMB::report())
##       stage - name of size stage cpue as it appears in in_data ("pre_recruit", "recruit", "post_recruit")
##       name_path - file paths, name and extension for saving png of plots. two are required for 1) abundance and 2) biomass. 
##                   example: c("./figures/plot_abund.png", "./figures/plot_biomass.png")
f_MMB <- function(in_data, par_est, report, name_path){
  # x axis breaks and labels
  x_axis = FNGr::tickr(in_data, survey_year, 5)
  
  # extract q est, back transform
  par_est %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    rename_all(~c("par", "est", "se")) %>%
    mutate(back_trans = exp(est)) %>%
    filter(par == "ln_q") %>%
    pull(back_trans) -> q
  
  # compute MMB
  report[[1]] %>%
    as_tibble() %>%
    rename_all(~c("pre_recruit_abund", "recruit_abund", "post_recruit_abund")) %>%
    mutate_all(function(x){x/q}) %>%
    mutate(legal_abund = recruit_abund + post_recruit_abund,
           mature_abund = pre_recruit_abund + recruit_abund + post_recruit_abund) -> tmp
  
  in_data %>%
    filter(!is.na(recruit)) %>%
    bind_cols(tmp) %>%
    mutate(legal_biomass = legal_abund * legal_avg_wt,
           pre_recruit_biomass = pre_recruit_abund * pre_recruit_avg_wt,
           mature_biomass = legal_biomass + pre_recruit_biomass) -> res
  
  # timeseries plots
  ## biomass
  res %>%
    dplyr::select(survey_year, legal_biomass, mature_biomass) %>%
    pivot_longer(c(legal_biomass, mature_biomass)) %>%
    mutate(name = case_when(name == "mature_biomass" ~ "MMB",
                            name == "legal_biomass" ~ "LMB")) %>%
    ggplot(aes(x = survey_year, y = value / 1000, color = name))+
    geom_point()+
    geom_line()+
    labs(x = NULL, y = "Biomass (1,000 lb)", color = NULL)+
    scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
    scale_color_manual(values = c(4, 3))+
    theme(legend.position = c(0.1,0.9)) -> p1
  ## abundance
  res %>%
    dplyr::select(survey_year, legal_abund, mature_abund) %>%
    pivot_longer(c(legal_abund, mature_abund)) %>%
    mutate(name = case_when(name == "mature_abund" ~ "MMB",
                            name == "legal_abund" ~ "LMB")) %>%
    ggplot(aes(x = survey_year, y = value / 1000, color = name))+
    geom_point()+
    geom_line()+
    labs(x = NULL, y = "Abundance (1,000s)", color = NULL)+
    scale_x_continuous(breaks = x_axis$breaks, labels = x_axis$labels)+
    scale_color_manual(values = c(4, 3))+
    theme(legend.position = c(0.1,0.9)) -> p2
  
  # table
  res %>%
    dplyr::select(survey_year, 
                  pre_recruit_abund, pre_recruit_biomass, 
                  legal_abund, legal_biomass, 
                  mature_abund, mature_biomass) -> x1
  
  if(missing(name_path)) {print(p1);print(p2);x1} else {
    ggsave(name_path[1], plot = p1, height = 3, width = 5, units = "in")
    ggsave(name_path[2], plot = p2, height = 3, width = 5, units = "in")
    x1
  }
  
}