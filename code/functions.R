# K.Palof   8-4-17

# 7/20/23: changed data set used in panel figures to 1995-present due to biologists' concerns about pre-1995 data (Caitlin Stern)

# Functions for processing of red king crab data
# need to keep area and year 

## load packages -----------
library(reshape)
library(tidyverse)
library(weights)
library(broom)
library(kableExtra)
library(xtable)
library(flextable)
library(lubridate)

library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
#library(SDMTools)
library(grid)
library(gridExtra)
#install.packages("devtools")
#devtools::install_github("ben-williams/fgnr")
#library(FNGr)
library(scales)
library(cowplot)
library(here)
library(TeachingDemos)
library(purrr)
library(TMB)
library(radiant.data)

library(ragg)

#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
windowsFonts()

loadfonts(quiet = T)
fonts()
set_null_device("png")

#theme_set(theme_sleek())
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))

### calculate midpoint date function ----------------
int_midpoint <- function(interval) {
  round_date(int_start(interval) + (int_end(interval) - int_start(interval))/2, unit="day")
}

### short term function ----------------
#input is file with last four years of data summarized by pot
      # area
      # year
short_t <- function(bypot_st, year, area) {
  bypot_st_long <- gather(bypot_st, recruit.status, crab, Missing:Small.Females, factor_key = TRUE) 
  
  bypot_st_long %>% 
    group_by(recruit.status) %>% 
    do(fit = lm(crab ~ Year, data = ., weights = weighting)) ->short_term
  
  bypot_st_long %>% 
    group_by(recruit.status) %>% 
    do(fit3 = glance(lm(crab ~ Year, data = ., weights = weighting))) %>% 
    unnest(fit3) -> short_term_out
  #short_term %>%
  #  glance(fit) ->short_term_out
  
  short_term_out %>%
    select(recruit.status, r.squared, p.value)->short_term_out2
  
  bypot_st_long %>% 
    group_by(recruit.status) %>% 
    do(fit2 = tidy(lm(crab ~ Year, data = ., weights = weighting))) %>% 
    unnest(fit2) -> short_term_slope
  
  #short_term %>%
  #  tidy(fit) -> short_term_slope
  
  short_term_slope %>%
    select(recruit.status, term,  estimate) %>%
    spread(term, estimate) %>% 
    right_join(short_term_out2)->short_term_results # estimate here is slope from regression
  short_term_results %>% 
    dplyr::rename(slope = Year) -> short_term_results
  #Now need to add column for significance and score
  short_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/rkc/', area, '/',year,  '/shortterm.csv'))
}


### Long term function -------------------
# need current years data and file with long term means

long_t <- function(dat5_current, baseline, year, area, location) {
  #baseline <- read.csv("./data/redcrab/longterm_means.csv")
  baseline %>% filter(Location == location)-> baseline_values
  baseline_values_long <- gather(baseline_values, recruit.status, lt_mean, Juvenile:Post_Recruit, factor_key = TRUE)
  #Uses a weighted mean to help calculate the t.test - part of package weights
  # the y = has to be changed for each area but once they are set they are the same from year to year
  
  juv <- wtd.t.test(dat5_current$Juvenile, y = baseline_values$Juvenile, weight = dat5_current$weighting, samedata=FALSE)
  sfem <- wtd.t.test(dat5_current$Small.Females, y = baseline_values$Small.Female, weight = dat5_current$weighting, samedata=FALSE)
  lfem <- wtd.t.test(dat5_current$Large.Females, y = baseline_values$Large.Female, weight = dat5_current$weighting, samedata=FALSE)
  
  prer <- wtd.t.test(dat5_current$Pre_Recruit, y = baseline_values$Pre_Recruit, weight = dat5_current$weighting, samedata=FALSE)
  rec <- wtd.t.test(dat5_current$Recruit, y = baseline_values$Recruit , weight = dat5_current$weighting, samedata=FALSE)
  postr <- wtd.t.test(dat5_current$Post_Recruit, y = baseline_values$Post_Recruit, weight = dat5_current$weighting, samedata=FALSE)
  
  long_term <- matrix(nrow = 6, ncol = 3)
  rownames(long_term) <- c("juv", "small.female", "large.female", "pre.recruit", "recruit", "post.recruit")
  colnames(long_term) <- c("mean", "p.value", "lt.mean")
  
  long_term[1,1] <-juv$additional["Mean"]
  long_term[1,2] <- juv$coefficients["p.value"]
  long_term[2,1] <-sfem$additional["Mean"]
  long_term[2,2] <- sfem$coefficients["p.value"]
  long_term[3,1] <-lfem$additional["Mean"]
  long_term[3,2] <- lfem$coefficients["p.value"]
  long_term[4,1] <-prer$additional["Mean"]
  long_term[4,2] <- prer$coefficients["p.value"]
  long_term[5,1] <-rec$additional["Mean"]
  long_term[5,2] <- rec$coefficients["p.value"]
  long_term[6,1] <-postr$additional["Mean"]
  long_term[6,2] <- postr$coefficients["p.value"]
  
  long_term[1:6,3] <- baseline_values_long$lt_mean
  
  long_term_results <- as.data.frame(long_term)
  
  long_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > lt.mean, 1,
                                ifelse(p.value <0.05 & mean < lt.mean, -1, 0))) %>% 
    mutate(recruit.status = baseline_values_long$recruit.status) -> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  write_csv(long_term_results, paste0('results/rkc/', area, '/', year, '/longterm.csv'))
  
}



### weights from weight -length relationship ------------
weights <- function(dat1, slope, intercept, area, year){
  dat1 %>%
    mutate(weight_lb = (exp((slope*log(Length.Millimeters))-intercept))*(2.2/1000)) -> dat1
  
  Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
  Legal =c("Recruit", "Post_Recruit")
  # summary of weights all together - would like these in one calc and one summary table
  dat1 %>% 
    group_by(Year) %>% 
    filter(Sex.Code == 1) %>% 
    summarise(mature_lbs = weighted.mean(weight_lb[Recruit.Status %in% Mature], 
                                   Number.Of.Specimens[Recruit.Status %in% Mature]), 
              legal_lbs = weighted.mean(weight_lb[Recruit.Status %in% Legal], 
                                  Number.Of.Specimens[Recruit.Status %in% Legal]), 
              prer_lbs = weighted.mean(weight_lb[Recruit.Status == "Pre_Recruit"], 
                                 Number.Of.Specimens[Recruit.Status == "Pre_Recruit"])) -> male_weights
  # final results with score - save here
  write_csv(male_weights, paste0('results/rkc/', area, '/', year, '/maleweights.csv'))
}


### female percent poor clutch ---------

poor_clutch <- function (LgF_dat1, area, year){
# large or mature females
# % poor clutch - less than 10%
LgF_dat1 %>% filter(!is.na(Egg.Percent)) %>% 
  mutate(Less25 = ifelse(Egg.Percent < 25, "y", "n"))-> LgF_dat1 # where 1 is yes and 2 is no

LgF_dat1 %>%
  group_by(Year, Location, Trip.No, Pot.No, Less25) %>%
  summarise(hat = sum(Number.Of.Specimens)) -> poorclutch

poorclutch1 <- dcast(poorclutch, Year + Location + Trip.No + Pot.No ~ Less25, sum, drop=TRUE)

poorclutch1 %>%
  mutate(var1 = y / (y+n)) -> poorclutch1
poorclutch1 %>%
  group_by(Year)%>%
  summarise(Pclutch = mean(var1) , Pclutch.se = (sd(var1))/sqrt(sum(!is.na(var1)))) -> poorclutch_summary_all

poorclutch1 %>% filter(Year == year) -> poorclutch1_current
write_csv(poorclutch1_current, paste0('results/rkc/', area, '/', year, '/poorclutch1_current.csv'))
write_csv(poorclutch1, paste0('results/rkc/', area, '/', year, '/poorclutch_all.csv'))
write_csv(poorclutch_summary_all, paste0('results/rkc/', area, '/', year, '/poorclutch_summary_all.csv'))

}

poor_clutch_long <- function(poorclutch_current, area, year){
  if(nrow(poorclutch_current) > 1){
  
  lt_female <- t.test(poorclutch_current$var1, mu = 0.10)
  
  longt_female <- matrix(nrow = 1, ncol = 2)
  rownames(longt_female) <- c("large.female")
  colnames(longt_female) <- c("mean", "p.value")
  
  longt_female[1,1] <-mean(poorclutch_current$var1)
  longt_female[1,2] <- lt_female$p.value
  
  longt_female <- as.data.frame(longt_female)
  longt_female %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > 0.10, -1,
                                ifelse(p.value <0.05 & mean < 0.10, 1, 0))) %>% 
    mutate(recruit.status = c("large.female")) -> longt_female #estimate is slope from regression
  }
  
  if(nrow(poorclutch_current) ==1) {
    longt_female <- matrix(nrow = 1, ncol = 2)
    rownames(longt_female) <- c("large.female")
    colnames(longt_female) <- c("mean", "p.value")
    longt_female <- as.data.frame(longt_female)
      }
  #return(longt_female)
  write_csv(longt_female, paste0('results/rkc/', area, '/', year, '/lt_female.csv'))
}

poor_clutch_short <- function(females_all, area, year){
  females_all %>%
    filter(Year >= (year-3)) -> LgF_short # short term file has last 4 years in it
  #output this file as .csv to add to next year
  #write_csv(females_all, paste0('results/redcrab/', area,'/poorclutch_females_all.csv'))
  
  LgF_short %>% 
    mutate(per_poorclt = var1)  -> LgF_short
  
  plot(LgF_short$Year, LgF_short$per_poorclt)
  LgF_fit <-lm(per_poorclt ~ Year, data = LgF_short)
  #abline(LgF_fit, col= 'red')
  #summary(LgF_fit)
  
  shortt_female <- matrix(nrow = 1, ncol = 4)
  rownames(shortt_female) <- c("large.female")
  colnames(shortt_female) <- c("intercept", "slope", "p.value", "r_squared")
  
  shortt_female[1,1:2] <- tidy(LgF_fit)$estimate # extract estimate column which is intercept and slope
  shortt_female[1,3] <- glance(LgF_fit)$p.value # extract r.squared, and p.value
  shortt_female[1,4] <- glance(LgF_fit)$r.squared # extract r.squared, and p.value
  shortt_female <- as.data.frame(shortt_female)
  #Now need to add column for significance and score
  shortt_female %>%
    mutate(significant = ifelse(p.value < 0.05 & slope < 0, 1,
                                ifelse(p.value < 0.05 & slope > 0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> shortt_female #estimate is slope from regression
  # final results with score - save here
  
  write.csv(shortt_female, paste0('results/rkc/', area, '/', year, '/short_female.csv'))
}

### females egg percentage ------------
egg_percent <-function(LgF_dat1, area, year){
  LgF_dat1 %>%
    group_by(Year, Location, Pot.No) %>% filter(!is.na(Egg.Percent)) %>% 
    summarise (egg_mean = weighted.mean(Egg.Percent, Number.Of.Specimens)) -> clutch_by_pot
  
  clutch_by_pot %>%
    group_by(Year) %>% 
    summarise(mean = mean(egg_mean), egg.se = (sd(egg_mean)/sqrt(sum(!is.na(egg_mean))))) -> egg_per_mean
  write_csv(egg_per_mean, paste0('results/rkc/', area, '/', year,  '/egg_percent_mean_all.csv'))
  write_csv(LgF_dat1, paste0('results/rkc/', area, '/', year, '/largef_all.csv'))
}


### total stock health table --------------
total_health <- function(area, year){
  longterm <- read_csv(paste0('results/rkc/', area, '/', year, '/longterm.csv'))
  shortterm <- read_csv(paste0('results/rkc/', area, '/',  year, '/shortterm.csv'))
  lt_female <- read_csv(paste0('results/rkc/', area, '/',  year, '/lt_female.csv'))
  short_female <- read_csv(paste0('results/rkc/', area, '/', year, '/short_female.csv'))

total_health <- sum(longterm$significant, shortterm$score, 
                    lt_female$significant, short_female$score) # long term scores CPUE
# short term scores CPUE
# need females poorclutch short and long term
stock_health <- matrix(nrow = 1, ncol = 2)
rownames(stock_health) <- c(area)
colnames(stock_health) <- c("location","score_f")

stock_health[1,1] <- area
stock_health[1,2] <- total_health
stock_health <- as.data.frame(stock_health)
stock_health %>% 
  mutate(score = as.numeric((score_f))) -> stock_health
stock_health %>% 
  mutate(health_status = ifelse(score < -4.25, "poor", ifelse(score > -4.25 & score<= -1.75, "below average", 
                                                              ifelse(score > -1.75 & score <= 1.5, "moderate", 
                                                                     ifelse(score > 1.75 & score <= 4.25, "above average", 
                                                                            ifelse(score > 4.25, "healthy", "unknown")))))) %>% 
  mutate (harvest_per = ifelse(health_status == "poor", 0, ifelse(health_status == "below average", 0.05, 
                                                                  ifelse(health_status == "moderate", 0.10, 
                                                                         ifelse(health_status == "above average", 0.15,
                                                                                ifelse(health_status == "healthy", 0.20, "unk")))))) -> stock_health
#select ( - score_f) -> stock_health
write_csv(stock_health, paste0('results/rkc/', area, '/', year, '/stock_health.csv'))
}


## CONF panel figure ---------------
panel_figure <- function(survey.location, cur_yr, base.location, option, scale){
  # survey.location and baseline.location are the same is most areas.  Check
  # baseline file to see if they differ
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females), Option 4 - created for Seymour Canal scaling issues
  CPUE_wt_graph <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                   '/cpue_wt_since_95.csv')) # changed this to one since 95 - make this change to all processing codes.
  poorclutch_summary <- read.csv(paste0('./results/rkc/', survey.location, 
                                        '/', cur_yr, '/poorclutch_summary_all.csv'))
  egg_mean_all <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                  '/egg_percent_mean_all.csv'))
  # file with year and mean percent poor clutch and se poor clutch from 1995 to current
  mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv')
  baseline <- read.csv("./data/rkc/longterm_means.csv")
  biomass <- read.csv("./data/rkc/biomass.csv") 
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  # mr adjustments can be made in the function using mr_adjust file.
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_wt", 
                                                        "recruit", ifelse(recruit.status == "PreR_SE", 
                                                                          "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                                                                "recruit", "post.recruit "))))) %>% 
    mutate(type = ifelse(recruit.status == "PreR_SE",
                         "se", 
                         ifelse(recruit.status == "Rec_SE", 
                                "se", ifelse(recruit.status == "PR_SE", 
                                             "se", "mean"))))-> males_long
  males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph
  
  ### females/juv prep ------------
  CPUE_wt_graph %>% 
    select(Year,Juvenile_wt, SmallF_wt, MatF_wt, 
           Juv_SE, SmallF_SE, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Juvenile_wt",
                                  "juvenile.male", 
                                  ifelse(recruit.status == "SmallF_wt", 
                                         "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                                                                   "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                                                                           "juvenile.female", "mature.female"))))) %>% 
    mutate(type = ifelse(recruit.status == "Juv_SE",
                         "se", 
                         ifelse(recruit.status == "SmallF_SE", 
                                "se", ifelse(recruit.status == "MatF_SE", 
                                             "se", "mean"))))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(Location == base.location) -> baseline2
    
  ## poor clutch --------
  poorclutch_summary %>% 
    filter(Year >= 1995) %>% 
    mutate(Pclutch100 = Pclutch *100, 
           Pclutch.se100 = Pclutch.se*100) %>% 
    select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary95
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(Year >= 1995) -> egg_mean_all_95
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary95 %>% 
    left_join(egg_mean_all_95) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch100:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch100",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se100", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se100", "se", ifelse(vname == "egg.se", 
                                                                "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  ## biomass manipulations 
  
  # file for all locations.  Has legal biomass from CSA, harvest
  # mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
  # adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
  mr_adjust %>% 
    select(-X) %>% 
    mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
    select(-area) -> mr_adjust2
  
  biomass %>% 
    left_join(mr_adjust2) %>% 
    mutate(adj.legal = legal.biomass*weighted_ADJ, 
           adj.mature = mature.biomass*weighted_ADJ) -> biomass
  
  if(survey.location != "Juneau") {
  biomass %>% 
    select(-weighted_ADJ, -legal.biomass, -mature.biomass) %>% 
    gather(type, pounds, harvest:adj.mature, factor_key = TRUE) %>% 
    filter(Location == survey.location) %>% 
    filter(Year >= 1995) -> biomass_graph
  
  biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_adj_mean = mean(adj.mature), 
                legal_adj_mean = mean(adj.legal)) -> baseline_means
  }
  
  if(survey.location == "Juneau"){
    biomass %>% 
      select(-weighted_ADJ, -adj.legal, -adj.mature) %>% 
      gather(type, pounds, harvest:mature.biomass, factor_key = TRUE) %>% 
      filter(Location == survey.location) %>% 
      filter(Year >= 1995) -> biomass_graph
    biomass_graph %>% 
      filter(Year <= 2007 & Year >=1995) %>% 
      spread(type, pounds) %>% 
      summarise(mature_mean = mean(mature.biomass), 
                legal_mean = mean(legal.biomass)) -> baseline_means
  }
  
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(colour = recruit.class, shape = recruit.class, 
                   fill = recruit.class), size =3) +
    geom_line(aes(group = recruit.class, colour = recruit.class))+
    #scale_colour_manual(name = "", values = c("grey1", "grey65", "grey34"))+
    #scale_fill_manual(name = "", values = c("grey1", "grey65", "grey34")) +
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    scale_y_continuous(breaks = seq(min(0),max((max(males_graph$mean) + max(males_graph$se))), by = 1)) + # change to have more tick marks
    #scale_y_continuous(limits = c(0,(max(males_graph$mean) + max(males_graph$se))),
    #                   oob = rescale_none) +
    #ylim(0,(max(males_graph$mean) + max(males_graph$se))) + 
    ggtitle(survey.location) + ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
    #              width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Recruit, color = "#56B4E9", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "#999999", 
               lwd = 0.75)+
    theme(legend.position = c(0.5,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(size = 24))
  
  if(survey.location == "LynnSisters"){
    p1 = p1 + ggtitle("Lynn Sisters")
  }

  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    #scale_colour_manual(name = "", values = c("grey34","grey62", "grey1"))+
    scale_shape_manual(name = "", values = c(17, 16, 15))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    #ylim(0,25) + 
    scale_y_continuous(limits = c(0,(max(round(femjuv_graph$mean, 0) +1))), oob = rescale_none) +
    ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
    #              width =.4) +
    geom_hline(yintercept = baseline2$Juvenile, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Small.Female, color = "#999999", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Large.Female, color = "#56B4E9")+
    theme(legend.position = c(0.7,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0('Female/juvenile CPUE & egg health for ', survey.location)) +
      theme(plot.title = element_text(size = 24))
  }
  
  if(survey.location == "LynnSisters"){
    p2 = p2 + ggtitle("Female/juvenile CPUE & egg health for Lynn Sisters") +
      theme(plot.title = element_text(size = 20))
  }
  

  #### F1c Female eggs graph -----------
  p3 <- ggplot(female_egg_graph, aes(Year, mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                  width =.4) +
    geom_line(aes(color = female.egg)) +
    geom_point(aes(fill = female.egg, shape = female.egg), size =3) +
    
    scale_fill_manual(name = "", values = c("black", "gray100")) +
    scale_colour_manual(name = "", values = c("grey1", "black")) +
    scale_shape_manual(name = "", values = c(21, 21)) +
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    ylab("Percentage") + 
    xlab(NULL) +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    theme(legend.position = c(0.2,0.5), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) 
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Year")
  }
  
  
  ### biomass harvest graph --------------
  if(survey.location != "Juneau"){
  p4 <- ggplot(biomass_graph, aes(Year, pounds, group = type))+ 
    geom_point(aes(color = type, shape = type), size =3) +
    geom_line(aes(color = type, group = type, linetype = type))+
    scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
    scale_shape_manual(name = "", values = c(1, 18, 32))+
    scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
    ylab("Biomass (lbs)") + 
    xlab("Year") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds, 
                                                        na.rm = TRUE) + 25000),
                       breaks= seq(min(0), max(max(biomass_graph$pounds, 
                                                   na.rm = TRUE)+25000), by = 50000)) +
    theme(legend.position = c(0.5,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) + 
    geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean), color = "grey1")+
    geom_hline(data = baseline_means, aes(yintercept = mature_adj_mean), 
               color = "grey55", linetype = "dashed")
  if(scale == 1){
    p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1600000),
                  breaks= seq(min(0), max(1600000), by = 150000), oob = rescale_none)
  }
  }
  
  if(survey.location == "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds, 
                                                          na.rm = TRUE) + 25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds, 
                                                     na.rm = TRUE)+25000), by = 50000)) +
      theme(legend.position = c(0.5,0.85), 
            axis.text = element_text(size = 12), 
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_mean), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_mean), 
                 color = "grey55", linetype = "dashed")
  }
  
  ### FINAL plot -------------
  #png(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), res= 600, 
  #    width = 8, height =11, units = "in")
  #grid.arrange(p1, p2, p3, p4, ncol = 1)
  #panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'vh')
  #ggsave(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), panel,  
  #       dpi = 800, width = 8, height = 9.5)
  #dev.off()
  
  ifelse(option == 1 , 
         panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v'),
         ifelse(option == 2, 
                panel <- plot_grid(p1, p4, ncol = 1, align = 'v'), 
                ifelse(option == 3, 
                       panel <- plot_grid(p2, p3, ncol = 1, align = 'v'), 0)))
  ggsave(paste0('./figures/rkc/',cur_yr, '/', survey.location, '_', cur_yr, '_', 
                            option, '.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}
  

## NC panel figure ---------------
panel_figure_NC <- function(survey.location, cur_yr, base.location, option, scale){
  # survey.location and baseline.location are the same is most areas.  Check
  # baseline file to see if they differ
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females), 
  # scale - created for Seymour Canal scaling issues
  CPUE_wt_graph <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                   '/cpue_wt_since_95.csv'))
  poorclutch_summary <- read.csv(paste0('./results/rkc/', survey.location, 
                                        '/', cur_yr, '/poorclutch_summary_all.csv'))
  egg_mean_all <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                  '/egg_percent_mean_all.csv'))
  # file with year and mean percent poor clutch and se poor clutch from 1995 to current
  mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv')
  baseline <- read.csv("./data/rkc/longterm_means.csv")
  biomass <- read.csv("./data/rkc/biomass.csv") 
  conf <- read.csv("./data/rkc/confidential_harvest_2018.csv")
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  # mr adjustments can be made in the function using mr_adjust file.
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_wt", 
                                                        "recruit", ifelse(recruit.status == "PreR_SE", 
                                                                          "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                                                                "recruit", "post.recruit "))))) %>% 
    mutate(type = ifelse(recruit.status == "PreR_SE",
                         "se", 
                         ifelse(recruit.status == "Rec_SE", 
                                "se", ifelse(recruit.status == "PR_SE", 
                                             "se", "mean"))))-> males_long
  males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph
  
  ### females/juv prep ------------
  CPUE_wt_graph %>% 
    select(Year,Juvenile_wt, SmallF_wt, MatF_wt, 
           Juv_SE, SmallF_SE, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Juvenile_wt",
                                  "juvenile.male", 
                                  ifelse(recruit.status == "SmallF_wt", 
                                         "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                                                                   "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                                                                           "juvenile.female", "mature.female"))))) %>% 
    mutate(type = ifelse(recruit.status == "Juv_SE",
                         "se", 
                         ifelse(recruit.status == "SmallF_SE", 
                                "se", ifelse(recruit.status == "MatF_SE", 
                                             "se", "mean"))))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(Location == base.location) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% 
    filter(Year >= 1995) %>% 
    mutate(Pclutch100 = Pclutch *100, 
           Pclutch.se100 = Pclutch.se*100) %>% 
    select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary95
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(Year >= 1995) -> egg_mean_all_95
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary95 %>% 
    left_join(egg_mean_all_95) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch100:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch100",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se100", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se100", "se", ifelse(vname == "egg.se", 
                                                                "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  ## biomass manipulations 
  
  # file for all locations.  Has legal biomass from CSA, harvest
  # mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
  # adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
  mr_adjust %>% 
    select(-X) %>% 
    mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
    select(-area) -> mr_adjust2
  
  ### harvest adjustments for confidential data --
  conf %>% 
    filter(year > 1992 & !is.na(survey.area)) %>% 
    mutate(Location = ifelse(survey.area == "lynn", "LynnSisters", 
                       ifelse(survey.area == 'excursion', "Excursion", 
                        ifelse(survey.area == 'gambier', 'Gambier',
                         ifelse(survey.area == 'juneau', 'Juneau', 
                          ifelse(survey.area == 'peril', 'Peril', 
                           ifelse(survey.area == 'pybus', 'Pybus', 
                            ifelse(survey.area == 'seymour', 'Seymour',"NA")))))))) %>% 
    select(Year = year, permits, confidential, Location) -> conf_summary 
  
  biomass %>% 
    left_join(mr_adjust2) %>% 
    mutate(adj.legal = legal.biomass*weighted_ADJ, 
           adj.mature = mature.biomass*weighted_ADJ) %>% 
    left_join(conf_summary) %>% 
    mutate(confidential = replace_na(confidential, 'no'))-> biomass
 
 if(survey.location != "Juneau") {
  biomass %>% 
    filter(Year >= 1995) %>% 
    mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
    mutate(harvest = as.numeric(harvest)) %>% 
    select(-weighted_ADJ, -permits, -confidential, -legal.biomass, -mature.biomass) %>% 
    gather(type, pounds, harvest:adj.mature, factor_key = TRUE) %>% 
    filter(Location == survey.location)  -> biomass_graph
  
  biomass_graph %>% 
    filter(Year <= 2007) %>% 
    spread(type, pounds) %>% 
    summarise(mature_adj_mean = mean(adj.mature), 
              legal_adj_mean = mean(adj.legal)) -> baseline_means
 }
  
  if(survey.location == "Juneau"){
    biomass %>% 
      filter(Year >= 1995) %>% 
      mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
      mutate(harvest = as.numeric(harvest)) %>% 
      select(-weighted_ADJ, -permits, -confidential, -adj.legal, -adj.mature) %>% 
      gather(type, pounds, harvest:mature.biomass, factor_key = TRUE) %>% 
      filter(Location == survey.location)  -> biomass_graph
    biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_mean = mean(mature.biomass), 
                legal_mean = mean(legal.biomass)) -> baseline_means
  }
  
  
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(colour = recruit.class, shape = recruit.class, 
                   fill = recruit.class), size =3) +
    geom_line(aes(group = recruit.class, colour = recruit.class))+
    #scale_colour_manual(name = "", values = c("grey1", "grey65", "grey34"))+
    #scale_fill_manual(name = "", values = c("grey1", "grey65", "grey34")) +
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    scale_y_continuous(breaks = seq(min(0),max((max(males_graph$mean) + max(males_graph$se))), by = 1)) +
    #ylim(0,(max(males_graph$mean) + max(males_graph$se))) + 
    ggtitle(survey.location) + ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
    #              width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75) +
    geom_hline(yintercept = baseline2$Recruit, color = "#56B4E9", 
               linetype = "longdash", lwd = 0.75) +
    geom_hline(yintercept = baseline2$Post_Recruit, color = "#999999", 
               lwd = 0.75) +
    theme(legend.position = c(0.8,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size = 14,face="bold"), 
          plot.title = element_text(size = 24))
  
  
  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    #scale_colour_manual(name = "", values = c("grey34","grey62", "grey1"))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(17, 16, 15))+
        #ylim(0,25) + 
    scale_y_continuous(limits = c(0,25), oob = rescale_none) +
    ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
    #              width =.4) +
    geom_hline(yintercept = baseline2$Juvenile, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Small.Female, color = "#999999", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Large.Female, color = "#56B4E9")+
    theme(legend.position = c(0.7,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0('Female/juvenile CPUE and egg health for ', survey.location)) +
      theme(plot.title = element_text(size = 24))
  }
  
  
  #### F1c Female eggs graph -----------
  p3 <- ggplot(female_egg_graph, aes(Year, mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                  width =.4) +
    geom_line(aes(color = female.egg)) +
    geom_point(aes(fill = female.egg, shape = female.egg), size =3) +
    
    scale_fill_manual(name = "", values = c("black", "gray100")) +
    scale_colour_manual(name = "", values = c("grey1", "black")) +
    scale_shape_manual(name = "", values = c(21, 21)) +
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    ylab("Percentage") + 
    xlab(NULL) +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    theme(legend.position = c(0.2,0.5), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) 
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Year")
  }
  
  ### biomass harvest graph --------------
  if(survey.location != "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds, 
                                                          na.rm = TRUE) + 25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds, 
                                                     na.rm = TRUE)+25000), by = 50000)) +
      theme(legend.position = c(0.8,0.8), 
            axis.text = element_text(size = 12), 
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_adj_mean), 
                 color = "grey55", linetype = "dashed")
    if(scale == 1){
      p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1600000),
                                   breaks= seq(min(0), max(1600000), by = 150000), oob = rescale_none)
    }
  }
  
  if(survey.location == "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds, 
                                                          na.rm = TRUE) + 25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds, 
                                                     na.rm = TRUE)+25000), by = 50000)) +
      theme(legend.position = c(0.8,0.9), 
            axis.text = element_text(size = 12), 
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_mean), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_mean), 
                 color = "grey55", linetype = "dashed")
  }
  
  ### FINAL plot -------------
  #png(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), res= 600, 
  #    width = 8, height =11, units = "in")
  #grid.arrange(p1, p2, p3, p4, ncol = 1)
  #panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'vh')
  #ggsave(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), panel,  
  #       dpi = 800, width = 8, height = 9.5)
  #dev.off()
  
  ifelse(option == 1 , 
         panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v'),
         ifelse(option == 2, 
                panel <- plot_grid(p1, p4, ncol = 1, align = 'v'), 
                ifelse(option == 3, 
                       panel <- plot_grid(p2, p3, ncol = 1, align = 'v'), 0)))
  ggsave(paste0('./figures/rkc/',cur_yr, '/', survey.location, '_', cur_yr, '_', 
                option, 'non_conf.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}


## Presentation NC panel figure ---------------
panel_figure_NC_PRES <- function(survey.location, cur_yr, base.location, option, scale, title){
  # survey.location and baseline.location are the same is most areas.  Check
  # baseline file to see if they differ
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females), 
  # scale - created for Seymour Canal scaling issues, 0 means do nothing, 1 is for Seymour, 2 is by 100,000 lbs
  # title for the figure. This could be different than the name given in the data files
  CPUE_wt_graph <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                   '/cpue_wt_since_95.csv'))
  poorclutch_summary <- read.csv(paste0('./results/rkc/', survey.location, 
                                        '/', cur_yr, '/poorclutch_summary_all.csv'))
  egg_mean_all <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                  '/egg_percent_mean_all.csv'))
  # file with year and mean percent poor clutch and se poor clutch from 1995 to current
  mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv')
  baseline <- read.csv("./data/rkc/longterm_means.csv")
  biomass <- read.csv("./data/rkc/biomass.csv") 
  conf <- read.csv("./data/rkc/confidential_harvest_2018.csv")
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  # mr adjustments can be made in the function using mr_adjust file.
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_wt", 
                                                        "recruit", ifelse(recruit.status == "PreR_SE", 
                                                                          "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                                                                "recruit", "post.recruit "))))) %>% 
    mutate(type = ifelse(recruit.status == "PreR_SE",
                         "se", 
                         ifelse(recruit.status == "Rec_SE", 
                                "se", ifelse(recruit.status == "PR_SE", 
                                             "se", "mean"))))-> males_long
  males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph
  
  ### females/juv prep ------------
  CPUE_wt_graph %>% 
    select(Year,Juvenile_wt, SmallF_wt, MatF_wt, 
           Juv_SE, SmallF_SE, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Juvenile_wt",
                                  "juvenile.male", 
                                  ifelse(recruit.status == "SmallF_wt", 
                                         "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                                                                   "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                                                                           "juvenile.female", "mature.female"))))) %>% 
    mutate(type = ifelse(recruit.status == "Juv_SE",
                         "se", 
                         ifelse(recruit.status == "SmallF_SE", 
                                "se", ifelse(recruit.status == "MatF_SE", 
                                             "se", "mean"))))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(Location == base.location) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% 
    filter(Year >= 1995) %>% 
    mutate(Pclutch100 = Pclutch *100, 
           Pclutch.se100 = Pclutch.se*100) %>% 
    select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary95
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(Year >= 1995) -> egg_mean_all_95
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary95 %>% 
    left_join(egg_mean_all_95) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch100:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch100",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se100", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se100", "se", ifelse(vname == "egg.se", 
                                                                "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  ## biomass manipulations 
  
  # file for all locations.  Has legal biomass from CSA, harvest
  # mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
  # adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
  mr_adjust %>% 
    select(-X) %>% 
    mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
    select(-area) -> mr_adjust2
  
  ### harvest adjustments for confidential data --
  conf %>% 
    filter(year > 1992 & !is.na(survey.area)) %>% 
    mutate(Location = ifelse(survey.area == "lynn", "LynnSisters", 
                             ifelse(survey.area == 'excursion', "Excursion", 
                                    ifelse(survey.area == 'gambier', 'Gambier',
                                           ifelse(survey.area == 'juneau', 'Juneau', 
                                                  ifelse(survey.area == 'peril', 'Peril', 
                                                         ifelse(survey.area == 'pybus', 'Pybus', 
                                                                ifelse(survey.area == 'seymour', 'Seymour',"NA")))))))) %>% 
    select(Year = year, permits, confidential, Location) -> conf_summary 
  
  biomass %>% 
    left_join(mr_adjust2) %>% 
    mutate(adj.legal = legal.biomass*weighted_ADJ, 
           adj.mature = mature.biomass*weighted_ADJ) %>% 
    left_join(conf_summary) %>% 
    mutate(confidential = replace_na(confidential, 'no'))-> biomass
  
  if(survey.location != "Juneau") {
    biomass %>% 
      mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
      mutate(harvest = as.numeric(harvest)) %>% 
      select(-weighted_ADJ, -legal.biomass, -mature.biomass, -permits, -confidential) %>% 
      gather(type, pounds, harvest:adj.mature, factor_key = TRUE) %>% 
      filter(Location == survey.location) %>% 
      filter(Year >= 1995) -> biomass_graph
    
    biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_adj_mean = mean(adj.mature), 
                legal_adj_mean = mean(adj.legal)) -> baseline_means
  }
  
  if(survey.location == "Juneau"){
    biomass %>% 
      mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
      mutate(harvest = as.numeric(harvest)) %>% 
      select(-weighted_ADJ, -adj.legal, -adj.mature, -permits, -confidential) %>% 
      gather(type, pounds, harvest:mature.biomass, factor_key = TRUE) %>% 
      filter(Location == survey.location) %>% 
      filter(Year >= 1995) -> biomass_graph
    
    biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_mean = mean(mature.biomass), 
                legal_mean = mean(legal.biomass)) -> baseline_means
  }
 
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(colour = recruit.class, shape = recruit.class, 
                   fill = recruit.class), size =3) +
    geom_line(aes(group = recruit.class, colour = recruit.class))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    scale_y_continuous(breaks = seq(min(0),max((max(males_graph$mean) + max(males_graph$se))), by = 1)) +
    #ylim(0,(max(males_graph$mean) + max(males_graph$se))) + 
    ggtitle(title) + ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Recruit, color = "#56B4E9", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "#999999", 
               lwd = 0.75)+
    theme(legend.position = c(0.8,0.8),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(size = 24))
  
  
  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_shape_manual(name = "", values = c(17, 16, 15))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    
    #ylim(0,25) + 
    scale_y_continuous(limits = c(0,25), oob = rescale_none) +
    ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    geom_hline(yintercept = baseline2$Juvenile, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Small.Female, color = "#999999", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Large.Female, color = "#56B4E9")+
    theme(legend.position = c(0.7,0.8), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0(title, " cont.")) +
      theme(plot.title = element_text(size = 24))
  }
  
  
  #### F1c Female eggs graph -----------
  p3 <- ggplot(female_egg_graph, aes(Year, mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                  width =.4) +
    geom_line(aes(color = female.egg)) +
    geom_point(aes(fill = female.egg, shape = female.egg), size =3) +
    
    scale_fill_manual(name = "", values = c("black", "gray100")) +
    scale_colour_manual(name = "", values = c("grey1", "black")) +
    scale_shape_manual(name = "", values = c(21, 21)) +
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    ylab("Percentage") + 
    xlab(NULL) +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    theme(legend.position = c(0.2,0.5), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold")) 
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Year")
  }
  
  ### biomass harvest graph --------------
  if(survey.location != "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds/100000, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (100,000 lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                                          na.rm = TRUE) + .25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                     na.rm = TRUE)+.25000), by = 0.5)) +
      theme(legend.position = c(0.8,0.8), 
            axis.text = element_text(size = 14), 
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5, 'lines'),
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean/100000), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_adj_mean/100000), 
                 color = "grey55", linetype = "dashed")
    if(scale == 1){
      p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1600000/100000),
                                   breaks= seq(min(0), max(1600000/100000), by = 1.5), oob = rescale_none)
    }
    
    if(scale == 2){
      p4 = p4 + scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                                    na.rm = TRUE) + .25000),
                                   breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                               na.rm = TRUE)+.25000), by = 1.0))
    }
  }
    
  if(survey.location == "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds/100000, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (100,000lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                                          na.rm = TRUE) + .25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                     na.rm = TRUE)+0.25000), by = 0.5)) +
      theme(legend.position = c(0.8,0.9), 
            axis.text = element_text(size = 14), 
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5, 'lines'),
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_mean/100000), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_mean/100000), 
                 color = "grey55", linetype = "dashed")
  }
  
  
  ### FINAL plot -------------
  #png(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), res= 600, 
  #    width = 8, height =11, units = "in")
  #grid.arrange(p1, p2, p3, p4, ncol = 1)
  #panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'vh')
  #ggsave(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), panel,  
  #       dpi = 800, width = 8, height = 9.5)
  #dev.off()
  
  ifelse(option == 1 , 
         panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v'),
         ifelse(option == 2, 
                panel <- plot_grid(p1, p4, ncol = 1, align = 'v'), 
                ifelse(option == 3, 
                       panel <- plot_grid(p2, p3, ncol = 1, align = 'v'), 0)))
  ggsave(paste0('./figures/rkc/', cur_yr, '/', survey.location, '_', cur_yr, '_', 
                option, 'non_conf_presentation.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}

## Adjustable lengend for Presentation NC panel figure ---------------
legend_panel_figure_NC_PRES <- function(survey.location, cur_yr, base.location, option, scale, title, l1, l2){
  # survey.location and baseline.location are the same is most areas.  Check
  # baseline file to see if they differ
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females), 
  # scale - created for Seymour Canal scaling issues, 0 means do nothing, 1 is for Seymour, 2 is by 100,000 lbs
  # title for the figure. This could be different than the name given in the data files
  CPUE_wt_graph <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                   '/cpue_wt_since_95.csv'))
  poorclutch_summary <- read.csv(paste0('./results/rkc/', survey.location, 
                                        '/', cur_yr, '/poorclutch_summary_all.csv'))
  egg_mean_all <- read.csv(paste0('./results/rkc/', survey.location, '/', cur_yr,
                                  '/egg_percent_mean_all.csv'))
  # file with year and mean percent poor clutch and se poor clutch from 1995 to current
  mr_adjust <- read.csv('./data/rkc/adj_final_stock_assessment.csv')
  baseline <- read.csv("./data/rkc/longterm_means.csv")
  biomass <- read.csv("./data/rkc/biomass.csv") 
  conf <- read.csv("./data/rkc/confidential_harvest_2018.csv")
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  # mr adjustments can be made in the function using mr_adjust file.
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    select(Year,Pre_Recruit_wt, Recruit_wt, Post_Recruit_wt, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_wt:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_wt",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_wt", 
                                                        "recruit", ifelse(recruit.status == "PreR_SE", 
                                                                          "pre.recruit", ifelse(recruit.status == "Rec_SE", 
                                                                                                "recruit", "post.recruit "))))) %>% 
    mutate(type = ifelse(recruit.status == "PreR_SE",
                         "se", 
                         ifelse(recruit.status == "Rec_SE", 
                                "se", ifelse(recruit.status == "PR_SE", 
                                             "se", "mean"))))-> males_long
  males_long %>% select (-recruit.status) %>% spread(type, value1) -> males_graph
  
  ### females/juv prep ------------
  CPUE_wt_graph %>% 
    select(Year,Juvenile_wt, SmallF_wt, MatF_wt, 
           Juv_SE, SmallF_SE, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, Juvenile_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Juvenile_wt",
                                  "juvenile.male", 
                                  ifelse(recruit.status == "SmallF_wt", 
                                         "juvenile.female", ifelse(recruit.status == "Juv_SE", 
                                                                   "juvenile.male", ifelse(recruit.status == "SmallF_SE", 
                                                                                           "juvenile.female", "mature.female"))))) %>% 
    mutate(type = ifelse(recruit.status == "Juv_SE",
                         "se", 
                         ifelse(recruit.status == "SmallF_SE", 
                                "se", ifelse(recruit.status == "MatF_SE", 
                                             "se", "mean"))))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(Location == base.location) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% 
    filter(Year >= 1995) %>% 
    mutate(Pclutch100 = Pclutch *100, 
           Pclutch.se100 = Pclutch.se*100) %>% 
    select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary95
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(Year >= 1995) -> egg_mean_all_95
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary95 %>% 
    left_join(egg_mean_all_95) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch100:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch100",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se100", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se100", "se", ifelse(vname == "egg.se", 
                                                                "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  ## biomass manipulations 
  
  # file for all locations.  Has legal biomass from CSA, harvest
  # mr.biomass is biomass adjusted using mark-recapture experiments for those years or previous years
  # adj.biomass applied the m/r adjusted that was current in 2016 to all previous years - just for visualization.
  mr_adjust %>% 
    select(-X) %>% 
    mutate(Location = ifelse(area == "St_James", "LynnSisters", as.character(area))) %>% 
    select(-area) -> mr_adjust2
  
  ### harvest adjustments for confidential data --
  conf %>% 
    filter(year > 1992 & !is.na(survey.area)) %>% 
    mutate(Location = ifelse(survey.area == "lynn", "LynnSisters", 
                             ifelse(survey.area == 'excursion', "Excursion", 
                                    ifelse(survey.area == 'gambier', 'Gambier',
                                           ifelse(survey.area == 'juneau', 'Juneau', 
                                                  ifelse(survey.area == 'peril', 'Peril', 
                                                         ifelse(survey.area == 'pybus', 'Pybus', 
                                                                ifelse(survey.area == 'seymour', 'Seymour',"NA")))))))) %>% 
    select(Year = year, permits, confidential, Location) -> conf_summary 
  
  biomass %>% 
    left_join(mr_adjust2) %>% 
    mutate(adj.legal = legal.biomass*weighted_ADJ, 
           adj.mature = mature.biomass*weighted_ADJ) %>% 
    left_join(conf_summary) %>% 
    mutate(confidential = replace_na(confidential, 'no'))-> biomass
  
  if(survey.location != "Juneau") {
    biomass %>% 
      mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
      mutate(harvest = as.numeric(harvest)) %>% 
      select(-weighted_ADJ, -legal.biomass, -mature.biomass, -permits, -confidential) %>% 
      gather(type, pounds, harvest:adj.mature, factor_key = TRUE) %>% 
      filter(Location == survey.location) %>% 
      filter(Year >= 1995) -> biomass_graph
    
    biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_adj_mean = mean(adj.mature), 
                legal_adj_mean = mean(adj.legal)) -> baseline_means
  }
  
  if(survey.location == "Juneau"){
    biomass %>% 
      mutate(harvest = ifelse(confidential == "no", harvest, 'na')) %>% 
      mutate(harvest = as.numeric(harvest)) %>% 
      select(-weighted_ADJ, -adj.legal, -adj.mature, -permits, -confidential) %>% 
      gather(type, pounds, harvest:mature.biomass, factor_key = TRUE) %>% 
      filter(Location == survey.location) %>% 
      filter(Year >= 1995) -> biomass_graph
    
    biomass_graph %>% 
      filter(Year <= 2007) %>% 
      spread(type, pounds) %>% 
      summarise(mature_mean = mean(mature.biomass), 
                legal_mean = mean(legal.biomass)) -> baseline_means
  }
  
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(colour = recruit.class, shape = recruit.class, 
                   fill = recruit.class), size =3) +
    geom_line(aes(group = recruit.class, colour = recruit.class))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    scale_y_continuous(breaks = seq(min(0),max((max(males_graph$mean) + max(males_graph$se))), by = 1)) +
    #ylim(0,(max(males_graph$mean) + max(males_graph$se))) + 
    ggtitle(title) + ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Recruit, color = "#56B4E9", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "#999999", 
               lwd = 0.75)+
    theme(legend.position = c(0.8, 0.8),
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(size = 24))
  
  
  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_shape_manual(name = "", values = c(17, 16, 15))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    
    #ylim(0,25) + 
    scale_y_continuous(limits = c(0,25), oob = rescale_none) +
    ylab("CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    geom_hline(yintercept = baseline2$Juvenile, color = "#E69F00", 
               linetype = "dotdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Small.Female, color = "#999999", 
               linetype = "longdash", lwd = 0.75)+
    geom_hline(yintercept = baseline2$Large.Female, color = "#56B4E9")+
    theme(legend.position = c(0.7,0.8), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0(title, " cont.")) +
      theme(plot.title = element_text(size = 24))
  }
  
  
  #### F1c Female eggs graph -----------
  p3 <- ggplot(female_egg_graph, aes(Year, mean)) + 
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
                  width =.4) +
    geom_line(aes(color = female.egg)) +
    geom_point(aes(fill = female.egg, shape = female.egg), size =3) +
    
    scale_fill_manual(name = "", values = c("black", "gray100")) +
    scale_colour_manual(name = "", values = c("grey1", "black")) +
    scale_shape_manual(name = "", values = c(21, 21)) +
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    ylab("Percentage") + 
    xlab(NULL) +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
    theme(legend.position = c(0.2,0.5), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 14), 
          axis.title=element_text(size=14,face="bold")) 
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Year")
  }
  
  ### biomass harvest graph --------------
  if(survey.location != "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds/100000, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (100,000 lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                          na.rm = TRUE) + .25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                     na.rm = TRUE)+.25000), by = 0.5)) +
      theme(legend.position = c(l1, l2), 
            axis.text = element_text(size = 14), 
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5, 'lines'),
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean/100000), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_adj_mean/100000), 
                 color = "grey55", linetype = "dashed")
    if(scale == 1){
      p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1600000/100000),
                                   breaks= seq(min(0), max(1600000/100000), by = 1.5), oob = rescale_none)
    }
    
    if(scale == 2){
      p4 = p4 + scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                                    na.rm = TRUE) + .25000),
                                   breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                               na.rm = TRUE)+.25000), by = 1.0))
    }
  }
  
  if(survey.location == "Juneau"){
    p4 <- ggplot(biomass_graph, aes(Year, pounds/100000, group = type))+ 
      geom_point(aes(color = type, shape = type), size =3) +
      geom_line(aes(color = type, group = type, linetype = type))+
      scale_colour_manual(name = "", values = c("grey1", "grey1", "grey55"))+
      scale_shape_manual(name = "", values = c(1, 18, 32))+
      scale_linetype_manual(name = "", values = c("blank", "solid", "solid")) +
      ylab("Biomass (100,000lbs)") + 
      xlab("Year") +
      theme(plot.title = element_text(hjust =0.5)) + 
      scale_x_continuous(breaks = seq(min(1995),max(cur_yr), by =2)) +
      scale_y_continuous(limits = c(0,max(biomass_graph$pounds/100000, 
                                          na.rm = TRUE) + .25000),
                         breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                     na.rm = TRUE)+0.25000), by = 0.5)) +
      theme(legend.position = c(l1, l2), 
            axis.text = element_text(size = 14), 
            legend.text = element_text(size = 20),
            legend.key.size = unit(1.5, 'lines'),
            axis.title=element_text(size=14,face="bold")) + 
      geom_hline(data = baseline_means, aes(yintercept = legal_mean/100000), color = "grey1")+
      geom_hline(data = baseline_means, aes(yintercept = mature_mean/100000), 
                 color = "grey55", linetype = "dashed")
  }
  
  
  ### FINAL plot -------------
  #png(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), res= 600, 
  #    width = 8, height =11, units = "in")
  #grid.arrange(p1, p2, p3, p4, ncol = 1)
  #panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'vh')
  #ggsave(paste0('./figures/redcrab/', survey.location, '_', cur_yr, '.png'), panel,  
  #       dpi = 800, width = 8, height = 9.5)
  #dev.off()
  
  ifelse(option == 1 , 
         panel <- plot_grid(p1, p2, p3, p4, ncol = 1, align = 'v'),
         ifelse(option == 2, 
                panel <- plot_grid(p1, p4, ncol = 1, align = 'v'), 
                ifelse(option == 3, 
                       panel <- plot_grid(p2, p3, ncol = 1, align = 'v'), 0)))
  ggsave(paste0('./figures/rkc/', cur_yr, '/', survey.location, '_', cur_yr, '_', 
                option, 'non_conf_presentation.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}