#K.Palof 10-4-17

#Functions for processing Tanner crab data from the red crab survey.  All areas (except NJ / SP) 
#     are included in one file so need to add a grouping variable for Area/Location

## load packages -----------
library(tidyverse)
library(weights)
library(broom)

library(stringr)
library(reshape2)
library(extrafont)
library(ggthemes)
library(plotrix)
library(SDMTools)
library(grid)
library(gridExtra)
library(FNGr)
library(scales)
library(cowplot)


#font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

theme_set(theme_sleek())

### short term function ----------------
#input is file with last four years of data summarized by pot
  #  and year - grouped to include all areas
short_t_tanner <- function(bypot_st, year) {
  bypot_st_long <- gather(bypot_st, mod_recruit, crab, Juvenile:Small.Females, factor_key = TRUE) 
  
  bypot_st_long %>% 
    group_by(AREA, mod_recruit) %>% 
    do(fit = lm(crab ~ Year, data = .)) ->short_term
  
  short_term %>%
    tidy(fit) -> short_term_slope
  
  short_term %>%
    glance(fit) ->short_term_out
  
  recruit_used <- c("Large.Females",  "Pre_Recruit", "Recruit","Post_Recruit")
  short_term_out %>%
    filter(mod_recruit %in% recruit_used) %>%
    select(AREA, mod_recruit, r.squared, p.value)->short_term_out2
  
  short_term_slope %>%
    filter(mod_recruit %in% recruit_used, term == 'Year') %>%
    rename(slope = estimate) %>% 
    select(AREA, mod_recruit, slope) %>%
    right_join(short_term_out2)->short_term_results # estimate here is slope from regression
  
  #Now need to add column for significance and score
  short_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/tanner/tanner_rkc/', year, '/shortterm.csv'))
}

### Long term function -------------------
# need current years data and file with long term means

long_ttest <- function(area, year, baseline, bypot){
  baseline %>% 
    filter(AREA == area) -> baseline_values
  bypot %>% 
    filter(AREA == area & Year == year) ->data.use
  lfem <- t.test(data.use$Large.Females, mu = baseline_values$Large.Female)
  prer <- t.test(data.use$Pre_Recruit, mu = baseline_values$Pre_Recruit)
  rec <- t.test(data.use$Recruit, mu = baseline_values$Recruit)
  postr <- t.test(data.use$Post_Recruit, mu = baseline_values$Post_Recruit)
  
  long_term <- matrix(nrow = 4, ncol = 3)
  rownames(long_term) <- c("large.female", "pre.recruit", "recruit", "post.recruit")
  colnames(long_term) <- c("mean", "p.value", "lt.mean")
  
  long_term[1,1] <-lfem$estimate
  long_term[1,2] <- lfem$p.value
  long_term[1,3] <- lfem$null.value
  long_term[2,1] <-prer$estimate
  long_term[2,2] <- prer$p.value
  long_term[2,3] <- prer$null.value
  long_term[3,1] <-rec$estimate
  long_term[3,2] <- rec$p.value
  long_term[3,3] <- rec$null.value
  long_term[4,1] <-postr$estimate
  long_term[4,2] <- postr$p.value
  long_term[4,3] <- postr$null.value
  
  long_term_results <- as.data.frame(long_term)
  
  long_term_results %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > lt.mean, 1,
                                ifelse(p.value <0.05 & mean < lt.mean, -1, 0))) %>% 
    mutate(recruit.status = c("large.female", "pre.recruit", "recruit", "post.recruit")) %>% 
    mutate( AREA = area) -> long_term_results #estimate is slope from regression
  
  # final results with score - save here
  #write_csv(long_term_results, paste0('results/redcrab/', area, '/longterm.csv'))
  long_term_results 
}

### function to loop long term function above ------------
long_loop_17 <- function(x, curyr){
  long_ttest(x, curyr, baseline = baseline, bypot = dat3)
}

## weight-length function -------------
weight_L <- function(Tdat1, cur_yr){
  weight_length <- data.frame(AREA =character(),  slope =numeric(), coeff = numeric())
  # Linear model is changed for each area
  #AREA = unique(Tdat1$AREA) #"LS" "PS" "EI" "GB" "PB" "SC"
  #slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10)
  #coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55)
  
  weight_length <- data.frame(AREA = unique(Tdat1$AREA), slope = c(2.86,3.13, 3.30, 3.26, 3.05, 3.10),
                              coeff = c(7.33, 8.69, 9.48, 9.39, 8.34, 8.55))
  Tdat1 %>%
    right_join(weight_length) %>%
    mutate(weight_lb = (exp((slope*log(Width.Millimeters)) - coeff ))*(2.2/1000))-> datWL
  Mature = c("Pre_Recruit", "Recruit", "Post_Recruit")
  Legal =c("Recruit", "Post_Recruit")
  datWL %>% 
    group_by(AREA, Year) %>% 
    filter(Sex.Code == 1) %>% 
    summarise(mature_lbs = wt.mean(weight_lb[mod_recruit %in% Mature], 
                                   Number.Of.Specimens[mod_recruit %in% Mature]), 
              legal_lbs = wt.mean(weight_lb[mod_recruit %in% Legal], 
                                  Number.Of.Specimens[mod_recruit %in% Legal]), 
              prer_lbs = wt.mean(weight_lb[mod_recruit == "Pre_Recruit"], 
                                 Number.Of.Specimens[mod_recruit == "Pre_Recruit"])) -> male_weights
  
  write.csv(male_weights, paste0('./results/tanner/tanner_rkc/', cur_yr, '/RKCS_weights.csv'))
  
}


### female percent poor clutch ---------
poor_clutch_long <- function(poorclutch_current, area){
  poorclutch_current %>% 
    filter(AREA == area) -> data.use
  lt_female <- t.test(data.use$var1, mu = 0.10)
  
  longt_female <- matrix(nrow = 1, ncol = 2)
  rownames(longt_female) <- c("large.female")
  colnames(longt_female) <- c("mean", "p.value")
  
  longt_female[1,1] <-mean(data.use$var1)
  longt_female[1,2] <- lt_female$p.value
  
  longt_female <- as.data.frame(longt_female)
  longt_female %>%
    mutate(significant = ifelse(p.value < 0.05 & mean > 0.10, -1,
                                ifelse(p.value <0.05 & mean < 0.10, 1, 0))) %>% 
    mutate(recruit.status = c("large.female")) %>% 
    mutate(area = area) -> longt_female #estimate is slope from regression
  
  longt_female
}

Fem_long_loop <- function(x){
  poor_clutch_long(poorclutch_current = poorclutch1_current, x)
}

poor_clutch_short <- function(females_all, year){
  females_all %>%
    filter(Year >= (year-3)) -> LgF_short # short term file has last 4 years in it
  #output this file as .csv to add to next year
  #write_csv(females_all, paste0('results/redcrab/', area,'/poorclutch_females_all.csv'))
  
  LgF_short %>% 
    group_by(AREA) %>% 
    do(fit = lm(var1 ~ Year, data =.)) -> fem_short
  fem_short %>% 
    tidy(fit) -> fem_short_slope
  fem_short %>% 
    glance(fit) -> fem_short_out
  
  fem_short_out %>% 
    select(AREA, r.squared, p.value) -> fem_short_term_out
  fem_short_slope %>% 
    filter(term == 'Year') %>% 
    rename(slope = estimate) %>% 
    select(AREA, slope) %>% 
    right_join(fem_short_term_out) -> fem_short_results
  
  #Now need to add column for significance and score
  fem_short_results %>%
    mutate(significant = ifelse(p.value < 0.05 & slope > 0, 1,
                                ifelse(p.value <0.05 & slope <0, -1, 0))) %>%
    mutate(score = 0.25*significant) -> short_term_results
  write_csv(short_term_results, paste0('results/tanner/tanner_rkc/', year, '/female_shortterm.csv'))
}


### total stock health table --------------
total_health <- function(area, year){
  longterm <- read_csv(paste0('results/tanner/', area, '/', year, '/long_term.csv'))
  shortterm <- read_csv(paste0('results/tanner/', area, '/',  year, '/shortterm.csv'))
  lt_female <- read_csv(paste0('results/tanner/', area, '/',  year, '/Female_long_term.csv'))
  short_female <- read_csv(paste0('results/tanner/', area, '/', year, '/female_shortterm.csv'))
  
  longterm %>% 
    group_by(AREA) %>% 
    summarise(lt_sigf = sum(significant)) -> t1
  shortterm %>% 
    group_by(AREA) %>% 
    summarise(sigf = sum(score)) -> t2
  lt_female %>% 
    group_by(area) %>% 
    summarise(lf_sigf = sum(significant)) %>% 
    mutate(AREA = area) %>% 
    select(AREA,lf_sigf) -> t3
  short_female %>% 
    group_by(AREA) %>% 
    summarise(sf_sigf = sum(score)) -> t4
  
  t1 %>% 
    left_join(t2) %>% 
    left_join(t3) %>% 
    left_join(t4) %>% 
    group_by(AREA) %>% 
    summarise(score = lt_sigf + sigf + lf_sigf + sf_sigf) -> sum_all
  
 # summary score add stock health status
  sum_all %>%
    mutate(health_status = ifelse(score < -4.25, "poor", 
                                  ifelse(score > -4.25 & score<= -1.75, 
                                   "below average", 
                                   ifelse(score > -1.75 & score <= 1.5, "moderate", 
                                    ifelse(score > 1.75 & score <= 4.25, "above average", 
                                     ifelse(score > 4.25, "healthy", "unknown")))))) %>% 
    mutate (harvest_per = ifelse(health_status == "poor", 0, 
                                 ifelse(health_status == "below average", 0.05, 
                                  ifelse(health_status == "moderate", 0.10, 
                                   ifelse(health_status == "above average", 0.15,
                                    ifelse(health_status == "healthy", 0.20, "unk")))))) -> stock_health
  #select ( - score_f) -> stock_health
  write_csv(stock_health, paste0('results/tanner/', area, '/', year, '/stock_health.csv'))
}


## CONF panel figure ---------------
panel_figure <- function(survey.location, cur_yr, area, option, conf){
  # survey.location here are codes: EI, PS, PB, GB, SC, LS
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
      # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
      # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  # confidential - whether to include confidential data, if "include" then include all data, 
  #               if "exclude" then remove confidential data (i.e. Non-conf graphs)
  CPUE_wt_graph <- read.csv(paste0('./results/tanner/tanner_rkc/', cur_yr,
                                   '/RKCS_CPUE_all.csv'))
  poorclutch_summary <- read.csv(paste0('./results/tanner/tanner_rkc/', cur_yr, '/RKCS_percent_low_clutch.csv'))
  egg_mean_all <- read.csv(paste0('./results/tanner/tanner_rkc/', cur_yr,
                                  '/RKCS_percent_clutch.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/tanner/tanner_rkc/longterm_means_TC.csv")
  biomass <- read.csv(paste0('./data/tanner/tanner_', cur_yr, '_biomassmodel.csv'))
  harvest <- read.csv(paste0('./results/tanner/tanner_comm_catch_97_', cur_yr,'_confid.csv')) # needs to be updated with
                                    # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest

  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    filter(AREA == survey.location) %>% 
    select(Year,Pre_Recruit_u, Recruit_u, Post_Recruit_u, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_u:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_u",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_u", 
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
  # current only mature females is graphed for tanner crab areas - why?  not sure check on this.
  CPUE_wt_graph %>% 
    filter(AREA == survey.location) %>% 
    select(Year, MatF_u, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_u:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(AREA == survey.location) ->baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    filter(AREA == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(AREA == survey.location) %>% 
    select(Year, mean, egg.se) -> egg_mean_all_a
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary_a %>% 
    left_join(egg_mean_all_a) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se", "se", ifelse(vname == "egg.se", 
                                                             "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  
  ## biomass manipulations -------------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  if(conf == "exclude"){
    harvest %>% 
      filter(confidential == "n") -> harvest
  }
  
  harvest %>% 
    filter(Year >= 1997) %>% 
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    left_join(harvest_a) %>% 
    select(Year, Area, Harvest = pounds, Legal.Biomass = Legal, Mature.Biomass = Mature) %>% 
    gather(type, pounds, Harvest:Mature.Biomass, factor_key = TRUE) %>% 
    filter(Area == area) -> biomass_graph
  
  biomass_graph %>% 
    filter(Year < 2007) %>% 
    spread(type, pounds) %>% 
    summarise(legal_mean = mean(Legal.Biomass), 
              mature_mean = mean(Mature.Biomass)) -> baseline_means
  
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class, 
                   fill = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_colour_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9"))+
    scale_fill_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9")) +
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    #scale_y_continuous(limits = c(0,(max(males_graph$mean) + max(males_graph$se))),
    #                   oob = rescale_none) +
    ggtitle(survey.location) + ylab("Mature male CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
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
    theme(legend.position = c(0.3,0.85), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(size = 24))
  
  
  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class, fill = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_colour_manual(name = "", values = c( "#56B4E9"))+
    scale_shape_manual(name = "", values = c(15))+
    scale_fill_manual(name = "", values = c("#56B4E9")) +
    #ylim(0,25) + 
    #scale_y_continuous(limits = c(0,25), oob = rescale_none) +
    ylab("Mature female CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
    #              width =.4) +
    geom_hline(yintercept = baseline2$Large.Female, color = "#56B4E9")+
    theme(legend.position = c(0.3,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0(area, ' - Females')) +
      theme(plot.title = element_text(size = 24))
  }
  
  
  #### F1c Female eggs graph -----------
  p3 <- ggplot(female_egg_graph, aes(Year, mean, group = female.egg, fill = female.egg)) + 
    #geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = female.egg), 
    #              width =.4) +
    geom_point(aes(fill = female.egg, shape = female.egg), size =3) +
    geom_line(aes(color = female.egg, group = female.egg)) +
    scale_shape_manual(name = "", values = c(21, 1)) +
    scale_colour_manual(name = "", values = c("grey1", "black")) +
    scale_fill_manual(name = "", values = c("black", "gray45")) +
    ylab("Percentage") + 
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
    
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    xlab(NULL) +
    geom_hline(yintercept = 10, color = "black") +
    theme(legend.position = c(0.2,0.5), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) 
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Survey Year")
  }
  
  ### biomass harvest graph --------------
  p4 <- ggplot(biomass_graph, aes(Year, y = pounds/100000, group = type))+ 
    geom_point(aes(color = type, shape = type), size =3) +
    geom_line(aes(color = type, group = type, linetype = type))+
    scale_colour_manual(name = "", values = c("grey1", "grey1", "grey48", "grey62"))+
    scale_shape_manual(name = "", values = c(1, 18, 32, 18))+
    scale_linetype_manual(name = "", values = c("blank", "solid", "solid", "dashed")) +
    ylab("Pounds (100,000 lbs)") + 
    xlab("Survey Year") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds/100000, 
                                                        na.rm = TRUE) + 0.25000),
                       breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                   na.rm = TRUE)+0.25000), by = 0.5)) +
    theme(legend.position = c(0.55,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) + 
    geom_hline(data = baseline_means, aes(yintercept = legal_mean/100000), color = "grey1", 
               linetype = "dashed")
    #geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean), color = "grey62", linetype = "dashed")
  #if(scale == 1){
  #  p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1400000),
  #                               breaks= seq(min(0), max(1400000), by = 50000), oob = rescale_none)
  #}
  
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
  
if(conf == "exclude"){  
  ggsave(paste0('./figures/tanner/tanner_rkc/',cur_yr,'/', survey.location, '_', cur_yr, '_', 
                option, '_nonconf.png'), panel,  
         dpi = 800, width = 8, height = 9.5)}
if(conf == "include"){
  ggsave(paste0('./figures/tanner/tanner_rkc/' ,cur_yr,'/', survey.location, '_', cur_yr, '_', 
                option, 'confidential.png'), panel,  
         dpi = 800, width = 8, height = 9.5)}
}


## Presentation CONF panel figure ---------------
panel_figure_pres <- function(survey.location, cur_yr, area, option, conf){
  # survey.location here are codes: EI, PS, PB, GB, SC, LS
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
  # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
  # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  # confidential - whether to include confidential data, if "include" then include all data, 
  #               if "exclude" then remove confidential data (i.e. Non-conf graphs)
  CPUE_wt_graph <- read.csv(paste0('./results/tanner/tanner_rkc/', cur_yr,
                                   '/RKCS_CPUE_all.csv'))
  poorclutch_summary <- read.csv(paste0('./results/tanner/tanner_rkc/', cur_yr, '/RKCS_percent_low_clutch.csv'))
  egg_mean_all <- read.csv(paste0('./results/RKCS_tanner/', cur_yr,
                                  '/RKCS_percent_clutch.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/tanner/tanner_rkc/longterm_means_TC.csv")
  biomass <- read.csv("./data/tanner/tanner_rkc/tanner_2018_biomassmodel.csv") 
  harvest <- read.csv("./results/tanner/tanner_rkc/tanner_comm_catch_97_2018_confid.csv") # needs to be updated with
  # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    filter(AREA == survey.location) %>% 
    select(Year,Pre_Recruit_u, Recruit_u, Post_Recruit_u, 
           PreR_SE, Rec_SE, PR_SE) -> males
  males_long <- gather(males, recruit.status, value1, Pre_Recruit_u:PR_SE, factor_key = TRUE)
  males_long %>% 
    mutate(recruit.class = ifelse(recruit.status == "Pre_Recruit_u",
                                  "pre.recruit", ifelse(recruit.status == "Recruit_u", 
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
  # current only mature females is graphed for tanner crab areas - why?  not sure check on this.
  CPUE_wt_graph %>% 
    filter(AREA == survey.location) %>% 
    select(Year, MatF_u, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_u:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(AREA == survey.location) ->baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    filter(AREA == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(AREA == survey.location) %>% 
    select(Year, mean, egg.se) -> egg_mean_all_a
  ## female egg data -------
  # combine these data sets for graphing.  Create one with means and one with SEs.
  poorclutch_summary_a %>% 
    left_join(egg_mean_all_a) -> female_egg
  female_egg_long <- gather(female_egg, vname, value1, Pclutch:egg.se, factor_key = TRUE)
  female_egg_long %>% 
    mutate(female.egg = ifelse(vname == "Pclutch",
                               "% poor clutch", 
                               ifelse(vname == "mean", 
                                      "total % clutch", ifelse(vname == "Pclutch.se", 
                                                               "% poor clutch", "total % clutch")))) %>% 
    mutate(type = ifelse(vname == "Pclutch.se", "se", ifelse(vname == "egg.se", 
                                                             "se", "mean"))) %>% 
    select (-vname) %>% 
    spread(type, value1) -> female_egg_graph
  
  ## biomass manipulations -------------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  if(conf == "exclude"){
    harvest %>% 
      filter(confidential == "n") -> harvest
  }
  
  harvest %>% 
    filter(Year >= 1997) %>% 
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    left_join(harvest_a) %>% 
    select(Year, Area, harvest = pounds, legal.biomass = Legal, mature.biomass = Mature) %>% 
    gather(type, pounds, harvest:mature.biomass, factor_key = TRUE) %>% 
    filter(Area == area) -> biomass_graph
  
  biomass_graph %>% 
    filter(Year < 2007) %>% 
    spread(type, pounds) %>% 
    summarise(legal_mean = mean(legal.biomass), 
              mature_mean = mean(mature.biomass)) -> baseline_means
  
  # Figure panel -----
  #### F1a mature male plot -----------
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_colour_manual(name = "", values = c("grey1", "grey62", "grey34"))+
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    #scale_y_continuous(limits = c(0,(max(males_graph$mean) + max(males_graph$se))),
    #                   oob = rescale_none) +
    ggtitle(area) + ylab("Mature male CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "grey65")+
    geom_hline(yintercept = baseline2$Recruit, color = "grey34")+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "black")+
    theme(legend.position = c(0.3,0.85), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 16), 
          axis.title=element_text(size=18,face="bold"), 
          plot.title = element_text(size = 24))
  
  
  ### F1b females/juvenile plot ---------------
  p2 <- ggplot(femjuv_graph, aes(Year, mean, group = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_colour_manual(name = "", values = c( "grey1"))+
    scale_shape_manual(name = "", values = c(15))+
    
    #ylim(0,25) + 
    #scale_y_continuous(limits = c(0,25), oob = rescale_none) +
    ylab("Mature female CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Large.Female, color = "black")+
    theme(legend.position = c(0.3,0.8), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 16), 
          axis.title=element_text(size=18,face="bold"))
  
  if(option == 3){
    p2 = p2 + ggtitle(paste0(area, ' - Females')) +
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
    geom_hline(yintercept = 10, color = "black") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    theme(legend.position = c(0.2,0.5), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 16), 
          axis.title=element_text(size=18,face="bold"))
  
  if(option ==1){
    p3 = p3 + theme(axis.text.x = element_blank())
  }
  if(option ==3){
    p3 = p3 + xlab("Survey Year")
  }
  
  ### biomass harvest graph --------------
  p4 <- ggplot(biomass_graph, aes(Year, y = pounds/100000, group = type))+ 
    geom_point(aes(color = type, shape = type), size =3) +
    geom_line(aes(color = type, group = type, linetype = type))+
    scale_colour_manual(name = "", values = c("grey1", "grey1", "grey48", "grey62"))+
    scale_shape_manual(name = "", values = c(1, 18, 32, 18))+
    scale_linetype_manual(name = "", values = c("blank", "solid", "solid", "dashed")) +
    ylab("Pounds (100,000 lbs)") + 
    xlab("Survey Year") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(breaks = seq(min(1993),max(cur_yr), by =2)) +
    scale_y_continuous(labels = comma, limits = c(0,max(biomass_graph$pounds/100000, 
                                                        na.rm = TRUE) + 0.25000),
                       breaks= seq(min(0), max(max(biomass_graph$pounds/100000, 
                                                   na.rm = TRUE)+0.25000), by = 0.5)) +
    theme(legend.position = c(0.25,0.8), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 16), 
          axis.title=element_text(size=18,face="bold")) + 
    geom_hline(data = baseline_means, aes(yintercept = legal_mean/100000), color = "grey1", 
               linetype = "dashed")
  #geom_hline(data = baseline_means, aes(yintercept = legal_adj_mean), color = "grey62", linetype = "dashed")
  #if(scale == 1){
  #  p4 = p4 + scale_y_continuous(labels = comma, limits = c(0,1400000),
  #                               breaks= seq(min(0), max(1400000), by = 50000), oob = rescale_none)
  #}
  
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
  
  if(conf == "exclude"){  
    ggsave(paste0('./figures/tanner/tanner_rkc/', survey.location, '_', cur_yr, '_', 
                  option, '_nonconf_presentation.png'), panel,  
           dpi = 800, width = 8, height = 9.5)}
  if(conf == "include"){
    ggsave(paste0('./figures/tanner/tanner_rkc/', survey.location, '_', cur_yr, '_', 
                  option, 'confidential_presentation.png'), panel,  
           dpi = 800, width = 8, height = 9.5)}
}

