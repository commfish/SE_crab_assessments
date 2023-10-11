# making panel figures individually for Lynn Sisters males and Gambier males, because the legend position otherwise obscures data
# also making Peril male and female figures individually to remove fake 2023 data point (since the area wasn't surveyed in 2023)


## load -------------------------
source('./code/functions.R')  #functions here for summarizing data and figures 

## setup global ---------------
cur_yr <- 2023 # this needs to be updated annually with current survey year 
pr_yr <- cur_yr -1
cur_yr2 <- 23
pr_yr2 <- 22




## Lynn Sisters males figure - special for 2023 ---------------

survey.location <- 'LynnSisters'

panel_figure_LS <- function(survey.location, cur_yr, base.location, option, scale){
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
    theme(legend.position = c(0.3,0.8), 
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

panel_figure_LS('LynnSisters', cur_yr, 'LynnSisters', 2, 0) # male panel


## Gambier Bay males figure - special for 2023 ---------------

survey.location <- 'Gambier'

panel_figure_GB <- function(survey.location, cur_yr, base.location, option, scale){
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
    theme(legend.position = c(0.3,0.8), 
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

panel_figure_GB('Gambier', cur_yr, 'Gambier', 2, 0) # male panel

## Peril Strait male and female figures - special for 2023 ---------------

survey.location <- 'Peril'

panel_figure_PS <- function(survey.location, cur_yr, base.location, option, scale){
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
    filter(Year != 2023) %>%
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
    filter(Year != 2023) %>%
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
    filter(Year != 2023) %>%
    filter(Year >= 1995) %>% 
    mutate(Pclutch100 = Pclutch *100, 
           Pclutch.se100 = Pclutch.se*100) %>% 
    select(Year, Pclutch100, Pclutch.se100) ->poorclutch_summary95
  ## mean egg percent -------
  egg_mean_all %>% 
    filter(Year != 2023) %>%
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
      filter(Year != 2023) %>%
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


panel_figure_PS('Peril', cur_yr, 'Deadman Reach', 2, 0) # male panel
panel_figure_PS('Peril', cur_yr, 'Deadman Reach', 3, 0) # female panel

