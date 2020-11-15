# K.Palof 11-28-18
# Functions to create figures for Stephens Passage (Juneau) and North Juneau.  Need seperate functions due 
# how data is compiled.

#####Load -------------
source('./code/tanner_functions.R')

## CONF panel figure ---------------
panel_figure_J <- function(survey.location, cur_yr, area, abrv, option){
  # survey.location here are codes: Juneau, North Juneau
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
  # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
  # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # abrv = 'NJ' or 'SP'
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  CPUE_wt_graph <- read.csv(paste0('./results/tanner/nj_stp/', cur_yr,
                                   '/', abrv, '_CPUE_ALL.csv'))
  poorclutch_summary <- read.csv(paste0('./results/tanner/nj_stp/', cur_yr, '/', abrv, '_percent_low_clutch.csv'))
  egg_mean_all <- read.csv(paste0('./results/tanner/nj_stp/', cur_yr,
                                  '/', abrv, '_percent_clutch.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/tanner/tanner_tcs/longterm_means_TC.csv")
  baseline_rkc <- read.csv("./data/tanner/tanner_rkc/longterm_means_TC.csv")
  biomass <- read.csv(paste0('./data/tanner/tanner_', cur_yr, '_biomassmodel.csv'))
  harvest <- read.csv(paste0('./results/tanner/harvest/', cur_yr, '/tanner_comm_catch_97_', cur_yr,'_confid.csv')) # needs to be updated with
  # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  
  if(abrv == "SP"){
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% already selected for specific area using file name
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
  # current only mature females is graphed for tanner crab areas - why?  not sure check on this.
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% 
    select(Year, MatF_wt, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  }
  
  if(abrv == "NJ"){
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% already selected for specific area using file name
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
    #filter(Location == survey.location) %>% 
    select(Year, MatF_u, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_u:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  }
  
  # baseline cpue values -----
  baseline %>% 
    bind_rows(baseline_rkc) %>% 
    filter(AREA == abrv) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    #filter(Location == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    #filter(Location == survey.location) %>% 
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
  
  ## biomass manipulations -----------------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  harvest %>% 
    filter(Year >= 1997) %>%
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    merge(harvest_a, by = c("Year", "Area"), all = TRUE) %>% 
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
    ggtitle(area) + ylab("Mature male CPUE (number/pot)")+ xlab(NULL)+
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
    theme(legend.position = c(0.25,0.85), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
          plot.title = element_text(size = 24)) +
    expand_limits(y = 0)
  
  
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
    theme(legend.position = c(0.15,0.8), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold")) +
    expand_limits(y=0) #+
    #scale_y_continuous(limits = comma)
  
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
    #scale_fill_discrete(breaks = c("total % clutch", "% poor clutch")) +
    ylim(0,100) + 
    ylab("Percentage") + 
    xlab(NULL) +
    geom_hline(yintercept = 10, color = "black") +
    theme(plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_ribbon(aes(ymin = mean - se, ymax = mean + se), 
                alpha = 0.2) +
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
                                                   na.rm = TRUE)+0.25000), by = 1.0)) +
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
  ggsave(paste0('./figures/tanner/', cur_yr, '/', survey.location, '_', cur_yr, '_', 
                option, '.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}

## CONF NJ panel figure ---------------
panel_figure_nj <- function(survey.location, cur_yr, area, abrv, option){
  # survey.location here are codes: North Juneau
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
  # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
  # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # abrv = 'NJ' 
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  CPUE_wt_graph <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                   '/', abrv, '_CPUE_ALL_edited.csv'))
  poorclutch_summary <- read.csv(paste0('./results/nj_stp/', cur_yr, '/', abrv, '_precent_low_clutch_edited.csv'))
  egg_mean_all <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                  '/', abrv, '_percent_clutch_edited.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")
  biomass <- read.csv("./data/rkc_tanner/tanner_2018_biomassmodel.csv") 
  harvest <- read.csv("./results/tanner/tanner_comm_catch_98_2018.csv") # needs to be updated with
  # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% already selected for specific area using file name
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
    #filter(Location == survey.location) %>% 
    select(Year, MatF_u, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_u:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(AREA == abrv) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    #filter(Location == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    #filter(Location == survey.location) %>% 
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
  
  ## biomass manipulations -----------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  harvest %>% 
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    merge(harvest_a, by = c("Year", "Area"), all = TRUE) %>% 
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
  p1 <- ggplot(males_graph, aes(Year, mean, group = recruit.class))+ 
    geom_point(aes(color = recruit.class, shape = recruit.class), size =3) +
    geom_line(aes(color = recruit.class, group = recruit.class))+
    scale_colour_manual(name = "", values = c("grey1", "grey62", "grey34"))+
    scale_shape_manual(name = "", values = c(15, 16, 17))+
    #scale_y_continuous(limits = c(0,(max(males_graph$mean) + max(males_graph$se))),
    #                   oob = rescale_none) +
    ggtitle(area) + ylab("Mature male CPUE (number/pot)")+ xlab(NULL)+
    theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "grey65")+
    geom_hline(yintercept = baseline2$Recruit, color = "grey34")+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "black")+
    theme(legend.position = c(0.25,0.85), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"), 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Large.Female, color = "black")+
    theme(legend.position = c(0.15,0.9), 
          axis.text = element_text(size = 12), 
          axis.title=element_text(size=14,face="bold"))
  
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
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
                                                   na.rm = TRUE)+0.25000), by = 1.0)) +
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
  ggsave(paste0('./figures/tanner/', survey.location, '_', cur_yr, '_', 
                option, '.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}

## PRESENTATION CONF SP panel figure ---------------
panel_figure_jnu_pres <- function(survey.location, cur_yr, area, abrv, option){
  # survey.location here are codes: Juneau, North Juneau
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
  # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
  # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # abrv = 'NJ' or 'SP'
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  CPUE_wt_graph <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                   '/', abrv, '_CPUE_ALL.csv'))
  poorclutch_summary <- read.csv(paste0('./results/nj_stp/', cur_yr, '/', abrv, '_precent_low_clutch.csv'))
  egg_mean_all <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                  '/', abrv, '_percent_clutch.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/TCS/longterm_means_TC.csv")
  baseline_rkc <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")
  biomass <- read.csv("./data/rkc_tanner/tanner_2018_biomassmodel.csv") 
  harvest <- read.csv("./results/tanner/tanner_comm_catch_98_2018.csv") # needs to be updated with
  # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% already selected for specific area using file name
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
  # current only mature females is graphed for tanner crab areas - why?  not sure check on this.
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% 
    select(Year, MatF_wt, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_wt:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(AREA == abrv) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    #filter(Location == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    #filter(Location == survey.location) %>% 
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
  
  ## biomass manipulations -----------------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  harvest %>% 
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    merge(harvest_a, by = c("Year", "Area"), all = TRUE) %>% 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "grey65")+
    geom_hline(yintercept = baseline2$Recruit, color = "grey34")+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "black")+
    theme(legend.position = c(0.25,0.85), 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Large.Female, color = "black")+
    theme(legend.position = c(0.25,0.8), 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
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
                                                   na.rm = TRUE)+0.25000), by = 1.0)) +
    theme(legend.position = c(0.55,0.8), 
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
  ggsave(paste0('./figures/tanner/', survey.location, '_', cur_yr, '_', 
                option, '_presentation.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}

## PRESENTATION CONF NJ panel figure ---------------
panel_figure_nj_pres <- function(survey.location, cur_yr, area, abrv, option){
  # survey.location here are codes: North Juneau
  # area is used in biomass /harvest file:  Icy Strait, Glacier Bay, 
  # Holkham Bay, Thomas Bay, Stephens Passage, North Juneau, Lynn Sisters, Pybus Bay, 
  # Gambier Bay, Excursion Inlet, Peril Strait, Seymour Canal  
  # abrv = 'NJ' 
  # cur_yr is the current year
  # option refers to output from this function. 
  # Option 1 - all 4 on one file, Option 2 - just p1, p4 (males), 
  # Option 3 - p2,p3 (females)
  CPUE_wt_graph <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                   '/', abrv, '_CPUE_ALL_edited.csv'))
  poorclutch_summary <- read.csv(paste0('./results/nj_stp/', cur_yr, '/', abrv, '_precent_low_clutch_edited.csv'))
  egg_mean_all <- read.csv(paste0('./results/nj_stp/', cur_yr,
                                  '/', abrv, '_percent_clutch_edited.csv'))
  # file with year and mean percent poor clutch and se poor clutch 
  baseline <- read.csv("./data/rkc_tanner/longterm_means_TC.csv")
  biomass <- read.csv("./data/rkc_tanner/tanner_2018_biomassmodel.csv") 
  harvest <- read.csv("./results/tanner/tanner_comm_catch_98_2018.csv") # needs to be updated with
  # recent year - both biomass and harvest files.
  # file for all locations.  Has legal and mature biomass from current year CSA & harvest
  
  # prep data 
  ### Mature males
  # create data frame that has mature males - just means
  # data fame that has mature males - just SE
  CPUE_wt_graph %>% 
    #filter(Location == survey.location) %>% already selected for specific area using file name
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
    #filter(Location == survey.location) %>% 
    select(Year, MatF_u, MatF_SE) -> femjuv
  femjuv_long <- gather(femjuv, recruit.status, value1, MatF_u:MatF_SE, factor_key = TRUE)
  femjuv_long %>% 
    mutate(recruit.class = "mature.female") %>% 
    mutate(type = ifelse(recruit.status == "MatF_SE", 
                         "se", "mean"))-> femjuv_long
  femjuv_long %>% select (-recruit.status) %>% spread(type, value1) -> femjuv_graph
  
  # baseline cpue values -----
  baseline %>% 
    filter(AREA == abrv) -> baseline2
  
  ## poor clutch --------
  poorclutch_summary %>% # this data is coming in as a percentage not a ratio
    #filter(Location == survey.location) %>% 
    select(Year, Pclutch, Pclutch.se) ->poorclutch_summary_a
  ## mean egg percent -------
  egg_mean_all %>% 
    #filter(Location == survey.location) %>% 
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
  
  ## biomass manipulations -----------
  # file for all locations.  Has preR, legal, and mature biomass from CSAs
  harvest %>% 
    select(Year, Area = survey.area, pounds) ->harvest_a
  
  biomass %>% 
    merge(harvest_a, by = c("Year", "Area"), all = TRUE) %>% 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Pre_Recruit, color = "grey65")+
    geom_hline(yintercept = baseline2$Recruit, color = "grey34")+
    geom_hline(yintercept = baseline2$Post_Recruit, color = "black")+
    theme(legend.position = c(0.35,0.85), 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se, color = recruit.class), 
                  width =.4) +
    geom_hline(yintercept = baseline2$Large.Female, color = "black")+
    theme(legend.position = c(0.25,0.9), 
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
    scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) +
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
                                                   na.rm = TRUE)+0.25000), by = 1.0)) +
    theme(legend.position = c(0.55,0.8), 
          legend.text = element_text(size = 20),
          legend.key.size = unit(1.5, 'lines'),
          axis.text = element_text(size = 16), 
          axis.title=element_text(size=18,face="bold"))  + 
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
  ggsave(paste0('./figures/tanner/', survey.location, '_', cur_yr, '_', 
                option, 'presentation.png'), panel,  
         dpi = 800, width = 8, height = 9.5)
}
