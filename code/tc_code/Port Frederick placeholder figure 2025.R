### Port Frederick placeholder figure for TANNER ###
### 2025 - Alex Reich - alex.reich@alaska.gov

#in 2025 we surveyed Port Frederick for the first time since 2014
# and thus we have this year's Port Frederick data, but no recent years
# so I went for a deep dive in the Douglas shared drive. Douglas/Shellfish/RESEARCH/Tanner/Management Analysis/2015- this is the last year of the Port Frederick CSA
# I'm pulling these standardized CPUE values to make a graph for the management biologists. The standardization methodology may have changned in the years since,
##but before re-standardizing I'll need to ask Zane to fill in the pre-2007 strata years (they're NA) in OceanAK.

#And thus, I'll standardize this year's Port Fredereick Tanner CPUE, and plot it with the years up to 2015. To create a data visualization.
library(tidyverse)

##Here's the data up to 2015
PF_old_data <- read.csv("data/tanner/tanner_tcs/Port Frederick 2015_dragged from the depths of the S drive.csv")
names(PF_old_data)
PF_old_cpue <- PF_old_data %>% select(Survey.Year, Pre.recruit, Recruit, Post.recruit)
PF_old_cpue
#cool, now I have the old (and possibly outdated) CPUEs

#now where's my new data?
#gonna download direct from oceanAK, I think
#here's the survey data for Port Frederick Tanner just for 2025:
##It's the raw data, not the CPUE, so I'll need to run the CPUE standardization
PF_2025 <- read.csv("data/tanner/tanner_tcs/RKC survey CSA_FRED_tanner_25.csv")
View(PF_2025)


#current year cpue standardization
## Load ---------------------------------
source('./code/tanner_rkc_functions.R') 

## setup global ---------------
cur_yr <- 2025


#baseline - in theory the average from...1997 to 2006. needs to be recalculated if the std CPUE methodology has changed since 2015
scratch_baseline <- PF_old_cpue %>% summarize(Prerec_base = mean(Pre.recruit), Rec_base = mean(Recruit), Postrec_base = mean(Post.recruit) )


##
PF_2025 -> dat
dat %>%
  filter(Pot.Condition == "Normal"|Pot.Condition == "Not observed") -> dat1

dat1 %>%
  filter(Recruit.Status == "", Width.Millimeters >= 1) #no rows is good

##
dat1 %>%
  #filter(!is.na(Width.Millimeters)) %>%  # lots of hoops to jump through so that NA come out as missing and not NA
  mutate(mod_recruit = ifelse(Number.Of.Specimens ==0, 'No_crab', ifelse(Sex.Code ==1 & Width.Millimeters <110 & 
                                                                           !is.na(Width.Millimeters), 'Juvenile', 
                                                                         ifelse(Sex.Code ==1 & Width.Millimeters>109 & Width.Millimeters < 138 &
                                                                                  !is.na(Width.Millimeters),'Pre_Recruit', 
                                                                                ifelse(Sex.Code ==1 & Width.Millimeters > 137 & Width.Millimeters <170 &
                                                                                         !is.na(Width.Millimeters)& Shell.Condition.Code <4, 'Recruit',
                                                                                       ifelse((Sex.Code ==1 & !is.na(Width.Millimeters)) &
                                                                                                Width.Millimeters >169|(Shell.Condition.Code >3 & Width.Millimeters >137 & !is.na(Width.Millimeters)), 'Post_Recruit', 
                                                                                              ifelse(Sex.Code ==2 & Egg.Development.Code==4 & !is.na(Egg.Development.Code), 'Small.Females', 
                                                                                                     ifelse(Sex.Code ==2 & Width.Millimeters>0 & !is.na(Width.Millimeters), 'Large.Females', 
                                                                                                            ifelse(is.na(Width.Millimeters), 'Missing', 'Missing'))))))))) -> Tdat1

Tdat1 %>% 
  filter(mod_recruit == "Missing")  # check for data issues

Tdat1 %>% filter(is.na(mod_recruit)) #check for data issues

  ##
#summarize by pot
Tdat1 %>%
  group_by(Year, Pot.No, mod_recruit) %>% # use AREA here instead of location due to 
  #  multiple location names for one survey area
  summarise(crab = sum(Number.Of.Specimens)) %>% 
  filter(!is.na(mod_recruit)) -> dat2 #remove any NAs due to data issues.

dat3 <- dcast(dat2, Year + Pot.No ~ mod_recruit, sum, drop=TRUE)

#cpue for all years (well, for 2025)
dat3 %>%
  group_by(Year) %>%
  summarise(Pre_Recruit_u = mean(Pre_Recruit), PreR_SE = (sd(Pre_Recruit)/(sqrt(sum(!is.na(Pre_Recruit))))), 
            Recruit_u = mean(Recruit), Rec_SE = (sd(Recruit)/(sqrt(sum(!is.na(Recruit))))), 
            Post_Recruit_u = mean(Post_Recruit), PR_SE = (sd(Post_Recruit)/(sqrt(sum(!is.na(Post_Recruit))))),
            Juvenile_u = mean(Juvenile), Juv_SE = (sd(Juvenile)/(sqrt(sum(!is.na(Juvenile))))), 
            MatF_u = mean(Large.Females), MatF_SE = (sd(Large.Females)/(sqrt(sum(!is.na(Large.Females))))),
            SmallF_u = mean(Small.Females), SmallF_SE = (sd(Small.Females)/(sqrt(sum(!is.na(Small.Females)))))) -> CPUE_all

#cool, now I have my old cpue and my 2025 cpue for port frederick
CPUE_fred_25_male <- CPUE_all %>% select(Year, Pre_Recruit_u, Recruit_u, Post_Recruit_u) %>%
  dplyr::rename(Survey.Year = Year, Pre.recruit = Pre_Recruit_u, Recruit = Recruit_u, Post.recruit = Post_Recruit_u)

df_cpue_fred <- rbind(PF_old_cpue, CPUE_fred_25_male) %>%
  dplyr::rename(Year = Survey.Year, Prerecruit = Pre.recruit, Postrecruit = Post.recruit)
#nice, that's my dataframe for graphing

area = "Port Frederick"

#graph
PF_gg_scratch <- ggplot(df_cpue_fred)+ 
  geom_point(aes(y= Prerecruit, x= Year), shape=16, size =3, color = "#E69F00",
             fill = "#E69F00") +
  geom_line(aes(y= Prerecruit, x= Year), color = "#E69F00")+
  geom_point(aes(y= Recruit, x= Year), shape=17, size =3, color = "#56B4E9",
             fill = "#56B4E9") +
  geom_line(aes(y= Recruit, x= Year), color = "#56B4E9")+
  geom_point(aes(y= Postrecruit, x= Year), shape=15, size =3, color = "#999999",
             fill = "#999999") +
  geom_line(aes(y= Postrecruit, x= Year), color = "#999999")+
  
  annotate("text", label = area, 
           x = 2013.5, y = Inf, hjust = -0.05, vjust = 1.1,  # fine-tune the positioning
           size = 6, fontface = "bold"    
  )+ 
  
  ylab("CPUE (number/pot)")+ xlab(NULL)+
  #theme(axis.text.x = element_blank(), plot.title = element_text(hjust =0.5)) + 
  scale_x_continuous(limits = c(1997, cur_yr), breaks = seq(min(1993),max(cur_yr), by =2)) + #seq(min(1994...)) for even years and 1993 for odd years
  #scale_y_continuous(limits = c(0,20), breaks =c(0, 5, 10, 15, 20))+
  geom_hline(yintercept = scratch_baseline$Prerec_base, color = "#E69F00", 
             linetype = "dotdash", lwd = 0.75)+
  geom_hline(yintercept = scratch_baseline$Rec_base, color = "#56B4E9", 
             linetype = "longdash", lwd = 0.75)+
  geom_hline(yintercept = scratch_baseline$Postrec_base, color = "#999999",
             lwd = 0.75)+
  scale_y_continuous(labels = comma) +
  theme(legend.position = c(0.44,0.85), 
        axis.text = element_text(size = 12), 
        axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size = 24))


ggsave("figures/tanner/tanner_rkc/2025/PF_scratch.png", PF_gg_scratch, dpi = 800, width = 8, height = 4.75)






