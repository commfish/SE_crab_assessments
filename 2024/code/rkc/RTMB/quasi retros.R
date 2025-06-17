######################3
# RETROS RETROS RETROS (quasi) -for Juneau area
## CS asked what this analysis would look like if we did it past years
#started: 6/16/25
## author: Alex Reich
###########################
set.seed(100)
#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)
library(abind)

#load the functions
##source the draft rtmb function
source("code/RTMB_functions.R")

#load the starting data- I will copy-paste from the Juneau CSA setup
df_juneau_24_compare <- read.csv("CSA_excel/JNU_test.csv") #this is the analysis at the end of 2024
df <- read.csv("CSA_excel/JNU_test_2023to2024_replication.csv") #this is the analysis at the end of 2023

#put data into individual stored places for RTMB
YEARS <- df$Survey.Year
WEIGHTS <- df$Weight #weighing. MAy need to add one for the new year

#make the weights CV 6/16/25
CV <- sqrt(exp(1/(2*WEIGHTS))-1)
CV[is.na(CV)] <- 0 #replace NA with 0 #def want to weight these as 0. $leaving NA there might be ok too.

CATCH <- as.numeric(gsub(",", "", df$Catch..Number.)) #get rid of commas, ideally before...
#replace NA in catch with 0
CATCH[is.na(CATCH)] <- 0 #replace NA with 0

##there was some thing in the juneau csa excel readme about how calculating PU is not straightforward. So... check that plz

CATCH_MIDDATE <-as.Date(df$Catch.Mid.Date,format = "%d-%b-%y") #data wrangle catch middate into usable format
REF_DATE <- CATCH_MIDDATE[1]
#CATCH_MIDDATE <- as.numeric(format(CATCH_MIDDATE, "%j"))
CATCH_MIDDATE <- as.numeric(CATCH_MIDDATE - REF_DATE) #sequential added days

SURVEY_MIDDATE <-as.Date(df$Survey.Mid.Date,format = "%d-%b-%y") #data wrangle survey middate into usable format
#SURVEY_MIDDATE <- as.numeric(format(SURVEY_MIDDATE, "%j")) #nope, I want not julian
SURVEY_MIDDATE <- as.numeric(SURVEY_MIDDATE - REF_DATE) #sequential added days
#fill NA's with the value before
for (i in 2:length(SURVEY_MIDDATE)){
  if (is.na(SURVEY_MIDDATE[i])) {
    SURVEY_MIDDATE[i] <- SURVEY_MIDDATE[i-1]
  }
}

#TAU's
CATCH_SURVEY_TAU <- rep(0, length(CATCH_MIDDATE)) #create a vector of zeros
CATCH_SURVEY_TAU[1] <- 0 #first year as 0
for (i in 2:length(CATCH_MIDDATE)){
  if(CATCH [i-1] ==0){
    CATCH_SURVEY_TAU[i] <- 1 #if the catch is 0, the CATCH_SURVEY_TAU = 1 for the next year
  }
  else{
    CATCH_SURVEY_TAU[i] <- (abs(SURVEY_MIDDATE[i]-CATCH_MIDDATE[i-1]))/365 #same formula as excel to get the tao adjustor
  }
}

SURVEY_TAU <- rep(0, length(SURVEY_MIDDATE)) #create a vector of zeros
SURVEY_TAU[1] <- 0 #first year as 0
for (i in 2:length(SURVEY_MIDDATE)){
  SURVEY_TAU[i] <- (abs(SURVEY_MIDDATE[i]-SURVEY_MIDDATE[i-1]))/365 #same formula as excel to get the tao adjustor
}
#survey info
##survey CPUE (from summary table) #vectors over all the years
CPUE_prerec <- df$Pre.recruit
CPUE_rec <- df$Recruit
CPUE_postrec <- df$Post.recruit
#replace NA's in survey CPUEs with 0 #AGR 6/10/25 removed- I think I can make it esimate where NA's, and not loop over them in the likelihood
#CPUE_prerec[is.na(CPUE_prerec)] <- 0 #replace NA with 0
#CPUE_rec[is.na(CPUE_rec)] <- 0 #replace NA with 0
#CPUE_postrec[is.na(CPUE_postrec)] <- 0 #replace NA with 0

#pred survey CPUE
pred_CPUE_prerec <- df$Estimated.Prerecruit
pred_CPUE_rec <- df$Estimated.Recruits
pred_CPUE_postrec <- df$Estimated.Postrecruits

#mean of predicted prerecuits (the "recruitment") except the last year
R_bar_1 <- mean(pred_CPUE_prerec[1:(length(pred_CPUE_prerec)-1)]) #mean of prerecruit CPUE
#mean of predicted prerecruits("recruitment") for the last 10 years
#R_bar_2 <- mean(pred_CPUE_prerec[(length(pred_CPUE_prerec)-9):length(pred_CPUE_prerec)]) #mean of prerecruit CPUE

#vector of deviates from the mean precruit CPUE/ devaites from the average recruitment
Eps_R <- rep(0, length(pred_CPUE_prerec)) #create a vector of zeros
for (i in 1:length(pred_CPUE_prerec)){
  Eps_R[i] <- (pred_CPUE_prerec[i] - R_bar_1)/R_bar_1 #deviate from the mean
}


##data as an array for each stage and with columns for year, CPUE, and CV
array_prerec_cpue <- array(c(YEARS, CPUE_prerec, CV), #weights will be replaced with CV once I... do that calc
                           dim = c(nrow(df), 3), 
                           dimnames = list(NULL, c("YEARS", "CPUE_prerec", "CV")))

array_rec_cpue <- array(c(YEARS, CPUE_rec, CV), #weights will be replaced with CV once I... do that calc
                        dim = c(nrow(df), 3), 
                        dimnames = list(NULL, c("YEARS", "CPUE_rec", "CV")))

array_postrec_cpue <- array(c(YEARS, CPUE_postrec, CV), #weights will be replaced with CV once I... do that calc
                            dim = c(nrow(df), 3), 
                            dimnames = list(NULL, c("YEARS", "CPUE_postrec", "CV")))

array_all_stages <- abind::abind(array_prerec_cpue, array_rec_cpue, array_postrec_cpue, along = 3)

# Set dimnames for the third dimension (stage)
dimnames(array_all_stages)[[3]] <- c("Stage_1", "Stage_2", "Stage_3")
#remove the rows with NA's in the second column
array_all_stages <- array_all_stages[!is.na(array_all_stages[,2,1]), , ] #remove rows with NA's in the second column (missing years of data)


#params
##hm, this starting value going back all years isn;t quite accurate. Can be approximate, tho. FLAG**
#REC <- 82.7928907453614/100  #preR to R suvival rate #I took the starting value from the 2024 analysis
T12 <- 84.6347230128254/100  #starting value from 2023 to 2024 analysis (last year)
#q <- 104.187334848418/1000000 #catchability as a rate (est as not/100? IDK (see csa excel for what they do...)) #THIS IS ALLOWED TO CHANGE
q <- 105.557381539957/1000000 #from 2023 to 2024 analysis (last year)
S <- 0.32 #surval/mortality proxy


#set up some bins
t <- 47 # my counter. Will max length +1
w <- 45 #my counter for the observed array (2 less becuse there are two NA's in the data that I need to dance around)
i <-1

bin <- array(0, dim =c(29, 3) )  #my storage box of numbers. Pre-set to hold 20.

#for loop 
for (i in 1:29){ #Let's go back to 1995

#remove one row from the data
c <- t-i #ok this can be my counter
d <- w-i #my counter for the survet data


#remove the bottom row from the df, from everything relevant to the data list (below)
YEARS <- YEARS[1:c]
CATCH <- CATCH[1:c]
array_all_stages <- array_all_stages[c(1:d),,]
df <- df[c(1:c),] 

#load the parameters and data with that row removed
data <- list(
  YEARS = YEARS, #all years inclduing missing cpue data years
  CATCH = CATCH,
  survey_data = array_all_stages,
  wt_mature= df$Mature.Weight,
  wt_legal = df$Legal.Weight,
  wt_prerec = df$Prerecruit.Weight,
  SURVEY_TAU = SURVEY_TAU,
  CATCH_SURVEY_TAU = CATCH_SURVEY_TAU 
)

pars <- list(
  ln_mean_rec = log(1.7), 
  ln_Eps_R = log(rep(1, length(df$Survey.Year))), #very small value if additive. close to 1 if multiplicative
  ln_q = log(q), # catchability 
  ln_T12 = log(T12), # preR to R survival rate and molt rate, both
  S = S, # will fixed. Suvival/mortality
  ln_sigma_survey = log(0.25), #0.1
  ln_init_rec_cpue = log(pred_CPUE_rec[1]), #initial recruit CPUE, 
  ln_init_postrec_cpue= log(pred_CPUE_postrec[1]) #initial postrecruit CPUE,
)


map <- list()
map$S <- factor(NA) 
map$ln_mean_rec <- factor(NA)

#run the analysis
pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map=map) #idk if I need this step each time but better safe than sorry

#opt <- nlminb(pop_mod$par, pop_mod$fn, pop_mod$gr) #simpler option of running the model
opt <- TMBhelper::fit_tmb(obj = pop_mod, #**some warning about reordering parameters 6/13/25
                          fn = pop_mod$fn,
                          gr = pop_mod$gr, 
                          newtonsteps = 1, # do I WANT newtonsteps if I don't need them?? - actually I need at least one for the gradient to be decent
                          getsd = TRUE)


#store the result
box <- pop_mod$report(pop_mod$env$last.par.best)
Index_thisyear <- box$PredSrvIdx #prerec, legal, mature
bin[i,] = c(2025-i, Index_thisyear[c,2], Index_thisyear[c,3]) #switch cur_year+1 with 2025. Year, legal, mature.

}#end for loop

#data there?
bin #yep.
df_bin <- data.frame(bin)
colnames(df_bin) <- c("Year", "Legal_biomass", "Mature_biomass")

###Graph
#load in the "each year we predicted this" data
##hmm where was that - bring in from S drive.
##found it (Table 2 in the 2024 analysis)- made a new csv file just for compariosn
SSQ_retros_old <- read.csv("Juneau 2024 quasi retros.csv") #nonsensically, I can only put this file in the main folder. Thanks, github.
#get rid of commas in the dataframe
SSQ_retros_old$Legal.biomass <- as.numeric(gsub(",", "", SSQ_retros_old$Legal.biomass))
SSQ_retros_old$Mature.biomass <- as.numeric(gsub(",", "", SSQ_retros_old$Mature.biomass))
colnames(SSQ_retros_old) <- c("Year", "Legal_biomass", "Mature_biomass")
SSQ_retros_old$Method <- rep("SSQ", length(SSQ_retros_old$Year)) #add a method column)

#rename my bin
Likelihodd_retros_new <- df_bin
Likelihodd_retros_new$Method <- rep("Likelihood", length(Likelihodd_retros_new$Year)) #add a method column

#stack em
Compare_retros <- rbind(SSQ_retros_old, Likelihodd_retros_new) #stack the two dataframes

#graph the likelihood (new) vs sum of squares (old) quasi-retros
library(ggplot2)
ggplot() + aes()+
  geom_point(data=SSQ_retros_old, aes(x=Year, y=Legal_biomass), size=3, shape=15, color="darkgrey") +
  geom_line(data=SSQ_retros_old,aes(x=Year, y=Legal_biomass), color="darkgrey") +
  geom_point(data=Likelihodd_retros_new, aes(x=Year, y=Legal_biomass), size=3) +
  geom_line(data=Likelihodd_retros_new,aes(x=Year, y=Legal_biomass))+
  theme_bw()+
  labs(y="Legal biomass")

a<- ggplot(Compare_retros) + aes(x=Year, y=Legal_biomass, color=Method, shape=Method) +
  geom_point(size=3) +
  geom_line() +
  theme_bw() +
  labs(y="Legal biomass") +
  scale_color_manual(values=c("Likelihood"="blue", "SSQ"="darkgrey")) #color the lines
 


ggplot() + aes()+
  geom_point(data=SSQ_retros_old, aes(x=Year, y=Mature_biomass), size=3, shape=15, color="darkgrey") +
  geom_line(data=SSQ_retros_old,aes(x=Year, y=Mature_biomass), color="darkgrey") +
  geom_point(data=Likelihodd_retros_new, aes(x=Year, y=Mature_biomass), size=3) +
  geom_line(data=Likelihodd_retros_new,aes(x=Year, y=Mature_biomass))+
  theme_bw()+
  labs(y="Legal biomass")


b <- ggplot(Compare_retros) + aes(x=Year, y=Mature_biomass, color=Method, shape=Method) +
  geom_point(size=3) +
  geom_line() +
  theme_bw() +
  labs(y="Mature biomass") +
  scale_color_manual(values=c("Likelihood"="blue", "SSQ"="darkgrey")) #color the lines

library(patchwork)
a/b + plot_layout(guides="collect")

#meh, I kind of want everything on the same graph
#make compare retros pivot longer
library(tidyverse)
Compare_retros_long <- Compare_retros %>%
  pivot_longer(cols = c("Legal_biomass", "Mature_biomass"), 
               names_to = "Biomass_Type", 
               values_to = "Biomass_Value")

(compare_retros_graph <- ggplot(Compare_retros_long) + aes(x=Year, y=Biomass_Value, color=Method, shape=Biomass_Type) +
  geom_point(size=3) +
  geom_line() +
  theme_bw() +
  labs(y="Biomass") +
  scale_color_manual(values=c("blue", "darkgrey"))+
    labs(title = "Quasi-retros by year and method")) #color the lines

#save that graph
ggsave("2024/figures/compare_quasi_retros.png", plot = compare_retros_graph, width = 12, height = 6)



