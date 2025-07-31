###Excursion inlet RTMB CSA Generalized###
##Alex Reich
##7/31/25

#WARNING: MAKE SURE PRERECRUITS ARENT OVERFITTING PLEASE!!

##############################################################################
#TO DO
##make non-specific to Juneau so I can use it as a template for other areas
##might have to make for excursion first

#WORK TO DO
## confidence intervals - they go negative and they should not be doing that.
## the Excel CSA that I am replicating uses GRG nonlinear for solver - find out if I need to replicate that here
### that could totally be because GRG nonlinear is the default option... and thus it would not be necessary to replicate

#############

#BASIC STRUCTURE
##setup with data
##wrangle data
##figuring out which parameters/data I need
##put those data and parameters in RTMB-ok form
##map - fix parameters
##RTMB function model
##run RTMB
##look at RTMB outputs and convergence
##graph things (Excel CSA 2024 output compared to RTMB CSA 2024 output (I re-did the 2024 analysis in RTMB, essentially))

##################################################################################


###input files - can be improved in  the future
#I'm trying to figure out the best way... to adjust the CSA excel to a regular CSV input
##adding in new values manually for now (best way?? probs not but I can adjust later)

#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)
library(abind)

#set seed for reproducibility
set.seed(100)

#######
#DATA
#######

#load in data
#df_juneau_24_compare <- read.csv("CSA_excel/JNU_test.csv") #this is the analysis at the end of 2024  TEST FROM EARLIER MODELS
#df1 <- read.csv("CSA_excel/JNU_test_2023to2024_replication.csv") #this is the analysis at the end of 2023 TEST FROM EARLIER MODELS
df <- read.csv("CSA_excel/Excursion Inlet 2025 input data for RTMB starting values.csv") #for initial years of running the RTMB, the data input will have to be saved from the 2024 csv

#TO AVOID A VERY UNESSESARY DATA HEADACHE, THE BELOW INFO WILL BE AUTOMATED IN THE FUTURE AND NOT RIGHT NOW.
#middate_catch_curyr <- read.csv("results/rkc/Juneau/pu_midpoint_2025.csv") #UPDATE year each year
#middate_c <- middate_catch_curyr[2]
#as.Date(middate_c$pu.midpoint, format = "%d-%b-%y") #convert to date format

#survey_middate_curyr <- read.csv("results/rkc/Juneau/2025/survey_midpoint_2025.csv") #UPDATE year each year
#middate_s <- survey_middate_curyr[2]

this_year_cpue <- read.csv("results/rkc/Excursion/2025/EI_CPUE_2025.csv")%>% #change for each area. update each year
  filter(Year==2025)
this_year_crab_weights <- read.csv("results/rkc/Excursion/2025/maleweights.csv") %>% #change for each area. update each year
  filter(Year==2025)

df$Catch..Number.[46] <- 0 #this needs to be automated.. both for the row number updated (curyr-1) and the PU number( from the PU R code output)
df$Catch.Mid.Date[46] <- NA #dates are a pain. Automate later


#get rid of some non-relevant stuff
df <- df %>% select(Survey.Year, Catch.Season, Pre.recruit, Recruit, Post.recruit, Catch..Number., Catch.Mid.Date, Survey.Mid.Date, Mature.Weight, Legal.Weight, Prerecruit.Weight, Weight,
                    Estimated.Recruits, Estimated.Postrecruits) #select only the columns I need

#add another line for the new data
##OK I should automate more of this below.... *FLAG*
new_line <- data.frame(
  Survey.Year = 2025, #this year UPDATE EACH YEAR
  Catch.Season = "2025/26", #this year and next year UPDATE EACH YEAR
  #Juv..Male = 3.176402, #cpue from summary this year
  Pre.recruit = this_year_cpue$Pre_Recruit_wt, #cpue from summary this yera
  Recruit = this_year_cpue$Recruit_wt, #cpue from summary this yera
  Post.recruit = this_year_cpue$Post_Recruit_wt ,#cpue from summary this yera
  #legal = , #I actually dont think I use this column
  Catch..Number. = NA, #no info for the last year
  Catch.Mid.Date = NA, #no info in most recent year (the fishing hasnt started yet!)
  Survey.Mid.Date = "19-Jul-25", #dates are a pain. Automate when I have some more time **FIX**
  Mature.Weight = this_year_crab_weights$mature_lbs ,
  Legal.Weight = this_year_crab_weights$legal_lbs,
  Prerecruit.Weight = this_year_crab_weights$prer_lbs,
 # Catch..Survey.Tau =  0,#this gets calced?? FLAG!!
#  Survey.Tau = 0, #this gets calced?? FLAG!!
  #Estimated.Prerecruits = df$Estimated.Prerecruits[46] , #starting value will be same as last year. UPDATE EACH YEAR
  Estimated.Recruits = 0,
  Estimated.Postrecruits = 0,
  Weight = 12#by the established subjective weighting mechanism
  #Prerecruit.Biomass = 0,
  #Legal.Biomass = 0,
  #Mature.Biomass = 0,
  #GHL..pounds. = 0
 
)

df<- rbind(df,new_line) 


#put data into individual stored places for RTMB
YEARS <- df$Survey.Year
WEIGHTS <- df$Weight  #weighing. MAy need to add one for the new year

WEIGHTS[is.na(WEIGHTS)] <- 0.1#0.0001 #teh weights are 0 but lets not let the CV be inf. AGR

#make the weights CV 6/16/25
CV <- sqrt(exp(1/(2*WEIGHTS))-1)
#CV[is.na(CV)] <- 0 #replace NA with 0 #def want to weight these as 0. $leaving NA there might be ok too. FLAG!! might have to re-activate this

CATCH <- as.numeric(gsub(",", "", df$Catch..Number.)) #get rid of commas, ideally before...
#replace NA in catch with 0
CATCH[is.na(CATCH)] <- 0 #replace NA with 0

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

#temp quick fix!!
CATCH_MIDDATE[34] <- CATCH_MIDDATE[33]

#TAU's
CATCH_SURVEY_TAU <- rep(0, length(CATCH_MIDDATE)) #create a vector of zeros
CATCH_SURVEY_TAU[1] <- 0.5815 #first year as 0 #wait why is this?? **FLAG** I gave it a starting value from EI
for (i in 2:length(CATCH_MIDDATE)){
  if(CATCH [i-1] ==0){
    CATCH_SURVEY_TAU[i] <- 1 #if the catch is 0, the CATCH_SURVEY_TAU = 1 for the next year
  }
  else{
  CATCH_SURVEY_TAU[i] <- (abs(SURVEY_MIDDATE[i]-CATCH_MIDDATE[i-1]))/365 #same formula as excel to get the tao adjustor
  }
}

#TROUBLESHOOT TEMP - oh I see it. Year 2012 is missing a catch mid-year date. I can put in a temp one for now and dig that up later
##need to fish out the commfish data... FLAG. for now I'll give it 2011



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
CPUE_prerec[is.na(CPUE_prerec)] <- 0 #replace NA with 0
CPUE_rec[is.na(CPUE_rec)] <- 0 #replace NA with 0
CPUE_postrec[is.na(CPUE_postrec)] <- 0 #replace NA with 0

#pred survey CPUE
#pred_CPUE_prerec <- df$Estimated.Prerecruit #AGR 25 tunrned off - extra I think
pred_CPUE_rec <- df$Estimated.Recruits
pred_CPUE_postrec <- df$Estimated.Postrecruits

#mean of predicted prerecuits (the "recruitment") except the last year
#R_bar_1 <- mean(pred_CPUE_prerec[1:(length(pred_CPUE_prerec)-1)]) #mean of prerecruit CPUE
#mean of predicted prerecruits("recruitment") for the last 10 years
#R_bar_2 <- mean(pred_CPUE_prerec[(length(pred_CPUE_prerec)-9):length(pred_CPUE_prerec)]) #mean of prerecruit CPUE

#vector of deviates from the mean precruit CPUE/ devaites from the average recruitment
#Eps_R <- rep(0, length(pred_CPUE_prerec)) #create a vector of zeros
#for (i in 1:length(pred_CPUE_prerec)){
#  Eps_R[i] <- (pred_CPUE_prerec[i] - R_bar_1)/R_bar_1 #deviate from the mean
#}


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



##########
#PARAMS
###########
T12 <- 32.55515817/100 #starting value for the 2025 analysis Excursion Inlet (automate later!!) #Q2 in excel
q <- 321.1311767/1000000 #starting point for 2025 analysis from excursion inlet (Q3 in excel - excusrion 2024 excel CSA)
S <- 0.32 #fixed

####################
#SETUP
#######################
data <- list(
  YEARS = YEARS, #all years including missing cpue data years
  #lambdas = WEIGHTS, AGR off 6/12/25- now this is in the cpue data (and I still need to convert to CV...)
  CATCH = CATCH,
  #log_survey_data = log(array_all_stages), #log makes it not go negative? try it
  survey_data = array_all_stages,
  wt_mature= df$Mature.Weight,
  wt_legal = df$Legal.Weight,
  wt_prerec = df$Prerecruit.Weight,
  SURVEY_TAU = SURVEY_TAU,
  CATCH_SURVEY_TAU = CATCH_SURVEY_TAU 
  #vector_avoid_NA = as.numeric(no_NAs$rowname) #this is a vector of the years with data, to avoid NAs in the model
  
)

pars <- list(
  ln_mean_rec = log(1.1), #1.09 is the excursion inlet prerec mean . 1.7 was near the juneau prerecruit mean
  ln_Eps_R = log(rep(1, length(df$Survey.Year))), #very small value if additive. close to 1 if multiplicative. 
  ln_q = log(q), # catchability 
  ln_T12 = log(T12), # preR to R survival rate and molt rate, both
  S = S, #fixed!!survival. do I need to log??
  ln_sigma_survey = log(0.25), #0.1, #0.25 is my starting value
  #ln_InitDevs = rep(0, n_ages - 2), # Initial Recruitment penalty
  #ln_RecDevs = rep(0, n_yrs) # Recruitment penalty
  ln_init_rec_cpue = log(pred_CPUE_rec[1]), #initial recruit CPUE, to be estimated #log-tranform perhaps? can't go neg.
  ln_init_postrec_cpue= log(pred_CPUE_postrec[1]) #initial postrecruit CPUE, to be estimated #log-tranform perhaps? can't go neg.
)

#remove all values from the environment except pars and data:
rm(list = ls()[!(ls() %in% c("pars", "data"))]) #remove everything except pars and data


#data I pulled in up here for graphing relevance below
df <- data.frame(data$wt_prerec, data$wt_legal, data$wt_mature) 
names(df) <- c("wt_prerec", "wt_legal", "wt_mature") #prep for graphing


#map- to fix parameters!!
map <- list()
map$S <- factor(NA) 
#map$ln_q <- factor(NA) #AGR turn back off
#map$ln_sigma_R <- factor(NA) #prerecurits is overfit when I let this estimate
map$ln_mean_rec <- factor(NA) #fix mean recruitment - not dealing with this right now
#map$Eps_R <- factor(rep(NA, length(pars$Eps_R))) #fix recruitment deviates - a test
#map$ln_T12 <- factor(NA)

############################3
#the function will be called basic_pop_model. I'll put it in a different R script and source it here.

source("code/rkc_code/RTMB CSA/RTMB_CSA_generalize.R") #creates the model.

# Run Model ---------------------------------------------------------------

pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map=map) 
#pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map=map, random=c("Eps_R")) #if random effects. We decided no for this model.

##################
#troubleshoot 2 6/13/25
#out <- basic_pop_model(pars)
#out #that is a number
#map
#str(pars)
#str(map)

#TORUBLESHOOT
# Check initial parameter values
print(pop_mod$par)

# Evaluate the objective function and gradient at initial parameter values
initial_fn <- pop_mod$fn(pop_mod$par)
initial_gr <- pop_mod$gr(pop_mod$par)

print(initial_fn) #if NA's there is problem *FLAG!! There are problems here for excursion inlet 2025. I'll have to get into it unfortunately
print(initial_gr) #if NA's, problem

# Ensure no NA/NaN values in the function and gradient evaluations
#if (any(is.na(initial_fn)) || any(is.nan(initial_fn))) {
#  stop("Objective function evaluation returned NA/NaN values.")
#}

#if (any(is.na(initial_gr)) || any(is.nan(initial_gr))) {
#  stop("Gradient evaluation returned NA/NaN values.")
#}

##END TROUBLESHOOT
################

pop_mod$par #object and starting params
pop_mod$fn()
pop_mod$gr() #SOMETHING WENT BAD. AGR HERE


#OPTION 1 for model run - nlminb
#opt <- nlminb(pop_mod$par, pop_mod$fn, pop_mod$gr) #can I do more newtonsteps here? IDK
#opt #no convergence 6/13/25


#OPTION 2 for model run - fit_tmb - lets me use newton steps
opt <- TMBhelper::fit_tmb(obj = pop_mod, #**some warning about reordering parameters 6/13/25
                                fn = pop_mod$fn,
                               gr = pop_mod$gr, 
                               newtonsteps = 1, # additional steps helps get the gradient lower; the retros need 1 so I'll have 1 here too
                         getsd = TRUE) #can check out this model results, how different from nlmimb
opt



# Model summaries
sdrep <- sdreport(pop_mod)
sdrep #max gradient component should be <0.001 I beleive, to indicate convergence
summary(sdrep)

names(pop_mod)
pop_mod$report()

pop_mod$report()$sigma_survey 

pop_mod$gr()
pop_mod$report(pop_mod$env$last.par.best) #output
pop_mod$env$last.par.best 

#covariance graphs??
#cor <- cov2cor(solve(as.matrix(pop_mod$sd_rep$jointPrecision)))
#library(corrplot)
#corrplot(cor, type='lower')

#cor = cov2cor(pop_mod$sd_rep$cov.fixed)
#corrplot(cor, type='lower')


#space to compare 2025 excel to RTMB model AGR HERE 2025
#this could all be automated better...
#df_juneau_24_compare <- read.csv("CSA_excel/JNU_test.csv")
df_25_compare <- read.csv("CSA_excel/Excursion Inlet 2025 Excel results comparison.csv")
#get rid of commas in the numbers for prerecuit biomass, legal biomass, and mature biomass
df_25_compare$Mature.Biomass <- as.numeric(gsub(",", "", df_25_compare$Mature.Biomass))
df_25_compare$Legal.Biomass <- as.numeric(gsub(",", "", df_25_compare$Legal.Biomass))
df_25_compare$Prerecruit.Biomass <- as.numeric(gsub(",", "", df_25_compare$Prerecruit.Biomass))

Temp<- df_25_compare%>%
  filter(!is.na(Recruit)) #get rid of any years with missing survey data
Year <- c(min(Temp$Survey.Year):max(Temp$Survey.Year)) #get the years from the data

#EXTRACT FINAL VALUES
temp <- pop_mod$report(pop_mod$env$last.par.best)
class(temp[8])

temp2<- array(temp[[8]], dim=c(47,4,3)) #UPDATE THE DIM!! EACH YEAR!! FLAG FIX AGR!!!
cv_df <- data.frame(temp2[,c(1,3),1])
colnames(cv_df) <- c("year", "CV")
result_df <- data.frame(temp[-8])
#result_df <- left_join(result_df, )
#change result df names to prerecruit, recruit, postrectuit
names(result_df) <- c("sd","prerecruit_biomass", "legal_biomass", "mature_biomass", 
                      "prerecuit_cpue", "recruit_cpue", "postrecuit_cpue",
                      "jnll","q", "T12", "EpsR")
results_df_relevant <- result_df %>%
  #get confidence intervals on the cpue
  mutate(prerecuit_cpue_upper = prerecuit_cpue + (1.96 * sd),
         prerecuit_cpue_lower = prerecuit_cpue - (1.96 * sd),
         recruit_cpue_upper = recruit_cpue + (1.96 * sd),
         recruit_cpue_lower = recruit_cpue - (1.96 * sd),
         postrecuit_cpue_upper = postrecuit_cpue + (1.96 * sd),
         postrecuit_cpue_lower = postrecuit_cpue - (1.96 * sd),
         #import the weights in
         prerecuit_weight = df$wt_prerec,
         legal_weight = df$wt_legal,
         mature_weight = df$wt_mature,
        #get CI's on the biomass by converting the cpue upper and lower to biomass for prerecuit, legal, and mature
        ##double check these equations please
        prerecruit_biomass_upper = (prerecuit_cpue_upper * prerecuit_weight) / q,
        prerecruit_biomass_lower = (prerecuit_cpue_lower * prerecuit_weight) / q,
        legal_biomass_upper = (recruit_cpue_upper + postrecuit_cpue_upper) * legal_weight / q,
        legal_biomass_lower = (recruit_cpue_lower + postrecuit_cpue_lower) * legal_weight / q,
        mature_biomass_upper = (prerecruit_biomass_upper + legal_biomass_upper),
        mature_biomass_lower = (prerecruit_biomass_lower + legal_biomass_lower),
        year = Year)  #%>% #that could have been cleaner but I added year
        #left_join(cv_df) %>% #pull in the cv
        #mutate


#I should add CV to the raw data
df_25_compare <- df_25_compare %>% 
  mutate(year = Survey.Year) %>% #added a matching year column
  left_join(cv_df) %>% #joined cv
  mutate(
    cv_upper_prerec = Pre.recruit + CV, #adding upper and lower CV
    cv_lower_prerec = Pre.recruit - CV,
    cv_upper_rec = Recruit + CV,
    cv_lower_rec = Recruit - CV,
    cv_upper_postrec = Post.recruit + CV,
    cv_lower_postrec = Post.recruit - CV
  )

 

#add in blank rows for the missing years:
#results_df_relevant <- results_df_relevant %>%
#  mutate(year = as.numeric(year)) %>% #make sure year is numeric
#  complete(year = full_seq(year, 1), fill = list(prerecuit_cpue = NA, recruit_cpue = NA, postrecuit_cpue = NA,
 #                                                prerecruit_biomass = NA, legal_biomass = NA, mature_biomass = NA,
  #                                               prerecuit_cpue_upper = NA, recruit_cpue_upper = NA, postrecuit_cpue_upper = NA,
   #                                              prerecuit_cpue_lower = NA, recruit_cpue_lower = NA, postrecuit_cpue_lower = NA,
    #                                             legal_biomass_upper = NA, legal_biomass_lower = NA,
     #                                            mature_biomass_upper = NA, mature_biomass_lower = NA)) #fill in the missing years with NAs


  
#graph to compare observed survey values, excel CSA model, and RTMB CSA model
##results_df_relevant has my predicted values
##df_juneau_24_compare has the observed values
color_levels <- c("RTMB", "Excel", "Observed")
p1 <- ggplot(results_df_relevant) + aes(x=year, y=prerecuit_cpue) + 
  geom_ribbon(aes(ymin=prerecuit_cpue_lower, ymax=prerecuit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(aes(color =factor("RTMB", levels=color_levels)), linewidth=1) + #the model-predicted cpue
  geom_point(aes(color=factor("RTMB", levels=color_levels)))+
  #add the CSA excel model CPUE
  geom_line(data=df_25_compare ,aes(y=Estimated.Prerecruits, x=Survey.Year, color=factor("Excel", levels=color_levels),)) + #this is the observed survey CPUE values
  geom_point(data=df_25_compare, aes(y=Pre.recruit, x=Survey.Year, color=factor("Observed", levels=color_levels))) + #this is the observed survey CPUE values
  geom_errorbar(data=df_25_compare, aes(ymin=cv_lower_prerec, ymax=cv_upper_prerec, x=Survey.Year, y=NULL))+
  labs(title="EI Pre-recruit CPUE", x="Year", y="CPUE", subtitle = "observed CPUE as black points with CV error bars") +
  scale_color_manual(
    name=NULL,
    values = c(
    "RTMB" = "lightblue",
    "Excel" = "darkgreen",
    "Observed" = "black"
  )) +
  theme_minimal() +
  theme(  legend.position = c(0.95, 0.95),    # x=95% right, y=95% top inside plot area
         # legend.justification = c("right", "top"),  # anchor legend box by its top-right corner
         # legend.background = element_rect(fill = alpha("white", 0.7), color = "gray80"),  # semi-transparent background for readability
          legend.box.background = element_rect(color = "gray80")
         )

#recruits CPUE
p2 <- ggplot(results_df_relevant) + aes(x=year, y=recruit_cpue) + 
  geom_ribbon(aes(ymin=recruit_cpue_lower, ymax=recruit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", linewidth=1) + #the model-predicted cpue
  geom_point(color="lightblue")+
  geom_point(data=df_25_compare ,aes(y=Recruit, x=Survey.Year)) + #this is the observed survey CPUE values
  geom_errorbar(data=df_25_compare, aes(ymin=cv_lower_rec, ymax=cv_upper_rec, x=Survey.Year, y=NULL))+
  #add the CSA excel model CPUE
  geom_line(data=df_25_compare ,aes(y=Estimated.Recruits, x=Survey.Year), color = "darkgreen") + #this is the observed survey CPUE values
  labs(title="EI Recruit CPUE", x="Year", y="CPUE") +
  theme_minimal()

#and postrecruit CPUE
p3 <- ggplot(results_df_relevant) + aes(x=year, y=postrecuit_cpue) + 
  geom_ribbon(aes(ymin=postrecuit_cpue_lower, ymax=postrecuit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", linewidth=1) + #the model-predicted cpue
  geom_point(color="lightblue")+
  geom_point(data=df_25_compare ,aes(y=Post.recruit, x=Survey.Year)) + #this is the observed survey CPUE values
  geom_errorbar(data=df_25_compare, aes(ymin=cv_lower_postrec, ymax=cv_upper_postrec, x=Survey.Year, y=NULL))+
  #add the CSA excel model CPUE
  geom_line(data=df_25_compare ,aes(y=Estimated.Postrecruits, x=Survey.Year), color = "darkgreen") + #this is the observed survey CPUE values
  labs(title="EI Post-recruit CPUE", x="Year", y="CPUE") +
  theme_minimal()

library(patchwork)
(p123 <- p1/p2/p3)

cur_yr <- 2025 #the current year
#save plot to figures file
ggsave(paste0("figures/rkc/",cur_yr,"/CSA_EI_CPUE.png"), plot = p123, width = 8, height = 10, dpi = 300)



##################################################
#graph CSA excel biomass vs. RTMB biomass estimates
#mature biomass
color_levels_2 <- c("Mature RTMB", "Mature Excel", "Legal RTMB", "Legal Excel", "Prerecruit RTMB", "Prerecruit Excel")

p4<-ggplot(results_df_relevant) + aes(x=year, y=mature_biomass) + 
  geom_ribbon(aes(ymin=mature_biomass_lower, ymax=mature_biomass_upper), alpha = 0.3, fill = "#56B4E9") + #uncertainty
  geom_line(aes(color =factor("Mature RTMB", levels=color_levels_2)), size=1) + #the model-predicted RTMB biomasss
  geom_line(data=df_25_compare ,aes(y=Mature.Biomass, x=Survey.Year, color=factor("Mature Excel", levels=color_levels_2))) + #this is excel model mature biomass
  #add legal biomass
  geom_ribbon(aes(ymin=legal_biomass_lower, ymax=legal_biomass_upper), alpha = 0.2, fill = "#009E73") + #uncertainty
  geom_line(aes(y=legal_biomass,color =factor("Legal RTMB", levels=color_levels_2)), size=1) + #the RTMB model-predicted biomasss
  geom_line(data=df_25_compare ,aes(y=Legal.Biomass, x=Survey.Year, color=factor("Legal Excel", levels=color_levels_2))) + #this is the excel moodel
  #add prerecruit biomass
  geom_ribbon(aes(ymin=prerecruit_biomass_lower, ymax=prerecruit_biomass_upper), alpha = 0.2, fill = "#CC79A7") + #uncertainty
  geom_line(aes(y=prerecruit_biomass,color =factor("Prerecruit RTMB", levels=color_levels_2)), size=1) + #the RTMB model-predicted biomass
  geom_line(data=df_25_compare ,aes(y=Prerecruit.Biomass, x=Survey.Year, color=factor("Prerecruit Excel", levels = color_levels_2))) + #this is the excel-predicted biomass
  labs(title="JNU Mature, Legal, and Pre-recruit Biomass", x="Year", y="Biomass") +
  theme_minimal() +
  scale_color_manual(
    name=NULL,
    values = c(
      "Mature RTMB" = "#56B4E9",
      "Mature Excel" = "darkblue",
      "Legal RTMB" = "#009E73",
      "Legal Excel" = "darkgreen",
      "Prerecruit RTMB" = "#CC79A7",
      "Prerecruit Excel" = "#542788"
    )) +
  theme(  legend.position = c(0.15, 0.8),    # x=95% right, y=95% top inside plot area
          # legend.justification = c("right", "top"),  # anchor legend box by its top-right corner
           legend.background = element_rect(fill = alpha("white", 0.7), color = "gray80"),  # semi-transparent background for readability
          legend.box.background = element_rect(color = "gray80")
  )

p4
#ggsave to current year figures folder
ggsave(paste0("figures/rkc/",cur_yr,"/CSA_EI_Biomass.png"), plot = p4, width = 8, height = 5, dpi = 300)


#I want to see the p4 graph with a wider y axis
p5 <- p4 + ylim(-70000, 500000)
p5
ggsave(paste0("figures/rkc/",cur_yr,"/CSA_EI_Biomass2.png"), plot = p5, width = 8, height = 5, dpi = 300)

p7 <- p4 +ylim(-70000, 1000000)
p7
ggsave(paste0("figures/rkc/",cur_yr,"/CSA_EI_Biomass3.png"), plot = p7, width = 8, height = 5, dpi = 300)

#make the df in long format#######
# Combine datasets with source label for plotting

#end df long wrangle here#######

#what's this plot??
#p6<-ggplot(results_df_relevant) + aes(x=year, y=mature_biomass) + 
#  geom_ribbon(aes(ymin=mature_biomass_lower, ymax=mature_biomass_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
#  geom_line(color ="lightblue", size=1) + #the model-predicted RTMB biomasss
#  geom_line(data=df_juneau_24_compare ,aes(y=Mature.Biomass, x=Survey.Year), color="blue") + #this is excel model mature biomass
  #add legal biomass
#  geom_ribbon(aes(ymin=legal_biomass_lower, ymax=legal_biomass_upper), alpha = 0.2, fill = "lightgreen") + #uncertainty
#  geom_line(aes(y=legal_biomass),color ="lightgreen", size=1) + #the RTMB model-predicted biomasss
#  geom_line(data=df_juneau_24_compare ,aes(y=Legal.Biomass, x=Survey.Year), color="darkgreen") + #this is the excel moodel
  #add prerecruit biomass
#  geom_ribbon(aes(ymin=prerecruit_biomass_lower, ymax=prerecruit_biomass_upper), alpha = 0.2, fill = "lightpink") + #uncertainty
#  geom_line(aes(y=prerecruit_biomass),color ="lightpink", size=1) + #the RTMB model-predicted biomass
 # geom_line(data=df_juneau_24_compare ,aes(y=Prerecruit.Biomass, x=Survey.Year), color="pink") + #this is the excel-predicted biomass
#  labs(title="JNU Biomass - Excel dark lines, RTMB light lines with CI", x="Year", y="Biomass") +
 # scale_color_manual(name = "Biomass Type",
#                     values = c("Mature" = "lightblue", "Legal" = "lightgreen", "Prerecruit" = "lightpink")) +
#  scale_linetype_manual(name = "Model",
 #                       values = c("RTMB" = "solid", "Excel" = "dashed")) +
#  labs(title = "JNU Biomass – Excel (dashed) vs RTMB (solid) with 95% CI",
 #      x = "Year", y = "Biomass") +
#  theme_minimal() #not colorblind friendly

#p6


#EVERYTHING BELOW HERE IS IN DEVELOPMENT. iN THEORY I'LL PRODUCE WHATEVER RESULTS AND OUTPUT TABLES I NEED BELOW.
######################################################################
#agr here 25, more or less. Next I'll do the annoying tables and ... export the results (biomass) into a csv that I can do the rest of the analysis with.
#also Qc this analysis and send figs to C
#tranform results_df_relevant to the same strucutre as df_juneau_24_compare with the same column names
output_df <-  df_juneau_24_compare %>%
  select (-X, -GHL..pounds.) %>%
  mutate(Estimated.Prerecruits = results_df_relevant$prerecuit_cpue,
         Estimated.Recruits = results_df_relevant$recruit_cpue,
         Estimated.Postrecruits = results_df_relevant$postrecuit_cpue,
         Prerecruit.Biomass = results_df_relevant$prerecruit_biomass,
         Legal.Biomass = results_df_relevant$legal_biomass,
         Mature.Biomass = results_df_relevant$mature_biomass)
    #that should give me inputs for next year

#SAVE SAVE that output csv

#save biomass output for the restof the analysis- overwrite the old biomass file


##############3
#CALC GHL HERE
################
#it is mature biomass * 0.1 or whatever - a range for the tables. See excel tables and base off of those **TO DO NEXT**

##################################################################
#the update the biomass csv part
#####################################################################
output_df$Mature.Biomass
output_df$Legal.Biomass #here, can copy uncertainty in if I choose to do so




#Table 3 - add in mature and legal biomasses from this year.... and current year's legal weight
##this table will be a pain, might need to make 2 different ones
###table 3.1 - note that there will be some grammar of table wrangling in the output RMD file 
Legal_biomass_curyr <- output_df %>% 
  filter(Survey.Year == max(Survey.Year)) %>%
  select(Legal.Biomass)%>%
  unlist() #turns it into a numeric vector
Mature_biomass_curyr <- output_df %>% 
  filter(Survey.Year == max(Survey.Year)) %>%
  select(Mature.Biomass)%>%
  unlist()
legal_weight_curyr <- output_df %>% 
  filter(Survey.Year == max(Survey.Year)) %>%
  select(Legal.Weight) %>%
  unlist()

GHL_Allocation <- c("PU Summmer", "PU Winter", "Commercial", "Total legal", "Total mature", "Percent legal if targeting total mature")
HR_20 <- c(Legal_biomass_curyr*0.20*0.5, Legal_biomass_curyr*0.20*0.1, Legal_biomass_curyr*0.20*0.4) #allocations if the harvest rate is 20 percent of legal biomass
HR_17 <- c(Legal_biomass_curyr*0.17*0.5, Legal_biomass_curyr*0.17*0.1, Legal_biomass_curyr*0.17*0.4) #allocations if the harvest rate is 17 percent
HR_15 <- c(Legal_biomass_curyr*0.15*0.5, Legal_biomass_curyr*0.15*0.1, Legal_biomass_curyr*0.15*0.4) #allocations if the harvest rate is 15 percent
HR_12 <- c(Legal_biomass_curyr*0.12*0.5, Legal_biomass_curyr*0.12*0.1, Legal_biomass_curyr*0.12*0.4) #allocations if the harvest rate is 12 percent
HR_10 <- c(Legal_biomass_curyr*0.10*0.5, Legal_biomass_curyr*0.10*0.1, Legal_biomass_curyr*0.10*0.4) #allocations if the harvest rate is 10 percent
HR_8 <- c(Legal_biomass_curyr*0.08*0.5, Legal_biomass_curyr*0.08*0.1, Legal_biomass_curyr*0.08*0.4) #allocations if the harvest rate is 8 percent
HR_7 <- c(Legal_biomass_curyr*0.07*0.5, Legal_biomass_curyr*0.07*0.1, Legal_biomass_curyr*0.07*0.4) #allocations if the harvest rate is 7 percent
HR_6 <- c(Legal_biomass_curyr*0.06*0.5, Legal_biomass_curyr*0.06*0.1, Legal_biomass_curyr*0.06*0.4) #and so on
HR_5 <- c(Legal_biomass_curyr*0.05*0.5, Legal_biomass_curyr*0.05*0.1, Legal_biomass_curyr*0.05*0.4) #and so on
#combine in a df table
Table3.1_temp <- data.frame(HR_20, HR_17, HR_15, HR_12, HR_10, HR_8, HR_7, HR_6, HR_5)
#calculate total legal biomass as a sum of each column
Total_legal <- Table3.1_temp %>%
  summarize(across(everything(), sum)) # Sum each column
Total_mature <- c(Mature_biomass_curyr*0.2, Mature_biomass_curyr*0.17, Mature_biomass_curyr*0.15, Mature_biomass_curyr*0.12, Mature_biomass_curyr*0.10, Mature_biomass_curyr*0.08, Mature_biomass_curyr*0.07, Mature_biomass_curyr*0.06, Mature_biomass_curyr*0.05)
Percent_legal_if_targeting_mature <- c(Total_mature/Legal_biomass_curyr) *100

Table_3.1_temp2 <- data.frame(HR_20, HR_17, HR_15, HR_12, HR_10, HR_8, HR_7, HR_6, HR_5) %>%
  rbind(Total_legal) %>% rbind(Total_mature) %>% rbind(Percent_legal_if_targeting_mature)

round(Table_3.1_temp2)
Table_3.1 <- cbind(GHL_Allocation, Table_3.1_temp2) #add the GHL allocation column

###table 3.2: harvest rate in numbers
#calc the #'s using mature weight
mature_weight_curyr <- output_df %>% 
  filter(Survey.Year == max(Survey.Year)) %>%
  select(Mature.Weight) %>%
  unlist()

legal_numbers_approx <- round(Legal_biomass_curyr/legal_weight_curyr)  #ugh, I see a potential for error here. C used the average of a few random-looking years as the mean legal weight. I'm gonna use this year's. **FLAG!!**
mature_numbers_approx <- round(Mature_biomass_curyr/mature_weight_curyr) 

Table3.2 <- Table_3.1_temp2[-c(4:6),] %>% #removed total legalbiomass, mature biomass and crab %
  mutate(HR20 = HR_20/legal_weight_curyr, #convert to # of crab
         HR17 = HR_17/legal_weight_curyr,
         HR15 = HR_15/legal_weight_curyr,
         HR12 = HR_12/legal_weight_curyr,
         HR10 = HR_10/legal_weight_curyr,
         HR8 = HR_8/legal_weight_curyr,
         HR7 = HR_7/legal_weight_curyr,
         HR6 = HR_6/legal_weight_curyr,
         HR5 = HR_5/legal_weight_curyr) %>%
  round() #round to the nearest crab
  #add in a row, not column, that is the sum
Total_legal_nums <- Table3.2 %>%
  summarize(across(everything(), sum))
   

Total_mature_nums<- c(HR20 = mature_numbers_approx*0.20,# get mature harvest numbers for each harvest rate
                       HR17 = mature_numbers_approx*0.17, 
                       HR15 = mature_numbers_approx*0.15, 
                       HR12 = mature_numbers_approx*0.12, 
                       HR10 = mature_numbers_approx*0.10, 
                       HR8 = mature_numbers_approx*0.08, 
                       HR7 = mature_numbers_approx*0.07, 
                       HR6 = mature_numbers_approx*0.06, 
                       HR5 = mature_numbers_approx*0.05) #and so on

#combine 
Table3.2_final <- Table3.2 %>% rbind(Total_legal_nums) %>% rbind(Total_mature_nums) #add the total legal and mature crab numbers
Table3.2_final <- cbind(GHL_Allocation[-6], Table3.2_final)

#Ok I THINK that is everything I need from table 3 in the Juneau CSA excel file. Reference RMD if anything else is needed.

#Table 2 replication ("just update current year, this is a comparison from previous published forecasts")

#Table 2_currentYR replication ("This is the current yeras model output - all biomass values get replaced (from the entire CSV))







#########################################################################################
###DEVELOPMENT WORKFLOW
##1. create input dataset (the CSA starting values)
##2. create the RTMB code - to run the CSA starting values and get CSA ending calues
##3. Any other tables that I want

#Q: so WHY does this have to be in RTMB? Why was optim unstable?




######################################################
######################################################
######################################################
#TRASH CAN
#ok that was messy - try the other way
########################################
#pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, 
#                           map = map) # make adfun object #random is the epsiolons that are getting integrated out - the random effects

#uoptimized_rep <- pop_mod$report() #the report file prior to optimizing our objective function. Can get anything in report section using unoptomized values
#plot(uoptimized_rep$SSB)
#lines(sim$SSB)

# fit_tmb uses nlminb in the background to optimize model
#fitted_mod <- TMBhelper::fit_tmb(obj = pop_mod, #TMB optimizer- I could not insteall- 
#                                fn = pop_mod$fn,
#                             gr = pop_mod$gr, 
#                            newtonsteps = 5, # additional steps helps get the gradient lower
#                           getsd = TRUE)
#that does not work


#Idk what that is- AGR
# Predictions and standard errors from ADREPORT() #WILL i WANT TO CHANGE THIS TO GET DIFFERENT OUTPUTS??
#pred <- as.list(rep, "Est", report=TRUE)$pred
#se <- as.list(rep, "Std", report=TRUE)$pred


