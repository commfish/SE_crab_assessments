###Juneau RTMB CSA###
##Alex Reich
##start of development: 4/14/25
##recent work: 4/30/25
##IN DEVELOPMENT###

##############################################################################
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

#TAKE-AWAYS
##the RTMB model is pretty dang similar to the excel CSA model
##differnt results during the missing survey years- liekly because of how I told RTMB to handle missing values vs excel- but these were many years ago and have minimal impact on overall time series
##it's easy to add things to the RTMB model - my gut says the next step to make this model better will be something to do with RECRUITMENT involving another LIKELIHOOD
###But let's think about priorities here before I go on an improving this model tangent - much other work to do and I am one person.
##Notable that when S (survival) is estimated, it is a good deal lower than what we've been using as a fixed value - marginal convergence but I haven't tried multiple newton steps
###and I suspect something about recruitment, not survival, is the next step to messing with this model.

#I used the 2023 and 2024 Juneau Excel CSA's, Basic_Pop_Model_RTMB.Day3.R (from the RTMB workshop), and Tyler's TMB code for SE Tanner crab (on S drive) to draft this model.
##################################################################################


###input files - FLAG - can be improved in  the future
#I'm trying to figure out the best way... to adjust the CSA excel to a regular CSV input
##adding in new values manually for now (best way?? probs not but I can adjust later)
##est prerec is copied over from last year, est rec and est postrec set to 0. I think I calc these below so...


##output files- FLAG- gonna want to create an output that looks like the input, for ease and reproducibility in future years


#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)

#to test: use juneau 2023 CSA to calc the 2024 analysis.... and see what happens
set.seed(100)

#######
#DATA
#######

df_juneau_24_compare <- read.csv("CSA_excel/JNU_test.csv") #this is the analysis at the end of 2024
df <- read.csv("CSA_excel/JNU_test_2023to2024_replication.csv") #this is the analysis at the end of 2023


#put data into individual stored places for RTMB
YEARS <- df$Survey.Year
WEIGHTS <- df$Weight #weighing. MAy need to add one for the new year
#replace NA's with 0
WEIGHTS[is.na(WEIGHTS)] <- 0 #replace NA with 0

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
#replace NA's in survey CPUEs with 0
CPUE_prerec[is.na(CPUE_prerec)] <- 0 #replace NA with 0
CPUE_rec[is.na(CPUE_rec)] <- 0 #replace NA with 0
CPUE_postrec[is.na(CPUE_postrec)] <- 0 #replace NA with 0

##survey weights (from summary table) #vectors over all the years
#WEIGHT <- df$Weight
#pred survey CPUE
pred_CPUE_prerec <- df$Estimated.Prerecruit
pred_CPUE_rec <- df$Estimated.Recruits
pred_CPUE_postrec <- df$Estimated.Postrecruits
#survival parameters are apparently data, not parameters.
##model does bad thinbgs if they are parameters (prerecruit CPUE expected = observed exactly)
SURVIVAL_PARAMS <- df$Survival.Parameters # this ALSO the estimated prerecriuits for that year? WHY? Idk theory but replicating the excel. **FLAG**



##########
#PARAMS- merge with SETUP below??
###########
#REC <- 82.7928907453614/100  #preR to R suvival rate #I took the starting value from the 2024 analysis
REC <- 84.6347230128254/100  #starting value from 2023 to 2024 analysis (last year)
  ##(do we HAVE a rec -> postrec survival???) #THIS IS ALLOWED TO CHANGE
#q <- 104.187334848418/1000000 #catchability as a rate (est as not/100? IDK (see csa excel for what they do...)) #THIS IS ALLOWED TO CHANGE
q <- 105.557381539957/1000000 #from 2023 to 2024 analysis (last year)
S <- 0.32 #I think this is fixed.  #neg or positive tho? #FIXED
Z <- exp(-S)#total instantaneous mortality #FIXED
#SURVIVAL_PARAMS <- df$Survival.Parameters #FLAG- is this ALSO the estimated prerecriuits for that year? seems like it... #THIS IS ALLOWED TO CHANGE


####################
#SETUP
#######################
data <- list(
  YEARS = YEARS,
  lambdas = WEIGHTS,
  CATCH = CATCH,
  #CATCH_MIDDATE = CATCH_MIDDATE, #params I think #shit, this might be data, according to tyler code **FLAG**
  #SURVEY_MIDDATE = SURVEY_MIDDATE,
  CPUE_prerec = CPUE_prerec, #I replaced the NA's with 0's. Weights (lambda's are also 0 during the missing survey years)
  CPUE_rec = CPUE_rec,
  CPUE_postrec = CPUE_postrec,
  pred_CPUE_prerec = pred_CPUE_prerec,
  pred_CPUE_rec = pred_CPUE_rec,
  pred_CPUE_postrec = pred_CPUE_postrec, #uh, these are calced and thus dont need to be in data?
  wt_mature= df$Mature.Weight,
  wt_legal = df$Legal.Weight,
  wt_prerec = df$Prerecruit.Weight,
  survival_params = SURVIVAL_PARAMS #moved this to data instead of parameters. Is it right?? IDK!
  
)

pars <- list(
  #ln_mean_rec = log(1), # mean recruitment
  #ln_sigma_R = log(0.1), # recruitment variability #Add in in later iterations??
  ln_q = log(q), # catchability 
  #q=q,  #let's let q gp negative and see if it blows up #FLAG
  ln_rec = log(REC), # preR to R survival rate #where was this calc?
  #rec=REC, #should not be allowed to go neg, but let's see what happens when I let it...
  #survival_params = SURVIVAL_PARAMS, #not fixed!! What is this?? They can't go neg, so maybe make sure of that too...
  S = S, #fixed!!survival. do I need to log??
  ln_sigma_survey = log(0.1),
  SURVEY_TAU = SURVEY_TAU, #is this data or param? #regardless, fix this please #shit, this might be data, according to tyler code **FLAG**
  CATCH_SURVEY_TAU = CATCH_SURVEY_TAU #fix this in the mapping #shit, this might be data, according to tyler code **FLAG**
 # sigma_survey = 0.1 # survey index error #uh... do I need other error too? #also, dont let this go negative. For each one one one # total?
  #ln_InitDevs = rep(0, n_ages - 2), # Initial Recruitment penalty
  #ln_RecDevs = rep(0, n_yrs) # Recruitment penalty
)

#remove all values from the environment except pars and data:
rm(list = ls()[!(ls() %in% c("pars", "data"))]) #remove everything except pars and data


#quick graph to check CPUE distributions
df <- data.frame(data)
#graph the distribution of the observed survey CPUE
ggplot(df)+ aes(x=CPUE_prerec) + geom_density()
ggplot(df)+ aes(x=log(CPUE_prerec)) + geom_density() 
ggplot(df)+ aes(x=CPUE_rec) + geom_density()
ggplot(df)+ aes(x=log(CPUE_rec)) + geom_density() 
ggplot(df)+ aes(x=CPUE_postrec) + geom_density()
ggplot(df)+ aes(x=log(CPUE_postrec)) + geom_density() 
##ok ok I don't need to log...
#arguably CPUE_prerec and CPUE_rec could be logged


#map- to fix parameters!!
map <- list()
map$S <- factor(NA) #fix survival #when I let the model estimate survival, it estimates 0.229 (a good bit lower than what we typically fix) and model does not converge great (but what if I were to add more newtonsteps...)
map$SURVEY_TAU <- factor(rep(NA, length(pars$SURVEY_TAU))) #fix survey tau
map$CATCH_SURVEY_TAU <- factor(rep(NA, length(pars$CATCH_SURVEY_TAU))) #fix catch tau


############################3
#SOMETHNG LIKE THIS:
#the function
basic_pop_model <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, data) #or can write out as in the RTMB vonbert example
  
  # Model Set Up (Containers) -----------------------------------------------
  n_stages = 3 # number of stages for a 3 stage model
  n_yrs = length(YEARS) # number of years
  
  # Population Stuff
  #CPUE_AS = array(data = 0, dim = c(n_yrs + 1, n_stages)) # Numbers at stage, adds one for this year
  SSB = array(0, dim = c(n_yrs, n_stages)) # Pre-rec, legal, and mature biomasses
  
  # Survey Stuff
  ObsSrvCPUE = array(data = 0, dim = c(n_yrs, n_stages)) # Survey CPUE at stage #I read this in
  ObsSrvCPUE[,1] <- CPUE_prerec # prerecruit
  ObsSrvCPUE[,2] <- CPUE_rec # recruit
  ObsSrvCPUE[,3] <- CPUE_postrec # postrecruit
  PredSrvCPUE = array(data = 0, dim = c(n_yrs, n_stages)) # Predicted CPUE at stage
  PredSrvIdx = array(0, dim = c(n_yrs, n_stages)) # predicted biomass calculated from the predicted survey CPUE and waa
  
  # Likelihoods - box
  SrvIdx_nLL = array(0, dim = c(n_yrs, n_stages)) # Survey Index Likelihoods - this replaces the sum of squares - one likelihood for each year and each stage - summed by row and then summed by year

  # Penalties #I don't need penalties?? do I?
  #Rec_nLL = rep(0, n_yrs) # Recruitment penalty
  #Init_Rec_nLL = rep(0, n_ages - 2) # Initial Recruitment penalty
  jnLL = 0 # Joint negative log likelihood #this I need
  
  # Do some parameter transformations here AGR DO I NEED THESE?
  #mean_rec = exp(ln_mean_rec) # mean recruitment
  #sigma_R = exp(ln_sigma_R) # recruitment variability
  #sigma_F = exp(ln_sigma_F) # fishing mortality variability
  #M = exp(ln_M) # natural mortality #I think I fix natural mortality
  q = exp(ln_q) # survey catchability
  #mean_rec = exp(ln_mean_rec) # mean recruitment
  rec = exp(ln_rec)
  sigma_survey = exp(ln_sigma_survey) # survey index error

  
  # Initialize Population ---------------------------------------------------

  #pop initialization
  #juneau specific, I'm giving it the starting predicted cpue values from excel- will have to change this for every area
  PredSrvCPUE[1,] <- c( #should I call these something else?? since I read this in from excel at some point....
   # years = YEARS[1],
    Pred_CPUE_prerec_calc = survival_params[1],
    Pred_CPUE_rec_calc = pred_CPUE_rec[1],
    Pred_CPUE_postrec_calc = pred_CPUE_postrec[1]
    #CPUE_postrec = (CPUE_rec[t-1] + CPUE_postrec[t-1]) * exp(-S) - (q*CATCH[t-1]*exp(-S)) #i removed tau. see if she runs first
    #CPUE_postrec = (CPUE_rec[t-1] + CPUE_postrec[t-1]) * exp(-S * SURVEY_TAU[t]) - (q*CATCH[t-1]*exp(CATCH_SURVEY_TAU*-S)) 
  )
  
  #Pop projection
  for (t in 2:n_yrs){
  #predSrvCPUE[t,] <- c(
    #years = YEARS,
    PredSrvCPUE[t,1] = survival_params[t] #this is the prerecruit
    PredSrvCPUE[t,2] = rec*pred_CPUE_prerec[t-1] #this is the recruit
    PredSrvCPUE[t,3] = (pred_CPUE_rec[t-1] + pred_CPUE_postrec[t-1]) * exp(-S * SURVEY_TAU[t]) - (q*CATCH[t-1]*exp(CATCH_SURVEY_TAU[t]*-S)) #postrecruit
  #)
} #ok cool, got the pop (CPUE) projection in there.
  

  
  #calc the biomass per year for prerecruit, recruit, and postrecruit legal and mature
  PredSrvIdx[,1] <- (PredSrvCPUE[,1]/q) * wt_prerec #prerecruit biomass = prerecruit cpue/catchability * the weight 
  PredSrvIdx[,2] <- ((PredSrvCPUE[,2]+PredSrvCPUE[,3])/q) * wt_legal #legal biomasss = recruit cpue + postrecruit cpue, divided by catchability, times the legal weight
  PredSrvIdx[,3] <- PredSrvIdx[,1]+PredSrvIdx[,2] #mature biomass =  legal biomass + prerecruit biomasss
  #looks good
  
  # Likelihoods -------------------------------------------------------------

  ## Survey Index ------------------------------------------------------------


  for(y in 1:n_yrs) {
     for(st in 1:n_stages) {
    #SrvIdx_nLL[y, st] = -dnorm(log(ObsSrvCPUE[y,st]+0.0001), log(PredSrvCPUE[y,st]+0.0001), sigma_survey, TRUE) * lambdas[y] #to log or not to log (*FLAG*)?? #TO LOG!! 
       ##the above is a better (logged) model according to jnll. but the below (not logged) is more similar to RSS
       ##perhaps graph the data and see distribution
    SrvIdx_nLL[y, st] = -dnorm(ObsSrvCPUE[y,st], PredSrvCPUE[y,st], sigma_survey, TRUE) * lambdas[y] #NOT TO LOG!! - 
     } #end of st(stage) loop
  } #logged so they don't go negative. This ok?? Do they need a constant so they don't go 0?
  #other error needed too?
  #perhaps try logged and unlogged and see what happens...

  
  ## Recruitment ------------------------------------------------------------- PERHAPS ADD THIS LATER
  #Init_Rec_nLL = -sum(dnorm(ln_InitDevs, -sigma_R^2/2, sigma_R, TRUE)) #I am unsure if these stay for the crab CSA.. this will be the next addition if not now, at least
  #Rec_nLL = -sum(dnorm(ln_RecDevs, -sigma_R^2/2, sigma_R, TRUE))
  
  # Get joint likelihood
  jnLL = sum(SrvIdx_nLL) #we're keeping it simple for the crab CSA
  #jnLL = sum(Catch_nLL) + sum(SrvIdx_nLL) + sum(FishAgeComps_nLL) + 
   # sum(SrvAgeComps_nLL) + sum(Fmort_Pen) + sum(Init_Rec_nLL) +
    #sum(Rec_nLL)solver in excel including preR to R survival (and also catchability q) - is this part of the likelihood?? #FLAG- perhaps add this next!!
  
 
  # Report Section
  #RTMB::ADREPORT(SSB)# Mature and Legal biomasses, and error
  RTMB::REPORT(sigma_survey) #I want my error. Will have to add in other error sources later??
  #RTMB::ADREPORT(PredSrvIdx) #survey biomass by stage
  RTMB::REPORT(PredSrvIdx) #REPORT or ADREPORT?? *FLAG* #the pred biomass
  #RTMB::ADREPORT(PredSrvCPUE) #predicted survey CPUE by stage
  RTMB::REPORT(PredSrvCPUE) #REPORT or ADREPORT?? *FLAG* #the pred cpue
  RTMB::REPORT(jnLL)
  RTMB::REPORT(q)
  RTMB::REPORT(rec)
  RTMB::REPORT(survival_params) #report the survival params
  
  return(jnLL) #do I need this too?
}
#END POP MODEL EXAMPLE


# Run Model ---------------------------------------------------------------


pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map=map) #maybe I can try some ranefs


##################
#TORUBLESHOOT
# Check initial parameter values
print(pop_mod$par)

# Evaluate the objective function and gradient at initial parameter values
initial_fn <- pop_mod$fn(pop_mod$par)
initial_gr <- pop_mod$gr(pop_mod$par)

print(initial_fn) #if NA's there is problem
print(initial_gr) #if NA's, problem

# Ensure no NA/NaN values in the function and gradient evaluations
if (any(is.na(initial_fn)) || any(is.nan(initial_fn))) {
  stop("Objective function evaluation returned NA/NaN values.")
}

if (any(is.na(initial_gr)) || any(is.nan(initial_gr))) {
  stop("Gradient evaluation returned NA/NaN values.")
}

##END TROUBLESHOOT
################

pop_mod$par #object and starting params
pop_mod$fn()
pop_mod$gr()


#OPTION 1 for model run - nlminb
#opt <- nlminb(pop_mod$par, pop_mod$fn, pop_mod$gr) #can I do more newtonsteps here? IDK
#opt
#relative convergence - I've definitely seen worse! but **FLAG**

#OPTION 2 for model run - fit_tmb - let's me use newton steps
opt <- TMBhelper::fit_tmb(obj = pop_mod, #works now; in progress, turn off please!
                                fn = pop_mod$fn,
                               gr = pop_mod$gr, 
                               newtonsteps = 2, # additional steps helps get the gradient lower
                         getsd = TRUE) #can check out this model results, how different from nlmimb
opt



# Model summaries
sdrep <- sdreport(pop_mod)
sdrep #maximum gradient component is here - # 0.001 or smaller considered converged, can use newtonsteps (can I tho??) to make it smaller
## **FIXED** Crap- does not consistently have a positive definite hessian- maybe I do need more newtonsteps **FIXED**
#update- I think I fixed the hessian issue - moved surival params to data instead of parameters
summary(sdrep) #why are all sd' na??
#ok well, my results appear to be in here
#what's up with the sd tho, they dont have sd... survey sd is it's own thing?? Do I need to input sd differently perhaps?
names(pop_mod)
pop_mod$report()

pop_mod$report()$sigma_survey #yay, a number!  did I do this part right? #ooh. very different if log() vs. not log() in dnorm. **FLAG**
##so this is the standard error on my predicted values in CPUE? so to get the standard error around biomass of prerec, postrec, , I can do calcs


pop_mod$gr()
pop_mod$report(pop_mod$env$last.par.best) #parameters
pop_mod$env$last.par.best 

#covariance graphs??
#cor <- cov2cor(solve(as.matrix(pop_mod$sd_rep$jointPrecision)))
#library(corrplot)
#corrplot(cor, type='lower')

#cor = cov2cor(pop_mod$sd_rep$cov.fixed)
#corrplot(cor, type='lower')


#load in the CSA 2024 analysis (again...) for comparison of methods: RTMB to Ecvel
df_juneau_24_compare <- read.csv("CSA_excel/JNU_test.csv")
#get rid of commas in the numbers for prerecuit biomass, legal biomass, and mature biomass
df_juneau_24_compare$Mature.Biomass <- as.numeric(gsub(",", "", df_juneau_24_compare$Mature.Biomass))
df_juneau_24_compare$Legal.Biomass <- as.numeric(gsub(",", "", df_juneau_24_compare$Legal.Biomass))
df_juneau_24_compare$Prerecruit.Biomass <- as.numeric(gsub(",", "", df_juneau_24_compare$Prerecruit.Biomass))


#EXTRACT FINAL VALUES
result_df <- data.frame(pop_mod$report(pop_mod$env$last.par.best))
#change result df names to prerecruit, recruit, postrectuit
names(result_df) <- c("sd","prerecruit_biomass", "legal_biomass", "mature_biomass", 
                      "prerecuit_cpue", "recruit_cpue", "postrecuit_cpue",
                      "jnll","q", "rec", "survival_params")
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
        year = df_juneau_24_compare$Survey.Year) #that could have been cleaner but I added year


  
#graph to compare observed survey values, excel CSA model, and RTMB CSA model
##results_df_relevant has my predicted values
##df_juneau_24_compare has the observed values
ggplot(results_df_relevant) + aes(x=year, y=prerecuit_cpue) + 
  geom_ribbon(aes(ymin=prerecuit_cpue_lower, ymax=prerecuit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", linewidth=1) + #the model-predicted cpue
  geom_point(data=df_juneau_24_compare ,aes(y=Pre.recruit, x=Survey.Year)) + #this is the observed survey CPUE values
  ##probs should add the SE for that at some point- I'm sure it exists
  #add the CSA excel model CPUE
  geom_line(data=df_juneau_24_compare ,aes(y=Estimated.Prerecruits, x=Survey.Year), color = "darkgreen") + #this is the observed survey CPUE values
  labs(title="JNU Prerec CPUE - Excel in dark, RTMB light with CI", x="Year", y="CPUE") +
  theme_minimal()
#there we go.pre-rec is being estimated now.

#anyway, recruits CPUE
ggplot(results_df_relevant) + aes(x=year, y=recruit_cpue) + 
  geom_ribbon(aes(ymin=recruit_cpue_lower, ymax=recruit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", linewidth=1) + #the model-predicted cpue
  geom_point(data=df_juneau_24_compare ,aes(y=Recruit, x=Survey.Year)) + #this is the observed survey CPUE values
  ##probs should add the SE for that at some point- I'm sure it exists
  #add the CSA excel model CPUE
  geom_line(data=df_juneau_24_compare ,aes(y=Estimated.Recruits, x=Survey.Year), color = "darkgreen") + #this is the observed survey CPUE values
  labs(title="JNU Rec CPUE - Excel in dark, RTMB light with CIE", x="Year", y="CPUE") +
  theme_minimal()

#and postrecruit CPUE
ggplot(results_df_relevant) + aes(x=year, y=postrecuit_cpue) + 
  geom_ribbon(aes(ymin=postrecuit_cpue_lower, ymax=postrecuit_cpue_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", linewidth=1) + #the model-predicted cpue
  geom_point(data=df_juneau_24_compare ,aes(y=Post.recruit, x=Survey.Year)) + #this is the observed survey CPUE values
  ##probs should add the SE for that at some point- I'm sure it exists
  #add the CSA excel model CPUE
  geom_line(data=df_juneau_24_compare ,aes(y=Estimated.Postrecruits, x=Survey.Year), color = "darkgreen") + #this is the observed survey CPUE values
  labs(title="JNU Postrec CPUE - Excel in dark, RTMB light with CI", x="Year", y="CPUE") +
  theme_minimal()

##################################################
#graph CSA excel biomass vs. RTMB biomass estimates
#mature biomass
ggplot(results_df_relevant) + aes(x=year, y=mature_biomass) + 
  geom_ribbon(aes(ymin=mature_biomass_lower, ymax=mature_biomass_upper), alpha = 0.3, fill = "lightblue") + #uncertainty
  geom_line(color ="lightblue", size=1) + #the model-predicted RTMB biomasss
  geom_line(data=df_juneau_24_compare ,aes(y=Mature.Biomass, x=Survey.Year), color="blue") + #this is excel model mature biomass
  #add legal biomass
  geom_ribbon(aes(ymin=legal_biomass_lower, ymax=legal_biomass_upper), alpha = 0.2, fill = "lightgreen") + #uncertainty
  geom_line(aes(y=legal_biomass),color ="lightgreen", size=1) + #the RTMB model-predicted biomasss
  geom_line(data=df_juneau_24_compare ,aes(y=Legal.Biomass, x=Survey.Year), color="darkgreen") + #this is the excel moodel
  #add prerecruit biomass
  geom_ribbon(aes(ymin=prerecruit_biomass_lower, ymax=prerecruit_biomass_upper), alpha = 0.2, fill = "lightpink") + #uncertainty
  geom_line(aes(y=prerecruit_biomass),color ="lightpink", size=1) + #the RTMB model-predicted biomass
  geom_line(data=df_juneau_24_compare ,aes(y=Prerecruit.Biomass, x=Survey.Year), color="pink") + #this is the excel-predicted biomass
  labs(title="JNU Biomass - Excel dark lines, RTMB light lines with CI", x="Year", y="Biomass") +
  theme_minimal()





######################################################################

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


##############3
#CALC GHL HERE
################
#it is mature biomass * 0.1 or whatever - a range for the tables. See excel tables and base off of those **TO DO NEXT**

##################################################################
#the update the biomass csv part
#####################################################################
output_df$Mature.Biomass
output_df$Legal.Biomass #here, can copy uncertainty in if I choose to do so


#Table 2 replication ("just update current year, this is a comparison from previous puclished forecasts")

#Table 3 - add in mature and legal biomasses from this year.... and current year's legal weight
##this table will be a pain, might need to make 2 different ones

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


