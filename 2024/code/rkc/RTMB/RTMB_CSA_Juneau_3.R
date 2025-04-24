###Juneau RTMB CSA###
##Alex Reich
##start of development: 4/14/25
##recent work: 4/24/25
##IN DEVELOPMENT###


#TO DO:
##load in 2023 data to a df and see if I can replicate 2024 analysis predictions- ON IT
##log or no log for dnorm? 
#SHE RUNS! Does she run well? - as of 4/22/25

###input files - FLAG - can be improved in  the future
#I'm trying to figure out the best way... to adjust the CSA excel to a regular CSV input
##adding in new values manually for now (best way?? probs not but I can adjust later)
##est prerec is copied over from last year, est rec and est postrec set to 0. I think I calc these below so...
###sooo that should be ok??

##output files- FLAG- gonna want to create an output that looks like the input, for ease and reproducibility in future years


#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)

#to test: use juneau 2023 CSA to calc the 2024 analysis.... and see what happens

#######
#DATA
#######

#df <- read.csv("CSA_excel/JNU_test.csv") #df goes here!!
df <- read.csv("CSA_excel/JNU_test_2023to2024_replication.csv")


#put data into individual stored places for RTMB
YEARS <- df$Survey.Year
WEIGHTS <- df$Weight #weighing. MAy need to add one for the new year
#replace NA's with 0
WEIGHTS[is.na(WEIGHTS)] <- 0 #replace NA with 0

CATCH <- as.numeric(gsub(",", "", df$Catch..Number.)) #get rid of commas, ideally before...
#replace NA in catch with 0
CATCH[is.na(CATCH)] <- 0 #replace NA with 0

##there was some thing in the juneau csa excel readme about how calculating PU is not straightforward. So... check that plz


CATCH_MIDDATE <-as.Date(df$Catch.Mid.Date,format = "%d-%b-%y") #might have to do some weird date wrangling here FLAG- just put it into julian
REF_DATE <- CATCH_MIDDATE[1]
#CATCH_MIDDATE <- as.numeric(format(CATCH_MIDDATE, "%j"))
CATCH_MIDDATE <- as.numeric(CATCH_MIDDATE - REF_DATE) #eqential added days


SURVEY_MIDDATE <-as.Date(df$Survey.Mid.Date,format = "%d-%b-%y") #might have to do some weird date wrangling here FLAG- just put it into julian
#SURVEY_MIDDATE <- as.numeric(format(SURVEY_MIDDATE, "%j")) #nope, I want not julian
SURVEY_MIDDATE <- as.numeric(SURVEY_MIDDATE - REF_DATE) #seqential added days
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
SURVIVAL_PARAMS <- df$Survival.Parameters #FLAG- is this ALSO the estimated prerecriuits for that year? seems like it... #THIS IS ALLOWED TO CHANGE


####################
#SETUP
#######################
data <- list(
  YEARS = YEARS,
  lambdas = WEIGHTS,
  CATCH = CATCH,
  #CATCH_MIDDATE = CATCH_MIDDATE,#add this in to postrec after we get rolling
  #SURVEY_MIDDATE = SURVEY_MIDDATE,
  CPUE_prerec = CPUE_prerec, #I replaced the NA's with 0's. Weights (lambda's are also 0 during the missing survey years)
  CPUE_rec = CPUE_rec,
  CPUE_postrec = CPUE_postrec,
  pred_CPUE_prerec = pred_CPUE_prerec,
  pred_CPUE_rec = pred_CPUE_rec,
  pred_CPUE_postrec = pred_CPUE_postrec, #uh, these are calced and thus dont need to be in data?
  #survival params here or in parameters?!! something, at least
  wt_mature= df$Mature.Weight,
  wt_legal = df$Legal.Weight,
  wt_prerec = df$Prerecruit.Weight 
  
)

pars <- list(
  #ln_mean_rec = log(1), # mean recruitment
  #ln_sigma_R = log(0.1), # recruitment variability #MIGHT WANT TO ADD THIS IN!!
  ln_q = log(q), # catchability #AGR- will have to put this in there- insead of q FLAG
  #q=q,  #let's let q gp negative and see if it blows up #FLAG
  ln_rec = log(REC), # preR to R survival rate #where was this calc?
  #rec=REC, #should not be allowed to go neg, but let's see what happens when I let it...
  survival_params = SURVIVAL_PARAMS, #not fixed!! What is this?? They can't go neg, so maybe make sure of that too...
  S = S, #fixed!!survival. do I need to log??
  ln_sigma_survey = log(0.1),
  SURVEY_TAU = SURVEY_TAU, #is this data or param? #regardless, fix this please
  CATCH_SURVEY_TAU = CATCH_SURVEY_TAU #fix this in the mapping
 # sigma_survey = 0.1 # survey index error #uh... do I need other error too? #also, dont let this go negative. For each one one one # total?
  #ln_InitDevs = rep(0, n_ages - 2), # Initial Recruitment penalty
  #ln_RecDevs = rep(0, n_yrs) # Recruitment penalty
)
#remove other saved things in the environment:
#rm(list = ls(pattern = "^[^pars|data|df]$")) #remove everything except pars, data, and df
#rm(list = ls(pattern = "^[^pars|data]$")) #remove everything except pars, data, and df
#remove all values from the environment except pars and data:
rm(list = ls()[!(ls() %in% c("pars", "data"))]) #remove everything except pars and data

#map- to fix things!!
map <- list()
map$S <- factor(NA) #fix survival
map$SURVEY_TAU <- factor(rep(NA, length(pars$SURVEY_TAU))) #fix survey tau
map$CATCH_SURVEY_TAU <- factor(rep(NA, length(pars$CATCH_SURVEY_TAU))) #fix catch tau


#do I need to specify starting values for my params?? In some other way?

############################3
#SOMETHNG LIKE THIS:
basic_pop_model <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, data) #or can write out as in the RTMB vonbert example
  
  # Model Set Up (Containers) -----------------------------------------------
  ##DO I EVEN NEED CONTAINERS? I don't think I need ...
  n_stages = 3 # number of stages for a 3 stage model
  n_yrs = length(YEARS) # number of years
  #lambdas = length(YEARS) #the weights container
  
  # Population Stuff
  #CPUE_AS = array(data = 0, dim = c(n_yrs + 1, n_stages)) # Numbers at stage, adds one for this year
  SSB = array(0, dim = c(n_yrs, n_stages)) # Pre-rec, legal, and mature biomasses
  
  # Survey Stuff
  ObsSrvCPUE = array(data = 0, dim = c(n_yrs, n_stages)) # Survey index at stage #I read this in
  ObsSrvCPUE[,1] <- CPUE_prerec # prerecruit
  ObsSrvCPUE[,2] <- CPUE_rec # recruit
  ObsSrvCPUE[,3] <- CPUE_postrec # postrecruit
  PredSrvCPUE = array(data = 0, dim = c(n_yrs, n_stages)) #survey CPUE at stage
  PredSrvIdx = array(0, dim = c(n_yrs, n_stages)) # predicted biomass calcualted from the predicted survey CPUE and waa
  
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
  sigma_survey = exp(ln_sigma_survey) # survey index error)

  
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
  RTMB::REPORT(PredSrvIdx) #REPORT or ADREPORT?? *FLAG*
  #RTMB::ADREPORT(PredSrvCPUE) #predicted survey CPUE by stage
  RTMB::REPORT(PredSrvCPUE) #REPORT or ADREPORT?? *FLAG*
  RTMB::REPORT(jnLL)
  
  return(jnLL) #do I needs this too?
}
#END POP MODEL EXAMPLE


# Run Model ---------------------------------------------------------------


pop_mod <- RTMB::MakeADFun(basic_pop_model, parameters = pars, map=map)

#fitted_mod <- TMBhelper::fit_tmb(obj = pop_mod, #TMB optimizer- I could not insteall- #NO WORK!!
 ##                                fn = pop_mod$fn,
   #                              gr = pop_mod$gr, 
    #                             newtonsteps = 2, # additional steps helps get the gradient lower
    # (no work...)                            getsd = FALSE)

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


opt <- nlminb(pop_mod$par, pop_mod$fn, pop_mod$gr) 


# Model summaries
sdrep <- sdreport(pop_mod)
summary(sdrep) #why are all sd' na??
#ok well, my results appear to be in here
#what's up with the sd tho, they dont have sd... survey sd is it's own thing?? Do I need to input sd differently perhaps?
names(pop_mod)
pop_mod$report

pop_mod$report()$sigma_survey #yay, a number!  did I do this part right? #ooh. very different if log() vs. not log() in dnorm. **FLAG**
##so this is the standard error on my predicted values in CPUE? so to get the standard error around biomass of prerec, postrec, , I can do calcs
pop_mod$report() #why does my jnLL not exist?? #that's sketch. *FLAG*- it exists when dnorm has no logs. When dnorm has yes logs, jnll fails (perhaps because 0 or negative predicted #'s)
##Ok I addded a constant w/in the log so no 0's exist. and this jnLL is way lower- which is better. -65.38131

#Similar values when dnorm is unlogged. but slightly different. jnll 631.3896
#I NEED TO GRAPH THIS

#when dnorm is logged
##crap the biomass here is a good bit higher- WHY??

#Idk what that is- AGR
# Predictions and standard errors from ADREPORT() #WILL i WANT TO CHANGE THIS TO GET DIFFERENT OUTPUTS??
#pred <- as.list(rep, "Est", report=TRUE)$pred
#se <- as.list(rep, "Std", report=TRUE)$pred




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

######################################################################
#update the misc Juneau tables that I need (parallel the Excel doc) (only for the RKC juneau survey area)


##############3
#CALC GHL HERE
################
#it is mature biomass * 0.1 or whatever




##################################################################
#the update the biomass csv part


###DEVELOPMENT WORKFLOW
##1. create input dataset (the CSA starting values)
##2. create the RTMB code - to run the CSA starting values and get CSA ending calues
##3. Any other tables that I want

#Q: so WHY does this have to be in RTMB? Why was optim unstable?

