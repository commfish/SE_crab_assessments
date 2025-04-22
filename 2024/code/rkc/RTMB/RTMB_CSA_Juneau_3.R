###Juneau RTMB CSA###
##Alex Reich
##start of development: 4/14/25
##recent work: 4/14/25
##IN DEVELOPMENT###



#load libraries
library(tidyverse)
library(RTMB)
library(here)
library(TMBhelper)

#to test: use juneau 2023 CSA to calc the 2024 analysis.... and see what happens

#######
#DATA
#######

df <- read.csv("CSA_excel/JNU_test.csv") #I might want to put a big df here


#put data into individual stored places for RTMB
YEARS <- df$Survey.Year
WEIGHTS <- df$Weight #weighing. MAy need to add one for the new year
CATCH <- as.numeric(gsub(",", "", df$Catch..Number.)) #get rid of commas, ideally before...
#replace NA in catch with 0
CATCH[is.na(CATCH)] <- 0 #replace NA with 0

##there was some thing in the juneau csa excel readme about how calculating PU is not straightforward. So... check that plz


CATCH_MIDDATE <-as.Date(df$Catch.Mid.Date,format = "%d-%b-%y") #might have to do some weird date wrangling here FLAG- just put it into julian
REF_DATE <- CATCH_MIDDATE[1]
#CATCH_MIDDATE <- as.numeric(format(CATCH_MIDDATE, "%j"))
CATCH_MIDDATE <- as.numeric(CATCH_MIDDATE - REF_DATE) #eqential added days

#fill NA's with the value before
#for (i in 2:length(CATCH_MIDDATE)){
#  if (is.na(CATCH_MIDDATE[i])) {
#    CATCH_MIDDATE[i] <- CATCH_MIDDATE[i-1]
#  }
#}


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
##survey weights (from summary table) #vectors over all the years
WEIGHT <- df$Weight
#pred survey CPUE
pred_CPUE_prerec <- df$Estimated.Prerecruit
pred_CPUE_rec <- df$Estimated.Recruits
pred_CPUE_postrec <- df$Estimated.Postrecruits


##########
#PARAMS- merge with SETUP below??
###########
REC <- 82.7928907453614/100  #preR to R suvival rate #I took the starting value from the 2024 analysis
  ##(do we HAVE a rec -> postrec survival???) #THIS IS ALLOWED TO CHANGE
q <- 104.187334848418/1000000 #catchability as a rate (est as not/100? IDK (see csa excel for what they do...)) #THIS IS ALLOWED TO CHANGE
S <- 0.32 #I think this is fixed.  #neg or positive tho? #FIXED
Z <- exp(-S)#total instantaneous mortality #FIXED
SURVIVAL_PARAMS <- df$Survival.Parameters #FLAG- is this ALSO the estimated prerecriuits for that year? seems like it... #THIS IS ALLOWED TO CHANGE


####################
#SETUP
#######################
data <- list(
  YEARS = YEARS,
  lambdas = WEIGHT,
  CATCH = CATCH,
  #CATCH_MIDDATE = CATCH_MIDDATE,#add this in to postrec after we get rolling
  #SURVEY_MIDDATE = SURVEY_MIDDATE,
  CPUE_prerec = CPUE_prerec, #are NA's a problem? Cause we have 'em. For all survey obs
  CPUE_rec = CPUE_rec,
  CPUE_postrec = CPUE_postrec,
  pred_CPUE_prerec = pred_CPUE_prerec,
  pred_CPUE_rec = pred_CPUE_rec,
  pred_CPUE_postrec = pred_CPUE_postrec, #uh, these are calced and thus dont need to be in data?
  #survival params here or in parameters?!! something, at least
  wt_mature= df$Mature.Weight,
  wt_legal = df$Legal.Weight,
  wt_prerec = df$Prerecruit.Weight, 
  
)

pars <- list(
  #ln_mean_rec = log(1), # mean recruitment
  #ln_sigma_R = log(0.1), # recruitment variability #MIGHT WANT TO ADD THIS IN!!
  #ln_q = log(q), # catchability #AGR- will have to put this in there- insead of q FLAG
  q=q,  #let's let q gp negative and see if it blows up #FLAG
  #ln_rec = log(rec), # preR to R survival rate #where was this calc?
  rec=REC, #should not be allowed to go neg, but let's see what happens when I let it...
  survival_params = SURVIVAL_PARAMS, #not fixed!! What is this??
  S = S, #fixed!!survival. Fix using map. How to again??
  sigma_survey = 0.1 # survey index error #uh... do I need other error too? #also, dont let this go negative. For each one one one # total?
  #ln_InitDevs = rep(0, n_ages - 2), # Initial Recruitment penalty
  #ln_RecDevs = rep(0, n_yrs) # Recruitment penalty
)

#map- to fix things!! need to fix some of my params (FLAG!!)
#do I need to specify starting values for my params??

############################3
#SOMETHNG LIKE THIS:
basic_pop_model <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, data) #or can write out as in the RTMB vonbert example
  
  # Model Set Up (Containers) -----------------------------------------------
  ##DO I EVEN NEED CONTAINERS? I don't think I need ...
  n_stages = 3 # number of stages for a 3 stage model
  n_yrs = length(YEARS) # number of years
  lambdas = length(YEARS) #the weights container
  
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
  srv_q = exp(ln_srv_q) # survey catchability
  #mean_rec = exp(ln_mean_rec) # mean recruitment
  rec = REC

  
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
    SrvIdx_nLL[y, st] = -dnorm(log(ObsSrvIdx[y,st]), log(PredSrvIdx[y,st]), sigma_survey, TRUE) * lambdas[y] 
     } #end of st(stage) loop
  } #logged so they don't go negative. This ok?? Do they need a constant so they don't go 0?
  #other error needed too?
  #perhaps try logged and unlogged and see what happens...

  
  ## Recruitment ------------------------------------------------------------- PERHAPS ADD THIS LATER
  #Init_Rec_nLL = -sum(dnorm(ln_InitDevs, -sigma_R^2/2, sigma_R, TRUE)) #I am unsure if these stay for the crab CSA.. this will be the next addition if not now, at least
  #Rec_nLL = -sum(dnorm(ln_RecDevs, -sigma_R^2/2, sigma_R, TRUE))
  
  # Get joint likelihood
  jnLL = sum(SrvInd_nLL) #we're keeping it simple for the crab CSA
  #jnLL = sum(Catch_nLL) + sum(SrvIdx_nLL) + sum(FishAgeComps_nLL) + 
   # sum(SrvAgeComps_nLL) + sum(Fmort_Pen) + sum(Init_Rec_nLL) +
    #sum(Rec_nLL)solver in excel including preR to R survival (and also catchability q) - is this part of the likelihood?? #FLAG- perhaps add this next!!
  
 
  # Report Section
  RTMB::ADREPORT(SSB)# Mature and Legal biomasses, and error
  RTMB::REPORT(sigma_survey) #I want my error. Will have to add in other error sources later??
  RTMB::ADREPORT(PredSrvIdx) #survey biomass by stage
  RTMB::ADREPORT(PredSrvCPUE) #predicted survey CPUE by stage #ADREPORT instead perhaps? give me the sigmas??
  RTMB::REPORT(jnLL)
  
  return(jnLL) #do I needs this too?
}
#END POP MODEL EXAMPLE


# Constructs objective function with derivatives #PULLED FROM EXAMPLEAND I THINK THSI WORKS
obj <- MakeADFun(nll, par)

# Minimize the objective function # PULLED FROM EXAMPLE AND THIS WORKS HERE I BELEIVE
opt <- nlminb(obj$par, obj$fn, obj$gr)

# Model summaries
sdrep <- sdreport(obj)
summary(sdrep)

# Predictions and standard errors from ADREPORT() #WILL i WANT TO CHANGE THIS TO GET DIFFERENT OUTPUTS??
pred <- as.list(rep, "Est", report=TRUE)$pred
se <- as.list(rep, "Std", report=TRUE)$pred

# Output REPORT() variable (no SEs)
obj$report()$Sigma

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

