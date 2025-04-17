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
#load in datas - see the CSA excel for juneau for reference
#the old data - perhaps imported from last year's CSA

#df <- read.csv() #I might want to put a big df here

#I'll sim one for now:
df <- data.frame(years = c(1979:2024)) #end year length will be 2025 in 2025
df$years <- c(1979:2024) 

#put data into individual stored places for RTMB
YEARS <- df$years
WEIGHTS <- df$weights #weighing
CATCH <- df$Catch_.Number. #FLAG that this notation is ok. #just personal use for now, since the fishery is closed
##there was some thing in the juneau csa excel readme about how calculating PU is not straightforward. So... check that plz
CATCH_MIDDATE <- df$Catch_Mid_Date #might have to do some weird date wrangling here FLAG- just put it into julian
##also check column name, likely not right
SURVEY_MIDDATE <- df$Survey_Mid_Date #same same comments as CATCH_MIDDATE

#survey info
##survey CPUE (from summary table) #vectors over all the years
CPUE_prerec <- df$Pre_recruit
CPUE_rec <- df$Recruit
CPUE_postrec <- df$Post_recruit
##survey weights (from summary table) #vectors over all the years
WEIGHT_mature <- df$Mature_Weight
WEIGHT_legal <- df$Legal_Weight
WEIGHT_prerec <- df$Prerecruit_Weight



##########
#PARAMS
###########
Q2 <- 82.7928907453614/100  #preR to R suvival rate #I took the starting value from the 2024 analysis
  ##(do we HAVE a rec -> postrec survival???)
q <- 104.187334848418/100 #catchability as a rate (est as not/100? IDK (see csa excel for what they do...))
S <- 0.32 #I think this is fixed.  #neg or positive tho?
Z <- exp(-S)#total instantaneous mortality
SURVIVAL_PARAMS <- df$Survival_Parameters #FLAG- is this ALSO the estimated prerecriuits for that year? seems like it...


##intermediate calcs #FLAG- I needs to do this - perhaps add in after initial model works
#SURVEY_TAU <-  #ugh. FLAG. make this this year-last year's julian date for all years
#CATCH_SURVEY_TAU <- #greater than or = to survey_tau. Formula is this in excel: =IF(H41=0,1,+(J42-I41)/365). SO 
  ##if the catch = 0, the CATCH_SURVEY_TAU = 1 for the previous year. IF catch =! 0 for the previous year, last year SURVEY MID DATE - last year CATCH MID DATE / 365 = the TAU for the current year

#NOTE- I might have to log things so nothing goes below 0...


####################
#DERVIVED QUANTITIES
EST_PREREC <- SURVIVAL_PARAMS #FLAG - it equals survival params post adjustment
#EST_REC <- EST_PREREC(last year) * Q2 #Q2 is the prerec to rec survival rate #FLAG I need to loop this I think
#EST_POSTREC <- #(EST_REC(last year) + EST_POSTREC(last year)) * exp(-S * SURVEY_TAU) - (q*CATCH(t-1)*exp(CATCH_SURVEY_TAU*-S))



############################3
#SOMETHNG LIKE THIS:
basic_pop_model <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, data) # i WILL NEED PARS AND DATA
  ##what are the parameters in the RTMB mdoel?
  
  # Model Set Up (Containers) -----------------------------------------------
  n_stages = 3 # number of stages for a 3 stage model
  n_yrs = length(YEARS) # number of years
  
  # Population Stuff
  NAA = array(data = 0, dim = c(n_yrs + 1, n_stages)) # Numbers at stage
  ZAA = array(data = 0, dim = c(n_yrs, n_ages)) # Total mortality at stage #DO WE FIX THIS- FLAG
  #Total_Biom = rep(0, n_yrs) # Total biomass
  #SSB = rep(0, n_yrs) # Spawning stock biomass
  Biom_prerec = rep(0, n_yrs) #Prerecuit biomass
  Biom_rec = rep(0, n_yrs) #Recuit biomass #this is derived. Do I need to stage this here.
  Biom_postrec = rep(0, n_yrs) #Postrecuit biomass  #this is derived. Do I need to stage this here.
  
  #AGR HERE
  
  # Fishing Stuff
  Fmort = array(0, dim = c(n_yrs, n_fish_fleets)) # Fishing mortality scalar
  FAA = array(data = 0, dim = c(n_yrs, n_ages, n_fish_fleets)) # Fishing mortality at age
  CAA = array(data = 0, dim = c(n_yrs, n_ages, n_fish_fleets)) # Catch at age
  PredCatch = array(0, dim = c(n_yrs, n_fish_fleets)) # Predicted catch in weight
  fish_sel = array(data = 0, dim = c(n_ages, n_fish_fleets)) # Fishery selectivity
  
  # Survey Stuff
  SrvIAA = array(data = 0, dim = c(n_yrs, n_ages, n_srv_fleets)) # Survey index at age
  PredSrvIdx = array(0, dim = c(n_yrs, n_srv_fleets)) # Predicted survey index 
  srv_sel = array(data = 0, dim = c(n_ages, n_srv_fleets)) # Survey selectivity
  
  # Likelihoods
  Catch_nLL = array(0, dim = c(n_yrs, n_fish_fleets)) # Fishery Catch Likelihoods #it does not have to be an array. Can be "other ways"
  FishAgeComps_nLL = array(data = 0, dim = c(n_yrs, n_fish_fleets)) # Fishery Age Comps Likelihoods
  SrvIdx_nLL = array(0, dim = c(n_yrs, n_srv_fleets)) # Survey Index Likelihoods
  SrvAgeComps_nLL = array(data = 0, dim = c(n_yrs, n_srv_fleets)) # Survey Age Comps Likelihoods
  
  # Penalties
  Fmort_Pen = array(0, dim = c(n_yrs, n_fish_fleets)) # Fishing Mortality Deviation penalty
  Rec_nLL = rep(0, n_yrs) # Recruitment penalty
  Init_Rec_nLL = rep(0, n_ages - 2) # Initial Recruitment penalty
  jnLL = 0 # Joint negative log likelihood
  
  # Do some parameter transformations here
  mean_rec = exp(ln_mean_rec) # mean recruitment
  sigma_R = exp(ln_sigma_R) # recruitment variability
  sigma_F = exp(ln_sigma_F) # fishing mortality variability
  M = exp(ln_M) # natural mortality
  srv_q = exp(ln_srv_q) # survey catchability
  
  # Set Up Fishery Selectivity -----------------------------------------------------
  for(f in 1:n_fish_fleets) {
    a50_tmp = exp(ln_fish_sel_pars[1, f]) # transform and extract a50
    k_tmp = exp(ln_fish_sel_pars[2, f]) # transform and extract slope
    fish_sel[,f] = 1 / (1 + exp(-k_tmp * (1:n_ages - a50_tmp))) # logistic selectivity
  } # end f loop
  
  
  # Set Up Survey Selectivity -----------------------------------------------
  for(sf in 1:n_srv_fleets) {
    a50_tmp = exp(ln_srv_sel_pars[1, sf]) # transform and extract a50
    k_tmp = exp(ln_srv_sel_pars[2, sf]) # transform and extract slope
    srv_sel[,sf] = 1 / (1 + exp(-k_tmp * (1:n_ages - a50_tmp))) # logistic selectivity
  } # end sf loop
  ##matt notes, can also write the selectivty function... outside?
  
  # Set Up Mortality --------------------------------------------------------
  for(y in 1:n_yrs) {
    for(f in 1:n_fish_fleets) {
      Fmort[y,f] = exp(ln_F_mean[f] + ln_F_devs[y,f]) # get fishing mortality
      FAA[y,,f] = Fmort[y,f] * fish_sel[,f] # get fishing mortality at age ##easy to index stuff in RTMB, says Matt
    } # end f loop
    for(a in 1:n_ages) ZAA[y,a] = sum(FAA[y,a,]) + M # Total Mortality at age
  } # end y loop
  
  
  # Initialize Population ---------------------------------------------------
  init_age_idx = 1:(n_ages - 2) # Get initial age indexing
  NAA[1,init_age_idx + 1] = mean_rec * exp(ln_InitDevs - (init_age_idx * M)) # not plus group
  NAA[1,n_ages] = mean_rec * exp(-(n_ages - 1) * M) / (1 - exp(-M)) # geometric series solution for plus group
  
  
  # Population Projection ---------------------------------------------------
  for(y in 1:n_yrs) {
    NAA[y,1] = mean_rec * exp(ln_RecDevs[y]) # mean recruitment
    # Project Numbers at Age
    for(a in 1:n_ages) {
      if(a < n_ages) {
        # Exponential mortality for individuals not in plus group
        NAA[y+1,a+1] = NAA[y,a] * exp(-ZAA[y,a])
      } else {
        # Accumulate individuals into plus group and individuals from previous year
        NAA[y+1,n_ages] = NAA[y+1,n_ages] + NAA[y,n_ages] * exp(-ZAA[y,a])
      } # end else
    } # end a loop
    # Calculations for Biomass
    Total_Biom[y] = sum(NAA[y,] * WAA) # Total Biomass
    SSB[y] = sum(NAA[y,] * WAA * MatAA)  * 0.5 # Spawning Stock Biomass 
  } # end y loop
  
  
  # Fishery Observation Model -----------------------------------------------
  for(y in 1:n_yrs) {
    for(f in 1:n_fish_fleets) {
      CAA[y,,f] = FAA[y,,f] / ZAA[y,] * NAA[y,] * (1 - exp(-ZAA[y,])) # Get Catch at Age via Baranov's
      PredCatch[y,f] = sum(CAA[y,,f] * WAA) # get total catch
    } # end f loop
  } # end y loop
  
  
  # Survey Observation Model ------------------------------------------------
  for(y in 1:n_yrs) {
    for(sf in 1:n_srv_fleets) {
      SrvIAA[y,,sf] = NAA[y,] * srv_sel[,sf] # Get survey indexed ages
      PredSrvIdx[y,sf] = srv_q * sum(SrvIAA[y,,sf] * WAA) # get predicted survey biomass index
    } # end sf loop
  } # end y loop
  
  # Likelihoods -------------------------------------------------------------
  ## Catch -------------------------------------------------------------------
  for(y in 1:n_yrs) {
    for(f in 1:n_fish_fleets) {
      Catch_nLL[y,f] = -dnorm(log(ObsCatch[y,f]), log(PredCatch[y,f]), sigma_Catch[f], TRUE)
    } # end f loop
  } # end y loop
  
  ## Survey Index ------------------------------------------------------------
  for(y in 1:n_yrs) {
    for(sf in 1:n_srv_fleets) {
      SrvIdx_nLL[y,sf] = -dnorm(log(ObsSrvIdx[y,sf]), log(PredSrvIdx[y,sf]), sigma_SrvIdx[sf], TRUE)
    } # end sf loop
  } # end y loop
  
  ## Fishery Ages ------------------------------------------------------------
  for(y in 1:n_yrs) {
    for(f in 1:n_fish_fleets) {
      tmp_size = sum(ObsFishAges[y,,f]) # get sample size
      FishAgeComps_nLL[y,f] = -dmultinom(x = ObsFishAges[y,,f], size = tmp_size, prob = CAA[y,,f], TRUE) #CAA is catch at age
    } # end f loop
  } # end y loop
  
  ## Survey Ages ------------------------------------------------------------
  for(y in 1:n_yrs) {
    for(sf in 1:n_srv_fleets) {
      tmp_size = sum(ObsSrvAges[y,,sf]) # get sample size
      SrvAgeComps_nLL[y,sf] = -dmultinom(x = ObsSrvAges[y,,sf], size = tmp_size, prob = SrvIAA[y,,sf], TRUE)
    } # end sf loop   # x is nymbers of fish. #prob doesnt necessarily need to sum to 1 because dmultinom will do it
  } # end y loop
  
  ## Fishing Mortality Penalty -----------------------------------------------
  for(y in 1:n_yrs) {
    for(f in 1:n_fish_fleets) {
      Fmort_Pen[y,f] = -dnorm(ln_F_devs[y,f], 0, sigma_F, TRUE) #"sigma F is pretty big, so I specified it at 5-MATT"
    } # end f loop
  } # end y loop
  
  
  ## Recruitment -------------------------------------------------------------
  Init_Rec_nLL = -sum(dnorm(ln_InitDevs, -sigma_R^2/2, sigma_R, TRUE))
  Rec_nLL = -sum(dnorm(ln_RecDevs, -sigma_R^2/2, sigma_R, TRUE))
  
  # Get joint likelihood
  jnLL = sum(Catch_nLL) + sum(SrvIdx_nLL) + sum(FishAgeComps_nLL) + 
    sum(SrvAgeComps_nLL) + sum(Fmort_Pen) + sum(Init_Rec_nLL) +
    sum(Rec_nLL)
  
  # Report Section
  RTMB::REPORT(SSB)
  RTMB::REPORT(NAA)
  RTMB::REPORT(ZAA)
  RTMB::REPORT(CAA)
  RTMB::REPORT(SrvIAA)
  RTMB::REPORT(Total_Biom)
  RTMB::REPORT(jnLL)
  RTMB::REPORT(Catch_nLL)
  RTMB::REPORT(SrvIdx_nLL)
  RTMB::REPORT(FishAgeComps_nLL)
  RTMB::REPORT(SrvAgeComps_nLL)
  RTMB::REPORT(Fmort_Pen)
  RTMB::REPORT(Init_Rec_nLL)
  RTMB::REPORT(Rec_nLL)
  RTMB::REPORT(PredCatch)
  RTMB::REPORT(PredSrvIdx)
  RTMB::REPORT(fish_sel)
  RTMB::REPORT(srv_sel)
  
  return(jnLL)
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
#update the misc Juneau tables that I need (parallel the CSV) (only for the RKC juneau survey area)



##################################################################
#the update the biomass csv part


###DEVELOPMENT WORKFLOW
##1. create input dataset (the CSA starting values)
##2. create the RTMB code - to run the CSA starting values and get CSA ending calues
##3. Any other tables that I want

#Q: so WHY does this have to be in RTMB? Why was optim unstable?


##TRASH CAN
####################################################################
#The CSA part - in RTMB

#gonna want some initial parameters (WHAT ARE THEY??)
##WHICH WILL GO HERE

#define the objective function for the CSA model

##something like: #this is a LINEAR MODEL EXAMPLE right now
nll <- function(par) {
  pred <- par$alpha + par$beta * dat$x
  -sum(dnorm(dat$y, pred, exp(par$logSigma), log = TRUE))
}

#hmmm thinkinh about how my CSA works
