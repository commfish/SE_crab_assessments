#RTMB functions

library(RTMB)

#draft juneau function:
basic_pop_model <- function(pars) {
  
  # get parameters and data
  RTMB::getAll(pars, data) #or can write out as in the RTMB vonbert example
  
  # Model Set Up (Containers) -----------------------------------------------
  n_stages = 3 # number of stages for a 3 stage model
  n_yrs = length(YEARS) # number of years #this should be correct, I put years (includign the NAs) in the data
  
  # Population Stuff
  #CPUE_AS = array(data = 0, dim = c(n_yrs + 1, n_stages)) # Numbers at stage, adds one for this year
  SSB = array(0, dim = c(n_yrs, n_stages)) # Pre-rec, legal, and mature biomasses
  
  # Survey Stuff
  PredSrvCPUE = array(data = 0, dim = c(n_yrs, n_stages)) # Predicted CPUE at stage PREREC, REC, POSTREC #THE MATRIX MODEL
  PredSrvIdx = array(0, dim = c(n_yrs, n_stages)) # predicted biomass calculated from the predicted survey CPUE and waa
  
  #LINK SURVEY DATA TO MATRIX PREDICTIONS TO LIKELIHOOD. 
  
  # Likelihoods - box
  #SrvIdx_nLL = array(0, dim = c(n_yrs, n_stages)) # Survey Index Likelihoods - this replaces the sum of squares - one likelihood for each year and each stage - summed by row and then summed by year
  SrvIdx_nLL = rep(0,3) #AGR changed to this 6/12/25
  
  #identify a vector where there are no NA's in the data- for the likelihood loop
  #no_NAs <- data.frame(CPUE_rec) %>% #not a df. Need to do this for just a numeric item in a list
  #  rownames_to_column() %>%
  #  filter(!is.na(CPUE_rec)) %>% #will tell us where NA's are in the data
  #  select(rowname)
  # no_NAs <- as.integer(no_NAs$rowname) #not srue if I still need this
  
  
  # Penalties #I don't need penalties?? do I?
  #Rec_nLL = rep(0, n_yrs) # Recruitment penalty - AR- why is this a penalty?? **FLAG**
  #Init_Rec_nLL = rep(0, n_ages - 2) # Initial Recruitment penalty #and why is there an initial?? ***FLAG**
  jnLL = 0 # Joint negative log likelihood #this I need
  
  # Do some parameter transformations here 
  mean_rec = exp(ln_mean_rec) # mean recruitment
  q = exp(ln_q) # survey catchability
  T12 = exp(ln_T12)
  sigma_survey = exp(ln_sigma_survey) # survey index error
  init_rec_cpue = exp(ln_init_rec_cpue) # initial recruit CPUE
  init_postrec_cpue = exp(ln_init_postrec_cpue) # initial postrecruit CPUE
  Eps_R = exp(ln_Eps_R) # recruitment deviates
  #survey_data = exp(log_survey_data) #delete later perhaps
  WEIGHTS = 1.0/(2* sqrt(log(1.0+survey_data[,3,]^2))^2)   #turn CV into weights
  survey_data <- abind(survey_data, WEIGHTS, along=2) ## add weights to the 3d array
  
  
  # Initialize Population ---------------------------------------------------
  
  #pop initialization
  PredSrvCPUE[1,] <- c( 
    #Pre_CPUE_prerec_calc <- Eps_R[1] * mean_rec + mean_rec, #Tyler said pick additive or multiplicative, having both is weird
    Pre_CPUE_prerec_calc <- Eps_R[1] * mean_rec,
    #Pre_CPUE_prerec_calc <- Eps_R[1] + mean_rec,
    Pred_CPUE_rec_calc = init_rec_cpue,
    Pred_CPUE_postrec_calc = init_postrec_cpue 
  )
  
  #Pop projection
  
  #softplus <- function(x) log1p(exp(x)) #hmm. This subjectively raises my postrecruit starting values and I'm not sure how I feel about that
  #softplus <- function(x) log(0.5 + exp(x)) #ugh. seems subjective.
  
  for (t in 2:n_yrs){
    #predSrvCPUE[t,] <- c(
    #years = YEARS,
    PredSrvCPUE[t,1] = Eps_R[t] * mean_rec
    PredSrvCPUE[t,2] = T12*PredSrvCPUE[t-1, 1] #using my rtmb calcs instead of the read-in data
    #PredSrvCPUE[t,3] = softplus((PredSrvCPUE[t-1, 2] + PredSrvCPUE[t-1, 3]) * exp(-S * SURVEY_TAU[t]) - (q*CATCH[t-1]*exp(CATCH_SURVEY_TAU[t]*-S))) #cant be neg
    PredSrvCPUE[t,3] = max((PredSrvCPUE[t-1, 2] + PredSrvCPUE[t-1, 3]) * exp(-S * SURVEY_TAU[t]) - (q*CATCH[t-1]*exp(CATCH_SURVEY_TAU[t]*-S)), 0.0001)
  } #ok cool, got the pop (CPUE) projection in there.
  
  #########
  ##POPULATION CALC IN LOGSPACE
  #PredSrvCPUE[1,] <- c( 
  #  Pre_CPUE_prerec_calc <- ln_Eps_R[1] + ln_mean_rec, #logspace; Eps_R[t] * mean_rec in regular space
  #  Pred_CPUE_rec_calc = ln_init_rec_cpue,
  #  Pred_CPUE_postrec_calc = ln_init_postrec_cpue 
  #)
  
  #for (t in 2:n_yrs){
  #  PredSrvCPUE[t,1] = ln_Eps_R[t] + ln_mean_rec #logspace equation of:  Eps_R[t] * mean_rec
  #  PredSrvCPUE[t,2] = ln_T12 + PredSrvCPUE[t-1, 1] #logspace equation of: T12*PredSrvCPUE[t-1, 1]
  #  x= log(exp(PredSrvCPUE[t-1, 2]) + exp(PredSrvCPUE[t-1, 3])) + (-S * SURVEY_TAU[t])
  #  y= ln_q + log(CATCH[t-1]) - S * CATCH_SURVEY_TAU[t]
  #  PredSrvCPUE[t, 3] <- x + log(1 - exp(y - x)) #check is correct
  #NOPredSrvCPUE[t,3] = log(exp(PredSrvCPUE[t-1, 2]) + exp(PredSrvCPUE[t-1, 3])) + (-S * SURVEY_TAU[t]) + log(1-exp(ln_q) + log(CATCH[t-1]) - S * CATCH_SURVEY_TAU[t]) #using my rtmb calcs instead of the read-in data
  
  #} 
  
  #####
  
  
  #calc the biomass per year for prerecruit, recruit, and postrecruit legal and mature
  PredSrvIdx[,1] <- (PredSrvCPUE[,1]/q) * wt_prerec #prerecruit biomass = prerecruit cpue/catchability * the weight 
  PredSrvIdx[,2] <- ((PredSrvCPUE[,2]+PredSrvCPUE[,3])/q) * wt_legal #legal biomasss = recruit cpue + postrecruit cpue, divided by catchability, times the legal weight
  PredSrvIdx[,3] <- PredSrvIdx[,1]+PredSrvIdx[,2] #mature biomass =  legal biomass + prerecruit biomasss
  
  # Likelihoods -------------------------------------------------------------
  
  ## Survey Index ------------------------------------------------------------
  #lambdas <- survey_data[,3,1] #the to, from CV conversion should be here (see tyler solution code and emails)
  #holder <- array(0, dim = c(44, 4, 3))
  pred <- numeric(nrow(survey_data[,,1])) #could be better assigned
  #for(y in 1:n_yrs) { #make a vector that skips the missing years TODO
  #for(y in no_NAs) { #this one skips missing years #oops, needs to be the years??- FLAG**
  for(h in 1:n_stages) {
    for(y in 1:nrow(survey_data[,,h])){
      y_row <- which(YEARS == survey_data[y,1,h])#[y, 1]) #index the nrow of the model matrix that year y of survey data corresponds to
      pred[y] <- PredSrvCPUE[y_row, h] # vector of predictions for stage h in only years that we have data. FLAG Negatives. NEgatives not ok? No negative CPUE... Log?
    }
    SrvIdx_nLL[h] = -sum(dnorm(survey_data[,2,h], pred, sigma_survey, TRUE) * survey_data[,4,h] ) #the likelihood. Pred is going negative. this bad? FLAG** #survey_data[,4,h] is weights
    # add predictions to the data for the report however you want to
    #holder[,,h] <- cbind(survey_data[,,h], pred) #I dont get why I have this. FLAG. MY data is spit out elsewhere.
  } #end of st(stage) loop
  
  
  
  ## Recruitment ------------------------------------------------------------- PERHAPS ADD THIS LATER
  #Init_Rec_nLL = -sum(dnorm(ln_InitDevs, -sigma_R^2/2, sigma_R, TRUE)) #I am unsure if these stay for the crab CSA.. this will be the next addition if not now, at least
  #Rec_nLL = -sum(dnorm(Eps_R, -sigma_R^2/2, sigma_R, TRUE)) #use if ranefs
  #for(y in 1:n_yrs) {
  #   Rec_nLL[y] = -dnorm(Eps_R, -sigma_R^2/2, sigma_R, TRUE) #* lambdas[y] #try adding weights here  #that ran poorly
  #}
  
  # Get joint likelihood
  jnLL = sum(SrvIdx_nLL)
  #jnLL = sum(SrvIdx_nLL) + sum(Rec_nLL) #we're keeping it simple for the crab CSA
  
  
  
  # Report Section
  #I'll want CV's for graphing
  #CVs <- survey_data[,3,1]
  
  #RTMB::ADREPORT(SSB)# Mature and Legal biomasses, and error
  RTMB::REPORT(sigma_survey) #I want my error. Will have to add in other error sources later??
  #RTMB::ADREPORT(PredSrvIdx) #survey biomass by stage
  RTMB::REPORT(PredSrvIdx) #REPORT or ADREPORT?? *FLAG* #the pred biomass
  #RTMB::ADREPORT(PredSrvCPUE) #predicted survey CPUE by stage
  RTMB::REPORT(PredSrvCPUE) #REPORT or ADREPORT?? *FLAG* #the pred cpue
  RTMB::REPORT(jnLL)
  RTMB::REPORT(q)
  RTMB::REPORT(T12)
  #RTMB::REPORT(holder) # a better name for this one perhaps. #does not convert to df because only 44 entries.
  #RTMB::ADREPORT(mean_rec)
  RTMB::REPORT(Eps_R) #annual recruitment
  RTMB::REPORT(survey_data)
  
  return(jnLL) #do I need this too?
}
#END POP MODEL