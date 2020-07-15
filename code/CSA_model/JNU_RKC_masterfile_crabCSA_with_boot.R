## AREA:Juneau
### SPECIES:Red crab
#### YEAR: 2018
#Date modified: 7-16-14 / 8-5-14 / 7-9-15/ 9-27-16/7-3-17/7-12-18(clean up for 2018)

rm(list=ls())
##Load packages-------------
library(tidyverse)
##Load data --------------------------
#JNUred <- read.csv("./data/Juneau2017RKC.csv")
JNUred <- read.csv("./data/Juneau2018RKC.csv")
#file name needs to be changed to reflect area and species

str(JNUred)

##### Load functions --------------------
source("./functions/RKC_RcrabCSA_fnc.R")# sources the file with the model code
source("./functions/graph_fnc_CSA.R")# sources the file with the graphing function for the bootstrap
#   OR 
########STOP and make sure crabCSA function is loaded##########
## open "RKC_Rcrab CSA fnc.R" and load function 

##### Run model ----------------------------------
## Notes: For RED KING CRAB need to adjust function for weighting variable to be different in each year.
#### HERE:
# For other areas  - Name of file and variables will need to be changed, 
#   initial(PreR initial each year, R in the 1st year, Post in the first year, q scaled by 100, 
        #          s scaled by 1,000,000)
        # M = 0.30 for Tanner crab AND M = 0.32 for Red Crab
        # w is a vector of weigthings found in the data file. 
# use input initial and M from Excel spreadsheet
JNU_RKC_fit1 <- RcrabCSA1 (year = JNUred$Year, catch = JNUred$Catch..Number., 
                          preR = JNUred$PreR, 
                   recr = JNUred$Recruit, post = JNUred$PostR, csT= JNUred$Catch..Survey.Tau, 
                   sTs =JNUred$Survey.Tau, LegWt=JNUred$Legal.Weight, 
                   PreRWt=JNUred$Prerecruit.Weight, M = 0.32, 
                   w = JNUred$w, initial = c(1.44, 2.02, 0.907, 103.99, 82.27), 
                   uprn = 1000000, graph = TRUE)

JNU_RKC_fit1
JNU_RKC_fit1$estimates
# save model output
write.csv(JNU_RKC_fit1$estimates, './output/2018/JNU_RKC_fit1_estimates.csv')

write.csv(JNU_RKC_fit1$CI, './output/2018/JNU_RKC_fit1_par&CI.csv')
write(JNU_RKC_fit1$SSQ, file = './output/2018/JNU_SSQ.txt')
### save graphical output also - DO THIS manually, I have NOT automated this step.

#########STOP  --------------------------
##### BOOTSTRAP ---------------------------------
##     go to below ### BOOTSTRAP FUNCTION
##      and follow instructions BEFORE
###    loading crabboot function - LOAD the specific function in this file.
##############################
##### Run bootstrap -------------------------
# WARNING ! Do NOT run 1,000 reps now.  This will take a LONG time.  B=1000 is preferred but 
#             here B is smaller to test the boostrap function.
source("./functions/RKC_RcrabCSA_fnc_for_boot.R")

JNU_RKC_boot_fit <- crabbootJNU(dataset=JNUred, CSAoutput=JNU_RKC_fit1, B=50)

#JNU_RKC_boot_fit$quantCI
#  getting errors so intitial values aren't changing...how to fix this???

#### bootstrap save ----------------------------------
write.csv(JNU_RKC_boot_fit$est, file = './output/2018/JNU_RKC_boot_estimate.csv')
write.csv(JNU_RKC_boot_fit$quantCI, file = './output/2018/JNU_RKC_boot_quantiles.csv')



#################  GRAPHS ------------------------------------
# These need to be loaded from the 'graph_fnc_CSA.R' file   
# Attempts to graph bootstrap results
# Biomass estimates are in crabCSA output as $est
## Quantiles ci's are in bootstrap output$quantCI    ###
# graph is now a function of the CSAoutput and the bootstrap output.
#  Also need to give this function a title - area and species, and a min y value

jpeg(filename = './figures/2018/JNU_biomass_bootout.jpg')
crabboot.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
               title = "JNU_RKC red crab", min= 0) 
dev.off()
#  plots both legal and mature in 2 ways.
jpeg(filename = './figures/2018/JNU_biomass_legal_boot.jpg')
crabboot.Legal.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
                     title = "JNU_RKC Legal crab", min= 0) 
dev.off()
# plots just legal shaded
jpeg(filename = './figures/2018/JNU_biomass_mature_boot.jpg')
crabboot.Mature.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
                     title = "JNU_RKC Mature crab", min= 0) 
dev.off()

jpeg(filename = './figures/2018/JNU_biomass_boot_larger.jpg')
crabboot.LM.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
                     title = "JNU_RKC Legal & Mature crab", min= 0) 
dev.off()
# plots both legal and mature - shaded

################BOOTSTRAP FUNTION ----------------------------------------
# meant for use after the crabCSA function because it uses inputs from this previous function.

# confirm data is loaded
#head(JNUred)  #or whatever the name of the data set is

####
#########
#steps:
#1. Years should be read automatically from input but confirm this below
#2. make sure that the original data file and the RcrabCSA output exist, these are input into 
##   this function.
#3. copy call to "RcrabCSA1" code into this function (from above)
            #(not the function just the call to the function from original run), make sure
            # the call here is to "RcrabCSA1B" - slightly different for bootstrap.
#4. make sure that in this function the PreR, R, and Post input refer to the bootstrap NOT the original data
#5. Make sure that graph= FALSE in crabCSA function

### test here after loading.
JNU_RKC_boot_fit <- crabbootJNU(dataset=JNUred, CSAoutput=JNU_RKC_fit1, B=1) 

##### LOAD this -----------------------------------------
source("./functions/RKC_RcrabCSA_fnc_for_boot.R")
crabbootJNU <- function (dataset=NULL, CSAoutput=NULL, B=NULL){
  est<- CSAoutput$estimates
  dat1 <- cbind(dataset, est[,2:4])
  #calculate log residuals
  dat1$LresidPR <- log(dat1$PreR) - log(dat1$prest)
  dat1$LresidR <- log(dat1$Recruit) - log(dat1$rest)
  dat1$LresidN <- log(dat1$PostR) - log(dat1$nest)
  #calculate SD of residuals for each data input  
  sd_LresidPR <- sd(dat1$LresidPR, na.rm=TRUE)
  sd_LresidR <- sd(dat1$LresidR, na.rm = TRUE)
  sd_LresidN <- sd(dat1$LresidN, na.rm = TRUE)
  
  yrs <- length(dat1$Year)
  boot.par <- matrix(nrow=B, ncol= (2*yrs)+3) # could also be # yrs*2+3
  # want to keep legal and mature biomass for each year (17) for each run, 
  #######   and keep estimates of q, s, and SSQ just for the hell of it
  #dimnames(boot.par) <-list(NULL, c(1979:2014, 1979:2014,"q", "s", "SSQ"))
  dimnames(boot.par) <- list(NULL, c(min(dat1$Year):max(dat1$Year), 
                                     min(dat1$Year):max(dat1$Year),"q", "s", "SSQ"))
  
  for (i in 1:B){
    # resample and get new PR, R, and N index input
    PR.boot <- exp(log(dat1$prest) + rnorm(yrs, sd=sd_LresidPR)+
                     ((sd_LresidPR^2)/2))
    R.boot <- exp(log(dat1$rest) + rnorm(yrs, sd=sd_LresidR)+
                    ((sd_LresidR^2)/2))
    N.boot <- exp(log(dat1$nest) + rnorm(yrs, sd=sd_LresidN)+
                    ((sd_LresidN^2)/2))
    #starting values and input same as in original function.
    
    fit.boot <- RcrabCSA1B (year = JNUred$Year, catch = JNUred$Catch..Number., 
                          preR = PR.boot, 
                          recr = R.boot, post = N.boot, csT= JNUred$Catch..Survey.Tau, 
                          sTs =JNUred$Survey.Tau, LegWt=JNUred$Legal.Weight, 
                          PreRWt=JNUred$Prerecruit.Weight, M = 0.32, 
                          w = JNUred$w, initial = c(1.47, 2.00, 0.90, 101.10, 79.02), 
                          uprn = 1000000, graph = FALSE)
     
    #need to figure out how to get output from function into boot.par
    boot.par[i, ] <- c(fit.boot$estimates[,11],fit.boot$estimates[,12], 
                       fit.boot$parm, fit.boot$SSQ)
    write.csv(boot.par, file = "boot.par.csv")
    
  }
  ########## Quantile, percentile confidence intervals
  ## need a quantile for each year biomass 
  boot.yearQ <- matrix(nrow=yrs, ncol= 4)
  #boot.yearQ
  #dimnames(boot.yearQ) <-list((1979:2014), c("L_0.025", "L_.975",
  #                                                       "M_0.025", "M_.975"))
  dimnames(boot.yearQ) <-list(min(dat1$Year):max(dat1$Year), c("L_0.025", "L_.975",
                                                               "M_0.025", "M_.975"))
  
  for (i in 1:yrs) {
    boot.yearQ[i,] <- c((quantile(boot.par[,i],c(0.025,0.975))),
                        (quantile(boot.par[,i+yrs],c(0.025,0.975))))
  }
  #name.boot.yearQ <- boot.yearQ # gives the quantiles for each year for both legal and mature biomass.
  
  
  
  output <- list(est = boot.par, quantCI = boot.yearQ)
  return(output)
}

##################### END / STOP ------------------------------------------------

#####load bootstrap results back in - when opening new session -------------------
#boot.par <- read.csv("JNU_RKC_boot_estimate.csv")
#boot.par <- boot.par[ ,-1] # need to remove the first column which has the rep number in it

#head(boot.par)

#SSQ quantiles
#boot.par$SSQ
#hist(boot.par$SSQ)
#mean(boot.par$SSQ)
#quantile(boot.par$SSQ, c(0.025, 0.975))

#write.csv(boot.yearQ, file = "JNU_RKC_boot_quantiles_trial1.csv")

##### END -----------------------------------------

#should end up with boot_fit file that has biomass (legal and mature), and
#estimates of q, s, and SSQ for each bootstrap replication.  This file also
#contains "quantCI" which are 2.5% and 97.5% confidence bounds for biomass (L and M)
#in each year, assuming years are 1979-2014.