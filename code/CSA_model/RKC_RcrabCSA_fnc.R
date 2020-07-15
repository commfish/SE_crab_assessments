# K.Palof updated 9-8-16
# Red king crab function for CSA model - use alternative fnc for bootstrap to speed it up.  

#trial1 <- RcrabCSA1 (year = JNUred$Year, catch = JNUred$Catch..Number., preR = JNUred$PreR, 
#                   recr = JNUred$Recruit, post = JNUred$PostR, csT= JNUred$Catch..Survey.Tau, 
#                   sTs =JNUred$Survey.Tau, LegWt=JNUred$Legal.Weight, 
#                   PreRWt=JNUred$Prerecruit.Weight, M = 0.30, 
#                   w = JNUred$w, initial = c(2.00, 2.80, 1.10, 101.10, 79.02), 
#                   uprn = 1000000, graph = TRUE)
##################FUNCTION CODE STARTS HERE ########################
######################################################################
###################################################################

# parameter values for q and s (the last 2 here) are rescaled for 
# easier estimation.  See cs function for rescaling.

RcrabCSA1 <- function (year = NULL, catch = NULL, preR = NULL, recr = NULL, 
                       post = NULL, csT=NULL, sTs=NULL, LegWt=NULL, PreRWt=NULL,
                       M = NULL, w = NULL, 
                       initial = c(NA, NA, NA, NA, NA), uprn = NA, graph = TRUE)
{  
  if (is.null(year))
    stop("missing year vector")
  if (is.null(year))
    stop("missing year vector")
  outs <- NULL
  yrs <- length(year) # creates a value for the number of years
  
  lower <- c(rep(0.001, yrs), 0.001,0.001, 0.01, 0.1) 
  #creates a vector with 21 spaces, all 1 except the last is 0 
  # q and s are scaled parameters so scale lower bounds
  upper <- c(rep(uprn, yrs +4)) # same as above except with upper bound given above
  PRs <- rep(initial[1], yrs) # initial estimates of PreR, needs to be estimated every year
  #the first value in intial vector is placed here as a starting value
  ### this PR is NOT numbers its CPUE or index
  rest <- c(rep(0, yrs)) #should be set up the same as the nest vector
  nest <- c(rep(0, yrs))
  #Z <<- c(rep(0, yrs)) # vector for instantaneous total mortality
  parms <- c(PRs, initial[2], initial[3], initial[4], initial[5])
  # Vector of Pre-recruits, intial values for PreR, R, and Post, q and s (pReR to R survival)
  # [5] is survival from PreR to R
  TPSS <- NULL
  TRSS <- NULL
  TPRSS <- NULL
  prest <- NULL # place holder for estimate of pre-recruits index predicted each year
  #x is the parms vector - DUH!
  cs <- function(x) {
    prest <<- x[1:yrs]
    for (i in 1:yrs) {
      if (i == 1) 
        rest[1] <<- x[yrs + 1] # included for recruit estimated index
      nest[1] <<- x[yrs + 2] # this is post-recruit estimated index
      if (i > 1) {
        rest[i] <<- max(0.001, prest[i - 1] * ((x[yrs + 4])/100)) #rescaling s
        nest[i] <<- max(0.001, ((nest[i - 1] + rest[i - 1]) * exp(-M * sTs[i]) 
                                - (x[yrs + 3]/1000000) * catch[i - 1] * exp(-M * csT[i])))
        # need to rescale q here
      }
    }
    TPSS <- 0
    TRSS <- 0
    TPRSS <- 0 #need one for PreR now
    
    TPRSS <- sum((((log(preR[seq(1, yrs, 1)]+0.001) - 
                      log(prest[seq(1, yrs, 1)]+0.001)) ^2)*w[seq(1,yrs,1)]), na.rm = T) # second 0.001 was a 0.002 why?? changed this 8-14-15
    TPSS <- sum((((log(post[seq(1, yrs, 1)]+0.001) - 
                     log(nest[seq(1, yrs, 1)]+0.001)) ^2)*w[seq(1,yrs,1)]), na.rm = T)
    TRSS <- sum((((log(recr[seq(1, yrs, 1)]+0.001) - 
                     log(rest[seq(1, yrs, 1)]+0.001)) ^2)*w[seq(1,yrs,1)]), na.rm = T)
    TPSS + TRSS + TPRSS # all summed together with weighting.  
  }
  outs <- optim(parms, cs, gr = NULL, lower = lower, upper = upper, 
                method = c("L-BFGS-B"), control = list(maxit = 1e+09), 
                hessian = T) #did not change this outs call
  
  #could be a scaling problem go back and rescale like Excel
  s <- ((outs$par[yrs+4])/100)
  q <- ((outs$par[yrs+ 3])/1000000)
  cov <- solve(outs$hessian) #didn't change
  est_se <- sqrt(abs(diag(cov))) # changed this to the absolute value 
  upper_CI <- outs$par + 1.96*est_se
  lower_CI <- outs$par - 1.96*est_se
  
  PR <- prest/q
  R <- rest/q
  N <- nest/q
  #TA <- PR + R + N
  #Z[seq(1, yrs, 1)] <- -log(N[seq(2, yrs, 1)]/(R[seq(1, 
  #                                                     yrs - 1, 1)] + N[seq(1, yrs - 1, 1)]))
  #Fmort <- Z - M
  out1 <- outs$value
  
  Legal <- R + N
  Mature <- PR + R + N
  #out4 <- cbind(Legal, Mature) 
  Legal_Biomass <- Legal*LegWt
  Pre_Biomass <- PR*PreRWt
  Mature_Biomass <- Legal_Biomass + Pre_Biomass
  
  out2 <- data.frame(year, prest, rest, nest, PR, R, N, Legal, Mature, 
                     Pre_Biomass, Legal_Biomass, Mature_Biomass)
  
  #out5 <- cbind(Pre_Biomass, Legal_Biomass, Mature_Biomass)
  out3 <- cbind(q, s)
  out6 <- cbind(outs$par, lower_CI, upper_CI)
  
  
  #output <- list(out2, out3, out4, out5, out1, out6, outs)
  output <- list(estimates = out2, parms = out3, SSQ=out1, CI = out6, outs)
  #names(output) <- c("q", "Estimates PreR", "Estimates R", "Estimates Post")
  
  if (graph == TRUE) {
    par(mfrow = c(2, 2))
    plot(x = year, y = preR, col = "black", ylab = "Survey Index", 
         xlab = "Year", main = "Pre-Recruit (O=black, P=red)", 
         ylim = c(0, max(preR, out2[, 2], na.rm=TRUE)))
    lines(x = year[1:yrs], y = out2[, 2], col = "red")
    plot(x = year, y = recr, col = "black", ylab = "Survey Index", 
         xlab = "Year", main = "Recruit (O=black, P=red)", 
         ylim = c(0, max(recr, out2[, 3], na.rm=TRUE)))
    lines(x = year[1:yrs], y = out2[, 3], col = "red")
    plot(x = year, y = post, col = "black", ylab = "Survey Index", 
         xlab = "Year", main = "Post-Recruit (O=black, P=red)", 
         ylim = c(0, max(post, out2[, 4], na.rm=TRUE)))
    lines(x = year, y = out2[, 4], col = "red")
    plot(x = year[1:yrs], y = out2[, 9], type = "l", 
         col = "black", ylab = "Mature & Legal Numbers", xlab = "Year", main = "Stock Abundance", 
         xlim = c(min(year), max(year)), ylim = c(0, max(out2[,9], na.rm=TRUE)))
    lines(x=year, y= out2[,8], col = "red")
    #plot(x = year[1:yrs - 1], y = out2[, 5], type = "l", 
    #    col = "black", ylab = "F", xlab = "Year", main = "Fishing Mortality", 
    #   xlim = c(min(year), max(year[1:yrs - 1])))
  }
  return(output)
}
