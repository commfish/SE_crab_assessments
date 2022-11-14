# notes ----


# load ----

library(TMB)
library(tidyverse)
library(lubridate)


# data ----

datfile <- "./lynn_sisters_csa.DAT"

# extract data objects, compute taus, combine in list
{dat <- as.matrix(read.table(datfile, fill = T, row.names = NULL, header = F, col.names = paste0("V",seq_len(100))))
colnames(dat) <- NULL

# yrs
yrs <- dat[1,1]:dat[1,2]
# stages
nstage <- dat[2,1]
# natural mortality
M <- dat[3,1]
# catch_num
end = 2+length(yrs)
catch_num <- as.numeric(dat[4:end, 2])
# catch mid date
catch_md <-  as.numeric(dat[4:end, 3])
# obs index
start = end+1
end = start+length(yrs)-1
obs_index <- dat[start:end, 2:(nstage+1)]
# index mid date
index_md <- dat[start:end, 5]
# wt survey
start = end+1
end = start+length(yrs)-1
wt_survey <- dat[start:end, 1:3]
# avg wt (lbs)
start = end+1
end = start+length(yrs)-1
avg_wt <- dat[start:end, 1:2]
# tau cs
tibble(catch_md = c(catch_md, NA),
       index_md = index_md) %>%
  mutate(catch_md_shift = lag(catch_md, 1)) %>%
  mutate(tau = (index_md - catch_md_shift) / 365) %>%
  pull(tau) %>% na.omit() %>% as.numeric() -> tau_cs
# tau s
tibble(yr = yrs,
       leap = yr/4,
       leapl = leap == round(leap, 0),
       index_md = index_md,
       index_md_lead = lead(index_md)) %>%
  mutate(tau = (index_md_lead + (365 + leapl - index_md)) / (365)) %>%
  pull(tau) %>% .[1:(length(index_md) - 1)] -> tau_s

input <- list(yrs = yrs, catch_num = catch_num, obs_index = obs_index, tau_cs = tau_cs, tau_s = tau_s, M = M, wt_survey = wt_survey, avg_wt = avg_wt)

}

# model ----

## compile and load model
compile("./CSA_TANNER.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("./CSA_TANNER"))

# load parameter starting values from pin file
{
  pinfile <- "./tanner_csa.pin"

ln_index_init <- scan(pinfile, skip = 2, n = 2, quiet = T)
trans_probs <- scan(pinfile, skip = 4, n = 4, quiet = T)
ln_Rbar <- scan(pinfile, skip = 6, n = 1, quiet = T)
Eps_R <- scan(pinfile, skip = 8, n = length(yrs), quiet = T)
ln_sigmaR <- scan(pinfile, skip = 10, n = 1, quiet = T)
preM <- scan(pinfile, skip = 12, n = 1, quiet = T)
ln_q <- scan(pinfile, skip = 14, n = 1, quiet = T)
ln_mu <- scan(pinfile, skip = 16, n = 1, quiet = T)

pin <- list(ln_index_init = ln_index_init, trans_probs = trans_probs, ln_Rbar = ln_Rbar,
            Eps_R = Eps_R, ln_sigmaR = ln_sigmaR, preM = preM, ln_q = ln_q, ln_mu = ln_mu)
}

# map parameters that should be fixed
map <- list(trans_probs = rep(factor(NA), 4), ln_sigmaR = factor(NA), ln_Rbar = factor(NA), ln_mu = factor(NA) )
obj <- TMB::MakeADFun(data = c(recruit_likelihood = 0,
                          recruit_stage = 2,
                          preM_tau = 0,
                          input),
                 parameters = pin,
                 DLL = "CSA_TANNER", silent = T , map = map, hessian = T)

opt <- nlminb(start = obj$par, obj = obj$fn, gr = obj$gr, control = list(eval.max = 100000, iter.max = 1000))
sdreport(obj)
obj$report()

ggplot()+
  geom_point(aes(x = yrs, y = input$obs_index[,2]))+
  geom_line(aes(x = yrs, y = re$index[,2]))+
  geom_line(aes(x = yrs, y = std$index[,2]), col = "red")




