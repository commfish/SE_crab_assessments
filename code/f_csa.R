# notes ---- 
## catch survey analysis following collie et al. 2005
## written by: Tyler Jackson

# csa function----
f_csa = function(file_path){
# load library ----
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse",quiet = T)}
library(tidyverse, quietly = T)

# setup functions ----
## read dat file
f_read_csa_dat <- function(file_path){
  
  dat <- read.table(file_path, fill = T, row.names = NULL, header = F, col.names = paste0("V",seq_len(100)))
  
  # number of years
  start_yr = as.numeric(dat[1,1])
  end_yr = as.numeric(dat[2,1])
  
  # number of stages
  n_stages = as.numeric(dat[3,1])
  
  # natural moratlity
  M = as.numeric(dat[4,1])
  
  # starting values
  ## pre recruit natural mortality
  preM_start = as.numeric(dat[5,1])
  ## catchability
  q_start = as.numeric(dat[6,1])
  
  # number of boostrap iterations
  boot_iterations = as.numeric(dat[7,1])
  
  # number of boostrap iterations
  alpha = as.numeric(dat[8,1])
  
  # stage first subject to fishing
  #Fstage = as.numeric(dat[4,1])
  
  # catch timeseries
  startrow = 11
  endrow = startrow+as.numeric(dat[(startrow-2),1] - 1)
  catch = dat[startrow:endrow,1:3]
  names(catch) = c("yr", "yield", "doy_mid")
  catch_units = dat[startrow - 1, 1]
  
  # survey index
  startrow = endrow+2
  endrow = startrow+as.numeric(dat[endrow+1,1])-1
  index = dat[startrow:endrow, 1:(n_stages + 1)]
  names(index) = c("yr", paste0("stage", 1:n_stages))
  index_mid = as.numeric(dat[endrow+1, 1:as.numeric(dat[(startrow-1),1])])
  
  # survey weights
  weights = as.numeric(na.omit(as.numeric(dat[endrow+2, 1:as.numeric(dat[(startrow-1),1])])))
  
  # avg weights for biomass calc
  avg_wts_units = as.numeric(dat[endrow+3, 1])
  startrow = endrow+4
  endrow = startrow+nrow(index)-1
  avg_wts = dat[startrow:endrow, 1:(n_stages + 1)]
  
  # organize data
  list(model_yrs = start_yr:end_yr,
       n_stages = n_stages,
       M = M,
       preM_start = preM_start,
       q_start = q_start,
       boot_iterations = boot_iterations,
       alpha = alpha,
       #Fstage = Fstage,
       catch_units = catch_units,
       catch = catch,
       index = index,
       index_mid = index_mid,
       weights = weights,
       avg_wts_units = avg_wts_units,
       avg_wts = avg_wts) -> dat_list
  
  return(dat_list)
  
}
## compute tau's
f_tau_cs <- function(dat_list){
  # join survey year to mid doy
  tibble(dat_list$catch) %>%
    rename(catch_mid = doy_mid) %>%
    full_join(tibble(yr = dat_list$index$yr,
                     index_mid = dat_list$index_mid), by = "yr") %>%
    mutate(catch_mid_shift = lag(catch_mid, 1)) %>%
    # filter to only include survey years
    filter(!is.na(index_mid)) %>%
    # compute tau
    mutate(tau = (index_mid - catch_mid_shift) / 365) %>%
    pull(tau) 
}
f_tau_s <- function(dat_list){
  # compute tau while accounting for leap year
  tibble(yr = sort(dat_list$index$yr),
         leap = yr/4,
         leapl = leap == round(leap, 0),
         index_mid = dat_list$index_mid,
         index_mid_lead = lead(index_mid)) %>%
    mutate(tau = (index_mid_lead + (365 + leapl - index_mid)) / (365)) %>%
    pull(tau) %>%
    .[1:(length(dat_list$index_mid) - 1)]
}
## estimate predicted survey index
f_pred_index <- function(index, catch, tau_cs, tau_s, M, ln_pars){
  
  # catch timeseries corresponding to survey timeseries
  cs = filter(catch, yr %in% index$yr)$yield
  
  # define parameters based on location in vector and exponentiate
  preM = exp(ln_pars[1]) ## pre recruit M
  q = exp(ln_pars[2]) ## catchability
  idx_init = exp(ln_pars[3:(3+ncol(index)-3)]) ## inital index per stage
  rec = exp(ln_pars[(3+ncol(index)-2):length(ln_pars)]) ## recruitment
  
  # matrix of preicted survey index
  pred_index = matrix(nrow = nrow(index), ncol = ncol(index) - 1)
  pred_index[1,2:ncol(pred_index)] = idx_init
  pred_index[,1] = rec
  # fill predicted index matrix
  for(i in 2:ncol(pred_index)){
    for(j in 2:nrow(pred_index)){
      if(i == 2){pred_index[j, i] = pred_index[j-1, i-1] * preM}
      if(i == (ncol(index) - 1)){pred_index[j, i] = max(0, ((pred_index[j-1, i-1] + pred_index[j-1, i]) * exp(-M*tau_s[j-1])) - (q * cs[j-1] * exp(-M*tau_cs[j])))}
    }
  }
  
  return(pred_index)
}
## define objective functon
f_objective <- function(index, catch, tau_cs, tau_s, M, ln_pars, weights){
  
  # compute predictions
  pred_index = f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars)
  ## sqq for each year
  ((log(pred_index) - log(as.matrix(index[,-1])))^2 * weights) %>%
    rowSums(.) %>%
    as.numeric(.) -> ssq
  ## objective function 
  obj_fxn = sum(ssq)
  
  return(obj_fxn)
}
## optim wrapper for bootstrapping (index needs to be first arg)
f_optim <- function(index, catch, tau_cs, tau_s, M, ln_pars, weights){
  optim(par = ln_pars, fn = f_objective,
        index = index, catch = catch, tau_cs = tau_cs,
        tau_s = tau_s, M = M, weights = weights,
        method="BFGS", 
        hessian=FALSE,
        control=list(trace=TRUE, maxit=1e4))
}
## bootstrap confidence intervals
f_csa_boot <- function(csa_fit, index, catch, tau_cs, tau_s, M, ln_pars, weights, boot_iterations, alpha){
  
  # do bootstrap resampling and fit model ----
  
  # compute estimated cpue index
  index_est = f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars = fit2$par)
  
  # compute sd of log residuals
  log_sd = (log(as.matrix(index[,-1])) - log(index_est)) %>%
    apply(., 2, sd) %>%
    as.numeric
  
  # simulate bootstrap replicate cpue index samples
  index_sim = list(NULL)
  idx_mat = matrix(nrow = nrow(index), ncol = ncol(index))
  for(i in 1:boot_iterations){
    for(j in 2:ncol(index)){
      idx_mat[,1] = index[,1]
      idx_mat[,j] = exp(log(index[,j]) + rnorm(n = nrow(index), mean = 0, sd = log_sd[j-1]) + ((log_sd[j-1]^2)/2))
      idx_mat = as.data.frame(idx_mat) %>% rename_all(~names(index))
    }
    index_sim[[i]] = idx_mat
  }
  
  # organize in tibble
  tibble(iteration = 1:boot_iterations,
         index_sim = index_sim) %>%
    # fit models
    mutate(sim_fit = purrr::map(index_sim, .f= f_optim, catch = catch, 
                                tau_cs = tau_cs, tau_s = tau_s, M  = M, 
                                ln_pars = ln_pars, weights = weights)) -> boot_mods
  
  # compute quantities of interest ----
  
  ## extract parameters estimates confidence interval
  boot_mods %>%
    mutate(boot_est = purrr::map(sim_fit, function(x){x$par})) %>%
    dplyr::select(iteration, boot_est) %>%
    unnest(boot_est) %>%
    mutate(position = rep((1:length(ln_pars)), boot_iterations)) %>%
    group_by(position) %>%
    summarise(lwr = quantile(boot_est, 0 + alpha/2),
              upp = quantile(boot_est, 1 - alpha/2),
              .groups = "drop") %>%
    dplyr::select(-position) %>%
    exp %>%
    round(., 8) %>%
    as.matrix() -> par_ci
  
  ## confidence interval on cpue
  boot_mods %>%
    mutate(boot_est = purrr::map(sim_fit, function(x){x$par}),
           boot_q = purrr::map_dbl(boot_est, function(x){exp(x[2])}),
           boot_cpue = purrr::map(boot_est, function(ln_pars){as.data.frame(f_pred_index(index, catch, tau_cs,
                                                                                         tau_s, M, ln_pars))})) %>%
    dplyr::select(iteration, boot_q, boot_cpue) %>%
    unnest(boot_cpue) %>%
    rename_all(~c("iteration", "q", paste0("stage", 1:ncol(index[,-1])))) %>%
    # scale to abundance
    mutate_at(3:ncol(.), function(x){x / .$q}) %>%
    # add year to boot estimates
    mutate(year = rep(index$yr, boot_iterations)) %>%
    dplyr::select(iteration, year, grep("stage", names(.))) %>%
    # get quantiles
    pivot_longer(3:ncol(.), names_to = "stage", values_to = "abundance") %>%
    as.data.frame() %>%
    group_by(year, stage) %>%
    summarise(lwr = quantile(abundance, 0 + alpha/2), 
              upp = quantile(abundance, 1 - alpha/2),
              .groups = "drop") -> abund_ci
  
  # matrix of abundance lwr
  abund_ci %>%
    dplyr::select(year, stage, lwr) %>%
    pivot_wider(names_from = stage, values_from = lwr) %>%
    arrange(year) %>%
    ungroup() %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    round(., 2) -> abund_lwr
  
  # matrix of abundance upp
  abund_ci %>%
    dplyr::select(year, stage, upp) %>%
    pivot_wider(names_from = stage, values_from = upp) %>%
    arrange(year) %>%
    ungroup() %>%
    dplyr::select(-year) %>%
    as.matrix() %>%
    round(., 3) -> abund_upp
  
  # output ----
  
  boot_out = list(boot_iterations = boot_iterations,
                  alpha = alpha,
                  par_ci = par_ci,
                  abund_lwr = abund_lwr,
                  abund_upp = abund_upp)
  
  return(boot_out)
  
}

# load and prep data ----
dat_list = f_read_csa_dat(file_path = file_path)

## compute cs tau and s tau
tau_cs = f_tau_cs(dat_list)
tau_s = f_tau_s(dat_list)

## dat_list to named object
n_stages = dat_list$n_stages
index = dat_list$index
catch = dat_list$catch
M = dat_list$M
weights = dat_list$weights
boot_iterations = dat_list$boot_iterations 
alpha = dat_list$alpha

# define parameter starting values ----

## pre-recruit M
preM = dat_list$preM_start
## catchability
q = dat_list$q_start
## inital survey index for each stage except the first
idx_init = rep(median(index[,2]), ncol(index) - 2) * 2
## recruitment
rec = rep(median(index[,2]), nrow(index)) * 2
#rec = rep(20, 22)

## coerce to list
ln_pars = c(log(preM), log(q), log(idx_init), log(rec))

# minimize objective function ----

# fit once
fit <- optim(par = ln_pars, fn = f_objective,
             index = index, catch = catch, tau_cs = tau_cs,
             tau_s = tau_s, M = M, weights = weights,
             method="BFGS", 
             hessian=FALSE,
             control=list(trace=TRUE, maxit=1e4))
# refit with fitted par as starting values to see if you do any better
fit2 <- optim(par = fit$par, fn = f_objective,
              index = index, catch = catch, tau_cs = tau_cs,
              tau_s = tau_s, M = M, weights = weights,
              method="BFGS",
              hessian=TRUE,
              control=list(trace=TRUE, maxit=1e4))

# bootstrap ----

if(boot_iterations > 0){
# compute bootstrap CI
boot = f_csa_boot(csa_fit = fit2, index, catch, tau_cs, tau_s, M, ln_pars, weights, boot_iterations, alpha)
} 

# build par file ----
par_file = matrix(nrow = 1000, ncol = 100)

# convergence information from optim
par_file[1, 1] = paste0("# Number of parameters = ", length(fit2$par))
par_file[2, 1] = paste0("# convergence code = ", fit2$convergence)
par_file[3, 1] = paste0("# message = ", fit2$message)

## objective function
par_file[4, 1] = paste0("# objective function = ", fit2$value)

## extract parmeter estimates
par_file[6, 1] = "# parameter estimates"
par_file[7, 1] = "# pre-recruitM"
par_file[8, 1] = round(exp(fit2$par[1]), 3)
par_file[9, 1] = "# q"
par_file[10, 1] = round(exp(fit2$par[2]), 8)
par_file[11, 1] = "# initial cpue for non pre-recruit stages"
par_file[12, 1:(ncol(index)-2)] = round(exp(fit2$par[3:(3+ncol(index)-3)]), 3)
par_file[13, 1] = "# annual pre-recruit cpue"
par_file[14, 1:nrow(index)] = round(exp(fit2$par[(3+ncol(index)-2):length(fit2$par)]), 3)

## predicted survey cpue index
par_file[16, 1] = "# estimated survey cpue by stage"
end_row = (17+nrow(index)-1)
par_file[17:end_row, 1:(ncol(index)-1)] =  round(f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars = fit2$par), 3)

## abundance estimates
start_row = end_row+2
end_row = (start_row+nrow(index))
par_file[start_row, 1] = "# abundance estimates at time of survey"
par_file[(start_row+1):end_row, 1:ncol(index[-1])] = round(f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars = fit2$par) / exp(fit2$par[2]), 3) 

if(boot_iterations > 0){
## parameter CI
start_row = end_row+2
end_row = start_row+length(fit2$par)
par_file[start_row, 1] = "# parameter bootstrap confidence interval"
par_file[(start_row+1):end_row, 1:2] = boot$par_ci

## abundance lwr CI
start_row = end_row+2
end_row = (start_row+nrow(index))
par_file[start_row, 1] = "# abundance confidence interval lower bound"
par_file[(start_row+1):end_row, 1:ncol(boot$abund_lwr)] = boot$abund_lwr

## abundance upp CI
start_row = end_row+2
end_row = (start_row+nrow(index))
par_file[start_row, 1] = "# abundance confidence interval upper bound"
par_file[(start_row+1):end_row, 1:ncol(boot$abund_upp)] = boot$abund_upp
}

## print hessian matrix
start_row = end_row+2
end_row = start_row+length(fit2$par)
par_file[start_row, 1] = "# hessian"
par_file[(start_row+1):end_row,  1:length(fit2$par)] = fit2$hessian

# write file
write.table(par_file[1:end_row,], file = gsub(".dat", ".par", file_path), na = "", row.names = F, col.names = F, quote = F)

# return ----

out = list(
  # input data
  input = dat_list,
  # parameter estimates
  par = list(preM = exp(fit2$par[1]),
             q = exp(fit2$par[2]),
             init_cpue = exp(fit2$par[3:(3+ncol(index)-3)]),
             pre_cpue = exp(fit2$par[(3+ncol(index)-2):length(fit2$par)])),
  # predicted cpue
  est_cpue = as.data.frame(round(f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars = fit2$par), 3)) %>%
                  rename_all(~paste0("stage", 1:n_stages)) %>%
                  bind_cols(dplyr::select(index, yr), .) %>%
                  rename(year = yr),
  # predicted abundance
  est_abundance = as.data.frame(round(f_pred_index(index, catch, tau_cs, tau_s, M, ln_pars = fit2$par) / exp(fit2$par[2]), 3)) %>%
                  rename_all(~paste0("stage", 1:n_stages)) %>%
                  bind_cols(dplyr::select(index, yr), .) %>%
                  rename(year = yr),
  # bootstrap info
  boot = if(boot_iterations > 0){boot} else{NULL},
  # hessian matrix
  hessian = fit2$hessian
)
                      
return(out)

}


f_csa(file_path = "./models/2020/glacier_bay_csa.dat")

