# notes ----

## Test SE Tanner crab CSA model using TMB
## Tyler Jackson
## 6/21/2021

# load ----

## libraries and functions
source("./code/SE_csa_tmb_Rfunctions.R")

## compile and load model
compile("./code/tanner_csa_tmb.cpp")
dyn.load(dynlib("./code/tanner_csa_tmb"))
  
# load data ----

## glacier bay csv
read_csv("./data/glacier_bay_tanner_2020.csv") -> glacier_bay

# prep input data ----

## TMB requires data input as list
csa_data <- list(
  ## survey years
  survey_yrs = 1999:2020,
  
  ## number of stages
  nstage = 3,
  
  ## retain catch in numbers
  ret_cat_num = glacier_bay %>%
                    filter(survey_year %in% 1999:2020) %>%
                    pull(catch_num) %>% .[!is.na(.)],
  
  ## survey cpue index (numbers)
  index = glacier_bay %>%
              filter(survey_year %in% 1999:2020) %>%
              dplyr::select(pre_recruit, recruit, post_recruit) %>%
              as.matrix(),
  
  # tau cs and s
  tau_cs = glacier_bay %>%
               mutate(tau_cs = f_tau_cs(catch_mid_date, survey_mid_date)) %>%
               filter(survey_year %in% 1999:2020) %>%
               pull(tau_cs),
  tau_s = glacier_bay %>%
              mutate(tau_s = f_tau_s(survey_mid_date)) %>%
              filter(survey_year %in% 1999:2020) %>%
              pull(tau_s) %>% .[!is.na(.)],
  
  # natural mortality on recruits
  M = 0.3,
  # pre_recruit molt probability
  molt = rep(1, nrow(glacier_bay)-1),
  # data weighting on annual survey index
  wt_survey = rep(10, 3)
)


# run model ----

## parameter starting values
par_start <- list(ln_index_init = rep(5, 3), 
                  ln_rec = rep(5, length(1999:2020)-1),
                  ln_preM = 0.3,
                  ln_q = -8)

## builds and fit model
obj <- MakeADFun(csa_data, par_start, DLL = "tanner_csa_tmb")
opt <- nlminb(start = obj$par, obj = obj$fn, gr = obj$gr)


## parameter estimates with se
par_est <- summary.sdreport(sdreport(obj))

## extract report
rep <- obj$report()

# plots and tables ----

### fits to survey cpue
f_plot_cpue_fit(glacier_bay, rep, "pre_recruit")
f_plot_cpue_fit(glacier_bay, rep, "recruit")
f_plot_cpue_fit(glacier_bay, rep, "post_recruit")

### abundance and biomass of legal males and mature males
f_MMB(glacier_bay, par_est, rep, name_path = c("./try1.png", "./try2.png"))


