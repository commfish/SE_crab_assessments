# notes ----
## SE tanner crab csa model runs 2023
## tyler jackson
## 11/10/2022

# load ----

source("./code/csa_tmb/csa_wrapper_funs.R")

## compile and load model
compile("./code/csa_tmb/CSA_TANNER.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("./code/csa_tmb/CSA_TANNER"))


# lynn sisters ----

## load data
input <- f_load_dat("./data/csa_tanner/lynn_sisters.dat")
## load parameters
pin <- f_load_pin("./data/csa_tanner/lynn_sisters.pin", 3, 26)
## fit model
fit <- f_fit_csa(input, pin, map_set = 1, prefix = "lynn_sisters")
fit$objective
summary(fit$sdreport)


# glacier bay ----

## load data
input <- f_load_dat("./data/csa_tanner/glacier_bay.dat")
## load parameters
pin <- f_load_pin("./data/csa_tanner/glacier_bay.pin", 3, 24)
## fit model
fit <- f_fit_csa(input, pin, map_set = 1)
fit$objective
summary(fit$sdreport)

## plots
f_csa_plots(fit, path = "./figures/csa_tanner", prefix = "glacier_bay")

capture.output(fit$sdreport, file = "./output/csa_tanner/glacier_bay.par")
f_rep_file(fit, path = "./output/csa_tanner", prefix = "glacier_bay")


# icy strait ----

## load data
input <- f_load_dat("./data/csa_tanner/icy_strait.dat")
## load parameters
pin <- f_load_pin("./data/csa_tanner/icy_strait.pin", 3, 26)
## fit model
fit <- f_fit_csa(input, pin, map_set = 1)
fit$objective
summary(fit$sdreport)

## plots
f_csa_plots(fit, path = "./figures/csa_tanner", prefix = "icy_strait")

capture.output(fit$sdreport, file = "./output/csa_tanner/icy_strait.par")
f_rep_file(fit, path = "./output/csa_tanner", prefix = "icy_strait")
