## gmacs R functions
## updated to GMACS 2.01.M.10; Compiled 2024-02-27
## tyler jackson, caitlin stern
## last update - 4/18/2024

# load ----

if(!require(tidyverse, quietly = T)) {install.packages("tidyverse", dependencies = T); library(tidyverse)}
if(!require(ggpmisc, quietly = T)) {install.packages("ggpmisc", dependencies = T); library(ggpmisc)}
if(!require(janitor, quietly = T)) {install.packages("janitor", dependencies = T); library(janitor)}
if(!require(patchwork, quietly = T)) {install.packages("patchwork", dependencies = T); library(patchwork)}
if(!require(latex2exp, quietly = T)) {install.packages("latex2exp", dependencies = T); library(latex2exp)}

# plot options -----

# graphic options
theme_sleek <- function(base_size = 12) {
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      #axis.text = element_text(colour = "grey30"),
      #axis.title = element_text(colour = "grey30"),
      #legend.title = element_text(colour = "grey30"),#, size = rel(0.9)
      panel.border = element_rect(fill = NA),#, colour = "grey70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      #legend.text = element_text(size = rel(0.7)),#, colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA)#,
      #plot.title = element_text(colour = "grey30"),#, size = rel(1)
      #plot.subtitle = element_text(colour = "grey30")#, size = rel(.85)
    )
  
}

# Depends on dplyr
tickr <- function(
    data, # dataframe
    var, # column of interest
    to # break point definition
) {
  
  VAR <- enquo(var) # makes VAR a dynamic variable
  
  data %>%
    dplyr::filter(!is.na(!!VAR)) %>%
    distinct(!!VAR) %>%
    mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
                           !!VAR, "")) %>%
    dplyr::select(breaks = UQ(VAR), labels)
}
theme_set(theme_sleek())
yraxis <- tickr(tibble(yr = 1900:2100), yr, 5)

# custom color scale
cbpalette <- colorRampPalette(colors = c("#009E73", "#0072B2", "#E69F00", "#56B4E9", "#D55E00", "#CC79A7","#F0E442", "black", "grey"))(9)


# gmacs_read_allout() ----

## read GMACSall.out file

## args:
### file - file path to Gmacsall.out
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

## output: list object
## example: gmacs_read_allout(file = "./AIGKC/models/2024/may/EAG/23.1b/Gmacsall.out", model_name = "23.1b")

gmacs_read_allout <- function(file, model_name = NULL) {
  
  # setup ----
  # Suppress the NA message in the coercion to double
  options(warn = -1) 
  
  # read text file
  allout <- read.delim(file, sep = "", header = F, col.names = c(1:1000), fill = T, na.strings = "", colClasses = "character")
  # create out object
  out <- list()
  
  # version ----
  out$version <- str_flatten(allout[1,], collapse = " ", na.rm = T)
  # model name ----
  out$model_name <- model_name
  # stock ----
  out$stock <- gsub("#Stock being assessed: ", "", str_flatten(allout[2,], collapse = " ", na.rm = T))
  # general info ----
  ## years
  out$yr_range <- as.numeric(gsub(";", "", allout[grep("Year_range", allout[,1]), 2:3]))
  out$mod_yrs <- out$yr_range[1]:out$yr_range[2]
  last <- grep("Year_range", allout[,1]) # start saving last position in file
  ## number of seasons
  out$n_season <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## number of fleets
  out$n_fleets <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## fleet names
  out$fleet_names <- as.character(allout[last + 1, 2:(out$n_fleets + 1)]); last <- last + 1
  ## n sexes
  out$n_sex <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1
  ## n shell conidition
  out$n_shell <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
  ## n maturity states
  out$n_maturity <- as.numeric(str_extract(str_flatten(allout[last + 1,], na.rm = T), "([[:digit:]]+)")); last <- last + 1 
  ## units 
  out$wt_units <-  gsub("Weightunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1 
  out$n_units <- gsub("Numbersunitis:", "", str_flatten(allout[last + 1,], na.rm = T)); last <- last + 1
  case_when(out$n_units %in% c(1, "1", "1s", "one", "One", "ones", "Ones", "Numbers") ~ "1s",
            out$n_units %in% c(10, "10", "10s", "ten", "Ten", "Tens") ~ "10s",
            out$n_units %in% c(100, "100", "100s", "hundred", "Hundred", "Hundreds") ~ "100s",
            out$n_units %in% c(1000, "10-0", "1000s", "thousand", "Thousand", "Thousands", "thou", "Thou") ~ "1000s", 
            out$n_units %in% c(1000000, "10-0", "1000000s", "millions", "Million", "Millions", "mill", "Mill") ~ "1000000s") -> out$n_units

  # likelihoods by type ----
  
  # read lines individually
  catch = as.numeric(na.omit(as.numeric(allout[last + 2,]))); last <- last + 2 
  index = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
  size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1 
  recruitment = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  tagging =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  penalties =  as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  priors = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  initial_size = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  total = as.numeric(na.omit(as.numeric(allout[last + 1,]))); last <- last + 1
  
  # coerce to tibble
  rbind(catch, index, size, recruitment, tagging) %>% as_tibble() %>%
    transmute(process = c("catch", "index", "size", "recruitment", "tagging"), raw_lik = V1, net_lik = V2) %>%
    add_row(process = "penalites", raw_lik = penalties, net_lik = penalties) %>%
    add_row(process = "priors", raw_lik = priors, net_lik = priors) %>%
    add_row(process = "initial_size", raw_lik = initial_size, net_lik = initial_size) %>%
    add_row(process = "total", raw_lik = sum(.$raw_lik), net_lik = total) -> out$likelihoods_by_type
  
  # likelihoods by type and fleet ---- 
  ## catches
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 4,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 5,])))) %>%
    transmute(process = paste0("catch_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> catch; last <- last + 5
  ## index
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("index_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> index; last <- last + 4    
  ## size composition
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("size_comp_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> size; last <- last + 4    
  ## recruitment penalties
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("rec_pen_", 1:nrow(.)), raw_lik, net_lik) -> rec_pen; last <- last + 2
  ## tagging
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         emphasis = as.numeric(na.omit(as.numeric(allout[last + 3,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 4,])))) %>%
    transmute(process = paste0("tagging_", 1:nrow(.)), raw_lik, emphasis, net_lik) -> tagging; last <- last + 4   
  ## growth 
  tibble(raw_lik = as.numeric(na.omit(as.numeric(allout[last + 2,]))),
         net_lik = as.numeric(na.omit(as.numeric(allout[last + 2,])))) %>%
    transmute(process = paste0("growth_", 1:nrow(.)), raw_lik, net_lik) -> growth; last <- last + 2
  bind_rows(catch, index, size, rec_pen, tagging, growth) -> out$likelihoods_by_type_and_fleet
  
  
  
  # penalties ----
  
  tmp <- matrix(nrow = 12, ncol = 3)
  for(i in 1:12) {tmp[i, 1:3] <- as.numeric(na.omit(as.numeric(allout[last + 1 + i,])[-1]))}
  as_tibble(tmp) %>%
    mutate(penalty = c("Mean_Fbar = 0", "Mean_Fdev", "Mdevs", "Rec_ini", "Rec_dev", "Sex_ratio",
                       "Molt_prob", "Smooth_select", "Init_numbers", "Fdevs_(flt)", "Fdovs_(flt)",
                       "Seldevs")) %>%
    transmute(penalty, raw_lik = V1, emphasis = V2, net_lik = V3) -> tmp

  out$penalties <- tmp
  last <- last + 14
  
  # parameters ----
  
  ## par tibble
  tmp <- matrix(ncol = 11, nrow = length((last + 2):(grep("#---", allout[,1])[1]-1)))
  for(i in 1:nrow(tmp)) {
    if("*" %in% as.character(allout[last + 1 + i, 1:ncol(tmp)])) {
      as.character(allout[last + 1 + i, 1:13]) %>%
        .[!is.na(.)] %>% .[. != "*"] -> tmp[i,]
    } else{as.character(allout[last + 1 + i, 1:ncol(tmp)]) -> tmp[i,]}
  }
  as_tibble(tmp) %>%
    rename_all(~c("parameter_count", "parameter", "colon", "estimate", "phase", "lower_bound", "upper_bound", 
                  "penalty", "gradient", "standard_error", "est_count")) %>%
    #janitor::clean_names() %>%
    mutate_at(c(1, 4:11), as.numeric) %>% 
    dplyr::select(-colon) -> out$parameters; last <- grep("#---", allout[,1])[1]
  out$n_par <- out$parameters %>% filter(phase > 0) %>% nrow
  ## parameters at bounds
  out$parameters %>%
    mutate(range = upper_bound - lower_bound,
           status = ifelse(estimate < (lower_bound+range*0.01), 1,
                           ifelse(estimate > upper_bound-range*0.01, 1, 0))) %>%
    filter(status == 1) %>% dplyr::select(-range, -status) -> out$parameters_at_bounds
  
  # max gradient ----
  out$max_gradient <- max(abs(out$parameters$gradient), na.rm = T)
  # reference points ----
  
  ## ref tibble
  tmp <- matrix(ncol = 3, nrow = length((last + 2):(grep("#---", allout[,1])[2] - 2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- na.omit(as.numeric(allout[last + 1 + i,]))
  }
  as_tibble(tmp) %>%
    mutate(ref = c("Male_spr_rbar", "Female_spr_rbar", "SSSB/R_F=0", "BMSY", "Bcurr/BMSY", "OFL_tot",
                   paste0("Fmsy_", 1:out$n_fleets), paste0("Fofl_", 1:out$n_fleets), 
                   paste0("OFL_", 1:out$n_fleets))) %>%
    transmute(parameter_name = ref, estimate = V1, se = V2, est_quantity_count = V3) -> out$reference_points
  out$mmb_curr <- prod(out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY", "Bcurr/BMSY")])
  out$ofl_tot <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("OFL_tot")]
  out$bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("BMSY")]
  out$b_bmsy <- out$reference_points$estimate[out$reference_points$parameter_name %in% c("Bcurr/BMSY")]
  out$f_msy_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fmsy_", 1:out$n_fleets)])
  out$f_ofl_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% paste0("Fofl_", 1:out$n_fleets)])
  out$rbar_tot <- sum(out$reference_points$estimate[out$reference_points$parameter_name %in% c("Male_spr_rbar", "Female_spr_rbar")])
  last <- grep("#---", allout[,1])[2] - 1
  ## ref sigma
  allout[last,] %>% dplyr::select_if(~ !any(is.na(.))) %>%
    mutate_all(., function(x){gsub(";", "", x)}) %>%
    .[1,] %>% as.numeric() %>%na.omit() %>% as.numeric() -> out$ref_sigmaR
  names(out$ref_sigmaR) <- c("sigmaR", "weight")
  last <- grep("#---", allout[,1])[2]
  
  # overall summary ----
  
  tmp <- matrix(ncol = 15 + (3*out$n_sex) + out$n_fleets, nrow = length(out$mod_yrs))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+2+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~make.unique(as.character(allout[last+2,1:ncol(tmp)]))) %>%
    janitor::clean_names(.) %>% rename(year = number_year, log_recruits_male = log_recruits, sd_log_recruits_male = sd_log_recruits) -> out$derived_quant_summary
  # do some renaming
  
  if(out$n_sex == 2) {
    out$derived_quant_summary %>%
      rename(log_recruits_female = log_recruits_1, sd_log_recruits_female = sd_log_recruits_1) -> out$derived_quant_summary
  }
  last <- grep("#---", allout[,1])[3]
  
  # mean wt ----
  
  ## add size bins
  out$size_bins <- as.numeric(na.omit(as.numeric(allout[last+2,])))
  ## number of bins
  out$n_size_bins <- length(out$size_bins)
  ## weight at size matrix
  tmp <- matrix(nrow = out$n_sex*out$n_maturity*length(out$mod_yrs), ncol = length(out$size_bins)+3)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    mutate_at(2:ncol(.), as.numeric) %>%
    rename_all(~c("sex", "maturity", "year", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "wt") %>%
    mutate(size = as.numeric(size)) -> out$wt_at_size
  last <- last + 3 + nrow(tmp)
  
  # maturity vector ----
  
  out$maturity_at_size_vector <- as.numeric(allout[last+1,1:length(out$size_bins)]); last <- grep("#---", allout[,1])[4]
  
  # catch fit summary ----
  
  ## catch summary
  tmp <- matrix(nrow = length((last+3):(grep("#---", allout[,1])[5]-3)), ncol = 14)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:14])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 6:7, 10:14), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "obs_catch", "cv", "type", "units", "mult", "effort", "disc_m", 
                  "pred_catch", "residual")) -> out$catch_fit_summary; last <- grep("#---", allout[,1])[5]-3
  ## catch q
  tibble(series = unique(out$catch_fit_summary$series),
         log_q = as.numeric(allout[last+2,1:length(unique(out$catch_fit_summary$series))])) -> out$log_q_catch; last <- grep("#---", allout[,1])[5]
  
  # index fix_summary ----
  
  ## index summary
  tmp <- matrix(nrow = length((last+3):(grep("sdnr_MAR_cpue", allout[,1])-2)), ncol = 13)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+2+i,1:13])
  }
  as_tibble(tmp) %>%
    mutate_at(c(1:2, 4, 7:9, 11:13), as.numeric) %>%
    rename_all(~c("series", "year", "fleet", "season", "sex", "maturity", "obs_index", "obs_cv", 
                  "tot_cv", "units", "q", "timing", "pred_index")) -> out$index_fit_summary; last <- last + nrow(tmp) + 2
  ## sdnr_MAR_cpue
  tmp <- matrix(nrow = length(unique(out$index_fit_summary$series)), ncol = 2)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+3, 1:2])
  }
  out$sdnr_MAR_cpue <- tmp; last <- grep("#---", allout[,1])[6]
  
  # size composition fit summary ----
  
  ## size composition fit summary
  ## get info first
  tmp <- matrix(ncol = 10, nrow = length((last+3):(grep("sdnr_MAR_lf", allout[,1])-2)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("org_series", "mod_series", "year", "fleet", "season", "sex", "type", "shell", "maturity", "nsamp_obs")) %>%
    mutate_at(c(1:3, 5, 10), as.numeric) -> tmp
  
  ## get comps
  last <- last + 2 # set last to start where the data is
  tmp %>%
    nest_by(mod_series, .keep = T) %>% ungroup() %>% 
    mutate(row = purrr::map_dbl(data, ~nrow(.)),
           row = lag(row),
           row = cumsum(ifelse(is.na(row), 0, row)) + last) %>% 
    mutate(comps = purrr::map2(data, row, function(data, row) {
      
      comp_tmp <- matrix(ncol = ncol(allout)-ncol(tmp), nrow = nrow(data))
      for(i in 1:nrow(comp_tmp)) {
        comp_tmp[i,] <- as.numeric(allout[row + i, 11:ncol(allout)])
      }
      as_tibble(comp_tmp) %>%
        dplyr::select(where(function(x)!all(is.na(x)))) -> comp_tmp
      if((ncol(comp_tmp)/out$n_size_bins) <= 2) {comp_agg <- F} else{comp_agg <- T}
      if(comp_agg == F){
        
        comp_tmp %>% 
          rename_all(~c(paste0("obs_", out$size_bins[1:(ncol(comp_tmp)/2)]), paste0("pred_", out$size_bins[1:(ncol(comp_tmp)/2)]))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit2) %>%
          transmute(org_series, mod_series, year, fleet, season, sex, type, shell, maturity, size = as.numeric(size), 
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      if(comp_agg == T){
        nobs <- ncol(comp_tmp)/2
        group <- rep(1:50, each = out$n_size_bins)[1:nobs] # << probably a more elegant way to do this...
        comp_tmp %>% 
          rename_all(~c(paste0("obs_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))), paste0("pred_", group, "_", as.numeric(matrix(out$size_bins, ncol = nobs))))) %>%
          bind_cols(data, .) %>%
          pivot_longer((ncol(data)+1):ncol(.), names_to = "group", values_to = "prop") %>% 
          separate_wider_delim(group, "_", names_sep = "split") %>%
          pivot_wider(names_from = groupsplit1, values_from = prop) %>%
          rename(size = groupsplit3, aggregate_series = groupsplit2) %>%
          transmute(org_series, mod_series, aggregate_series = as.numeric(aggregate_series), year, fleet, season, sex, type, shell, maturity, size = as.numeric(size),
                    nsamp_obs, obs = as.numeric(obs), pred = as.numeric(pred), residual = obs - pred) -> comp_out
      }
      
      return(comp_out)
      
    })) %>% transmute(comps) %>% unnest -> out$size_fit_summary
  
  last <- grep("sdnr_MAR_lf", allout[,1])
  ## sdnr_MAR_lf
  tmp <- matrix(ncol = 2, nrow = length(unique(out$size_fit_summary$mod_series)))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.numeric(allout[last+i, 1:2])
  }
  out$sdnr_MAR_lf <- tmp
  last <- grep("Francis_weights", allout[,1])
  ## francis weights
  out$francis_weights <- as.numeric(allout[last+1, 1:length(unique(out$size_fit_summary$mod_series))]); last <- grep("#---", allout[,1])[7]
  
  ## add stage two weights to fit summary
  out$size_fit_summary %>% 
    mutate(lambda = out$francis_weights[.$mod_series],
           nsamp_est = exp(out$parameters$estimate[grepl("Log_vn_comp", out$parameters$parameter)][.$mod_series]) * nsamp_obs * lambda) -> out$size_fit_summary
  
  # selectivity ----
  
  ## selex
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_capture", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_cap; last <- last + 3 + nrow(tmp)
  ## retention
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_retention", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_ret; last <- last + 2 + nrow(tmp)
  ## discard
  tmp <- matrix(ncol = 3 + length(out$size_bins), nrow = length(out$mod_yrs) * out$n_sex * out$n_fleets)
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "fleet", out$size_bins)) %>%
    pivot_longer(4:ncol(.), values_to = "slx_discard", names_to = "size") %>%
    mutate_at(c(1, 4, 5), as.numeric) -> slx_disc; last <- grep("Select_control", allout[,2])
  ## slx control
  as_tibble(allout[(last+2):(grep("#----", allout[,1])[8]-1), 1:11]) %>%
    mutate_all(as.numeric) %>%
    rename_all(~c("gear", "par", "phase", "start_yr", "end_yr", "env_link",
                  "link_par", "rand_walk", "re_start_yr", "re_end_yr", "re_sigma")) %>%
    # add sex wherever there is two selectivity functions with the same gear and block - Andre should change the output back to what I had at some point
    group_by(gear, start_yr, end_yr) %>% 
    mutate(sex = c("male", "female")[row_number()]) %>% ungroup() %>%
    transmute(gear, sex, par, phase, start_yr, end_yr, env_link, link_par, rand_walk, re_start_yr, re_end_yr, re_sigma) -> out$slx_control
  
  out$slx_control %>%
    mutate(fleet = out$fleet_names[abs(as.numeric(.$gear))],
           type = ifelse(gear > 0, "capture", "retention")) %>%
    distinct(fleet, type, sex, start_yr, end_yr) %>%
    mutate(start_yr = ifelse(start_yr == 0, out$yr_range[1], start_yr),
           end_yr = ifelse(start_yr == 0, out$yr_range[2], end_yr),
           year = purrr::map2(start_yr, end_yr, function(start_yr, end_yr) {start_yr:end_yr})) %>%
    unnest(year) %>%
    mutate(block = paste(start_yr, "-", end_yr)) %>%
    dplyr::select(-start_yr, -end_yr) -> tmp
  slx_cap %>%
    left_join(tmp %>%
                filter(type == "capture") %>%
                transmute(year, fleet, sex, capture_block = block),
              by = join_by(year, sex, fleet)) %>%
    left_join(slx_ret %>%
                left_join(tmp %>%
                            filter(type == "retention") %>%
                            transmute(year, fleet, sex, ret_disc_block = block),
                          by = join_by(year, sex, fleet)),
              by = join_by(year, sex, fleet, size)) %>%
    left_join(slx_disc, by = join_by(year, sex, fleet, size)) %>%
    transmute(year, sex, fleet, size, slx_capture, capture_block, slx_retention,
              slx_discard, ret_disc_block) -> out$selectivity
  last <- grep("#----", allout[,1])[8]
  
  # mortality ----
  
  ## M by season
  tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last + 3 + i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", paste0("season_", 1:out$n_season))) -> out$proportion_M_by_season; last <- last + nrow(tmp) + 4
  
  ## M by sex-maturity-size class
  tmp <- matrix(nrow = length(out$mod_yrs) * out$n_sex * out$n_maturity, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("year", "sex", "maturity", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "M") %>%
    mutate_at(c(1, 3:5), as.numeric) -> out$M_by_class; last <- last + nrow(tmp) + 2
  
  ## fully selected F by season, sex, fleet
  tmp <- matrix(nrow = out$n_sex * out$n_fleets * length(out$mod_yrs), ncol = 3 + out$n_season)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "fleet", "year", 1:out$n_season)) %>%
    mutate_at(c(3:ncol(.)), as.numeric) %>%
    pivot_longer(4:ncol(.), names_to = "season", values_to = "F") -> out$F_by_sex_fleet_year_season;last <- last + nrow(tmp) + 2
  
  ## fully selected F by season, sex, fleet
  ## skip same as above
  last <- last + nrow(out$F_by_sex_fleet_year_season)/out$n_fleets + 2
  
  ## F by sex, year, season, size
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs) * out$n_season, ncol = 3 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", "season", out$size_bins)) %>%
    pivot_longer(4:ncol(.), names_to = "size", values_to = "F_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- last + nrow(tmp) + 2
  out$F_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, year, season, size)); 
  
  ## Z by sex, year, maturity, season, size
  tmp <- matrix(nrow = out$n_sex * out$n_maturity * length(out$mod_yrs) * out$n_season, ncol = 4 + length(out$size_bins))
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_continuos") %>%
    mutate_at(2:ncol(.), as.numeric) -> cont; last <- last + nrow(tmp) + 2 
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.character(allout[last+1+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "maturity", "year", "season", out$size_bins)) %>%
    pivot_longer(5:ncol(.), names_to = "size", values_to = "Z_discrete") %>%
    mutate_at(2:ncol(.), as.numeric) -> disc; last <- grep("#---", allout[,1])[9]
  out$Z_by_sex_maturity_year_season_size <- left_join(cont, disc, by = join_by(sex, maturity, year, season, size))
  
  # n matrix ----
  
  nmats <- grep("#N(.)", apply(allout[(grep("#---", allout[,1])[9]+2):grep("#---", allout[,1])[10],], 1, str_flatten, na.rm = T), value = T)
  nmats_index <- as.numeric(names(nmats))
  
  list_tmp <- list()
  for(m in 1:length(nmats)) {
    tmp <- matrix(nrow = length(out$mod_yrs), ncol = 1 + length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[nmats_index[m]+1+i, 1:ncol(tmp)])
    }
    as_tibble(tmp) %>% rename_all(~c("year", out$size_bins)) %>%
      mutate(type = gsub("[()]|#N", "", nmats[m])) -> list_tmp[[m]]
  }
  do.call("bind_rows", list_tmp) %>%
    pivot_longer(2:(ncol(.)-1), names_to = "size", values_to = "n") %>%
    pivot_wider(names_from = type, values_from = n) -> out$n_matrix; last <- grep("#---", allout[,1])[10]
  
  
  # growth ----
  
  ## molt probability
  tmp <- matrix(nrow = out$n_sex * length(out$mod_yrs), ncol = 2 + length(out$size_bins))
  for(i in 1:nrow(tmp)) {
    tmp[i,] <- as.character(allout[last+3+i,1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("sex", "year", out$size_bins)) %>%
    pivot_longer(3:ncol(.), names_to = "size", values_to = "molt_probability") %>%
    mutate_at(2:ncol(.), as.numeric) -> out$molt_probability; last <- last + 3 + nrow(tmp)
  
  ## growth transition matrix
  gmats <- grep("#growth_matrix", allout[,1]); last <- gmats[1]
  gmats_names <- lapply(apply(allout[gmats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(gmats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[gmats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- gmats_names
  out$growth_transition <- list_tmp
  
  ## size transition matrix
  smats <- grep("#size_matrix", allout[,1]); last <- gmats[1]
  smats_names <- lapply(apply(allout[smats,], 1, str_flatten, na.rm = T), function(x){strsplit(x, ":")[[1]][2]}) %>% unlist
  list_tmp <- list()
  for(m in 1:length(smats)) {
    tmp <- matrix(nrow = length(out$size_bins), ncol = length(out$size_bins))
    for(i in 1:nrow(tmp)) {
      tmp[i,] <- as.numeric(allout[smats[m]+i, 1:ncol(tmp)])
    }
    row.names(tmp) <- out$size_bins; colnames(tmp) <- out$size_bins
    list_tmp[[m]] <- tmp
  }
  names(list_tmp) <- smats_names
  out$size_transition <- list_tmp
  last <- grep("#---", allout[,1])[11]
  
  
  
  
  # reference points ----
  
  # combinations of seasons and fleets with Fs
  tmp <- matrix(nrow = out$n_season, ncol = out$n_fleets + 1)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[last+2+i, 1:ncol(tmp)])
  }
  as_tibble(tmp) %>%
    rename_all(~c("season", out$fleet_names)) -> out$seasons_fleets_w_F
  
  ##reference points and ofl
  out$spr_syr <- as.numeric(allout[grep("spr_syr", allout[,1])+1, 1])
  out$spr_nyr <- as.numeric(allout[grep("spr_nyr", allout[,1])+1, 1])
  out$spr_rbar <- as.numeric(allout[grep("spr_rbar", allout[,1])+1, 1:2])
  out$proj_rbar <- as.numeric(allout[grep("proj_rbar", allout[,1])+1, 1:2])
  out$spr_sexr <- as.numeric(allout[grep("spr_sexr", allout[,1])+1, 1])
  out$SR_alpha_prj <- as.numeric(allout[grep("SR_alpha_prj", allout[,1])+1, 1])
  out$SR_beta_prj <- as.numeric(allout[grep("SR_beta_prj", allout[,1])+1, 1])
  out$spr_fofl <- as.numeric(allout[grep("spr_fofl", allout[,1])+1, 1])
  out$spr_cofl_ret <- as.numeric(allout[grep("spr_cofl_ret", allout[,1])+1, 1])
  
  # simple likelihood stuff ----
  max(c(length(unique(out$catch_fit_summary$series)),
        length(unique(out$index_fit_summary$series)),
        length(unique(out$size_fit_summary$series)))) -> cols
  tmp <- matrix(nrow = 5, ncol = cols)
  for(i in 1:nrow(tmp)){
    tmp[i,] <- as.numeric(allout[grep("nloglike", allout[,1])+i, 1:ncol(tmp)])
  }
  out$nloglike <- tmp
  out$nlogPenalty <- as.numeric(na.omit(as.numeric(allout[grep("nlogPenalty", allout[,1])+1,])))
  out$priorDensity <- as.numeric(na.omit(as.numeric(allout[grep("priorDensity", allout[,1])+1,])))
  
  # objective function ----
  out$objective_function <- out$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik)

  # output ----
  return(out)
}

# gmacs_read_rep() ----

## args: file = file path to report file from GMACS, gmacs.rep (from Jie Zheng)

gmacs_read_rep <- function(file) {
  fn <- file
  options(warn = -1) # Suppress the NA message in the coercion to double
  repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE, na.strings = c("nan","-nan"))
  #repfile <- scan(fn, what = "character", flush = TRUE, blank.lines.skip = FALSE, quiet = TRUE)
  inan <- which(is.na(repfile)) # Identify any nan entries so that they are not picked up as objects
  idx <- sapply(as.double(repfile), is.na)
  idx[inan] <- FALSE
  vnam <- repfile[idx] # list names
  nv <- length(vnam) # number of objects
  A <- list()
  ir <- 0
  for (i in 1:nv)
  {
    ir <- match(vnam[i], repfile)
    if (i != nv)
    {
      irr <- match(vnam[i+1], repfile)
    } else {
      irr <- length(repfile) + 1 # next row
    }
    dum <- NA
    if (irr-ir == 2)
    {
      dum <- as.double(scan(fn, skip = ir, nlines = 1, quiet = TRUE, what = ""))
    }
    if (irr-ir > 2)
    {
      # ncols <- 0
      # irows <- ir:irr-1
      # for(j in irows)
      # {
      #       tmp=as.double(scan(fn,skip=j,nlines=1,quiet=TRUE,what=""))
      #       if(length(tmp)>ncols) ncols <- length(tmp)
      #       #print(paste(1:ncols))
      # }
      # cname <- paste(1:ncols)
      # dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=TRUE,col.names=cname))
      # cat("\n ir ",ir," irr ",irr)
      dum <- as.matrix(read.table(fn, skip = ir, nrow = irr-ir-1, fill = TRUE, row.names = NULL))
    }
    if (is.numeric(dum)) # Logical test to ensure dealing with numbers
    {
      A[[vnam[i]]] <- dum
    }
  }
  
  options(warn = 0)
  return(A)
}

# gmacs_read_std() ----

## args: file = file path to gmacs.std file
#        sub_text = parameter name string used for filtering, Default = NULL

gmacs_read_std <- function(file, model_name = NULL, sub_text = NULL) {
  
  std <- read.delim(file, sep = "", skip = 2, header = F) 
  
  std %>%
    as_tibble() %>%
    rename_all(~c("est_no","par", "est", "se")) %>%
    mutate(model_name = model_name) -> out
  
  if(!is.null(sub_text)) {
    out %>% filter(grepl(sub_text, par)) -> out
  }
  
  return(out)
  
}

# gmacs_read_mcoutref() ----

## args: file = file path to mcoutREF.rep file
#        sub_text = parameter name string used for filtering, Default = NULL

gmacs_read_mcoutREF <- function(file, model_name = NULL){
  
  mcout <- read.delim(file, sep = "", skip = 1, header = F)
  ao <- gmacs_read_allout(file.path(dirname(file), "Gmacsall.out"))
  
  tibble(model = model_name, 
         mcout) %>%
    rename_all(~c("model", "draw", "mean_rec", "f", "mmb", "bmsy", "bmsy_b0", "ofl",
                  paste0("fmsy_", ao$fleet_names), paste0("fofl_", ao$fleet_names))) -> out
  
  return(out)
  
}

# gmacs_do_exe() ----

## run gmacs.exe program and tune length composition weights

## args:
### gmacs.dat - file path to gmacs.dat file
### pin - T/F use pin file
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### reweight - T/F tune size composition weights
### level - level of convergence for size comp wts
### max_iter - maximum iterations, convergence shouldn't take forever
### reweight_only - T/F no initial gmacs run

gmacs_do_exe <- function(gmacs.dat, pin = F, wait = T, reweight = T, level = 0.01, max_iter = 100, reweight_only = F) {
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  # check for other needed inputs
  if(!file.exists("gmacs.exe")){stop("Cannot find gmacs.exe!!")}
  dat <- readLines("./gmacs.dat")
  if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
  if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
  if(pin == T){
    dat[grep("pin", dat)] <- "1 # use pin file (0=no, 1=yes)"   
    writeLines(dat, "./gmacs.dat")
    if(!file.exists("gmacs.pin")) {stop(paste("Cannot find gmacs.pin!!"))}
  }
  if(pin == F){
    dat[grep("pin", dat)] <- "0 # use pin file (0=no, 1=yes)"   
    writeLines(dat, "./gmacs.dat")
  }
  if(reweight_only == F){
    # run gmacs.exe
    if(wait == F){shell("gmacs.exe", wait = F, intern = F)}else{shell("gmacs.exe")}
  }
  # do reweighting
  if(reweight == T) {
    
    # check wts convergence first time
    # get lambdas from ctl file
    readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
    # get lambdas from allout file
    readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
      str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
    tibble(ctl_wts, rep_wts) %>%
      mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
      pull(diff) %>% sum -> test
    if(test == 0){setwd(wd); return(paste0("wts convergence reached level = ", level))}
    if(test > 0){
      converged <- F
      # turn off reference point calculation
      dat <- readLines("./gmacs_files_in.dat")
      dat[33] <- "0 # Calculate reference points (0=no)" 
      writeLines(dat, "./gmacs.dat")
      ctl_file <- dat[6] # ctl file path
      # start a counter
      iteration <- 0
      while(iteration < max_iter && converged == F){
        # change ctl wts
        ctl <- readLines("gmacs_in.ctl") 
        ctl[grep("# Lambda for effective sample size", ctl)] <- paste(str_flatten(rep_wts, collapse = " "), "# Lambda for effective sample size")
        writeLines(ctl, ctl_file)
        # run gmacs
        shell("gmacs.exe")
        # test convergence
        readLines("gmacs_in.ctl")[grep("# Lambda for effective sample size", readLines("gmacs_in.ctl"))] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> ctl_wts
        # get lambdas from allout file
        readLines("Gmacsall.out")[grep("Francis_weights", readLines("Gmacsall.out"))+1] %>%
          str_split(" ") %>% unlist %>% as.numeric %>% na.omit -> rep_wts
        tibble(ctl_wts, rep_wts) %>%
          mutate(diff = abs(ctl_wts - rep_wts) >= level) %>%
          pull(diff) %>% sum -> test
        if(sum(test) == 0) {converged = T}
        iteration <- iteration + 1
      }
      # turn on reference point calculation, run gmacs once more
      dat[33] <- "1 # Calculate reference points (0=no)" 
      writeLines(dat, "./gmacs.dat")
      shell("gmacs.exe")
      setwd(wd)
      # done
      if(converged == F) {return(paste0("wts did not reach convergence level = ", level))}
      if(converged == T) {return(paste0("wts convergence reached level = ", level))}
      
    }
    
  } else{setwd(wd); return("done!")}
  
}

# gmacs_do_jitter() ----

# do gmacs jitter runs

### gmacs.dat - file path to gmacs.dat file
### sd - jitter standard deviation
### iter - number of iteration of jittering to run
### ref_points - T/F calculate reference points, add mmb and b35 to jittering results, default = T
### pin - T/F use pin file
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### save_csv - T/F, save csv file output
### csv_dir - file directory in which to save output
### save_plot - T/F, create histograms, default = T
### plot_dir - file directory in which to save plots
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

gmacs_do_jitter <- function(gmacs.dat, sd, iter, ref_points = T, pin = F, wait = T,
                            save_csv = T, csv_dir = NULL, save_plot = T, plot_dir = NULL, model_name = NULL, plot_only = F) {
  
  # create output directories
  if(save_csv == T & is.null(csv_dir)) {csv_dir <- file.path(dirname(gmacs.dat), "output"); dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(csv_dir) && !file.exists(csv_dir)) {dir.create(csv_dir, showWarnings = F, recursive = TRUE)}
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # directory ----
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd
  
  if(plot_only == F){
    
    # set up ----
    
    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    # look for gmacs_file_in.dat - if not present, run gmacs
    if(!file.exists("./gmacs_files_in.dat")) {setwd(wd); gmacs_do_exe(gmacs.dat, pin = pin, reweight = F)}
    dat <- readLines("./gmacs_files_in.dat")
    if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
    # make sure pin file is being used as expected
    if(pin == T){
      dat[grep("use pin file", dat)] <- "1 # use pin file (0=no, 1=yes)"   
      if(!file.exists("gmacs.pin")) {stop(paste("Cannot find gmacs.pin!!"))}
    }
    if(pin == F){
      dat[grep("use pin file", dat)] <- "0 # use pin file (0=no, 1=yes)"   
      
    }
    
    # do jitter ----
    
    # create subdirectory for jitter run files
    dir.create("./jitter")
    # put files in - this likely will not work with relative pathes
    file.copy(c(dat[grep("\\.dat", dat)], dat[grep("\\.ctl", dat)], dat[grep("\\.prj", dat)], "gmacs.exe", "gmacs_files_in.dat"), 
              to = "./jitter")
    # set working 
    setwd("./jitter")
    # turn on reference points
    if(ref_points == T){dat[33] <- "1 # Calculate reference points (0=no)"}
    if(ref_points == F){dat[33] <- "0 # Calculate reference points (0=no)"}
    # set up jitter
    dat[16] <- 1
    dat[18] <- sd
    # write gmacs.dat file
    writeLines(dat, "gmacs.dat"); file.remove("gmacs_files_in.dat")
    gfiles <- list.files()
    
    # do jitter runs
    out <- tibble(iteration = 1:iter,
                  obj_function = NA,
                  max_gradient = NA,
                  catch_lik = NA,
                  index_lik = NA,
                  size_lik = NA,
                  mmb_curr = NA,
                  bmsy = NA)
    for (i in 1:iter) {
      rundir <- paste0("./run_", i)
      dir.create(rundir)
      file.copy(from = gfiles, to = rundir)
      # do gmacs run
      setwd(rundir)
      while(!("gmacs.rep" %in% list.files())){shell("gmacs.exe", wait = wait)}
      ao <- gmacs_read_allout("./Gmacsall.out")
      out$obj_function[i] <- ao$objective_function
      out$max_gradient[i] <- ao$max_gradient
      out$catch_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "catch"]
      out$index_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "index"]
      out$size_lik[i] <- ao$likelihoods_by_type$net_lik[ao$likelihoods_by_type$process == "size"]
      if(ref_points == T) {
        out$mmb_curr[i] <- ao$mmb_curr
        out$bmsy[i] <- ao$bmsy
      }
      setwd("..")
    }
    out <- out %>% dplyr::select(where(function(x) !all(is.na(x))))
    # return to model directory
    setwd("..")
    
  }
  
  # get mle estimates of objects
  mle_ao <- gmacs_read_allout("./Gmacsall.out", model_name = model_name)
  # set wd back to original
  setwd(wd)
  
  # plots ----
  
  if(plot_only == T){out <- read_csv(paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
  
  # obj fxn
  ggplot()+
    geom_histogram(data = out, aes(x = obj_function), color = 1, fill = "grey80", 
                   width = 1)+
    geom_vline(xintercept = mle_ao$objective_function, linetype = 2, color = 2)+
    scale_x_continuous(labels = scales::comma)+
    labs(x = "Negative Log-likelihood", y = "Jitter Runs") -> p_obj
  
  if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_obj_fxn_jitter_sd_", sd, ".png"),
                            plot = p_obj,
                            height = 3, width = 5, units = "in")}
  
  if(ref_points == T){
    # mmb
    ggplot()+
      geom_histogram(data = out, aes(x = mmb_curr), color = 1, fill = "grey80", 
                     width = 1)+
      geom_vline(xintercept = mle_ao$mmb_curr, linetype = 2, color = 2)+
      scale_x_continuous(labels = scales::comma)+
      labs(x = paste0("MMB (", mle_ao$wt_units, ")") , y = "Jitter Runs") -> p_mmb
    
    if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_mmb_jitter_sd_", sd, ".png"),
                              plot = p_mmb,
                              height = 3, width = 5, units = "in")}  
    
    # b35
    ggplot()+
      geom_histogram(data = out, aes(x = bmsy), color = 1, fill = "grey80", 
                     width = 1)+
      geom_vline(xintercept = mle_ao$bmsy, linetype = 2, color = 2)+
      scale_x_continuous(labels = scales::comma)+
      labs(x = bquote(B["35%"]~"("~.(mle_ao$wt_units)~")"), y = "Jitter Runs") -> p_bmsy
    
    if(save_plot == T){ggsave(filename = paste0(plot_dir, "/", mle_ao$model_name, "_b35_jitter_sd_", sd, ".png"),
                              plot = p_bmsy,
                              height = 3, width = 5, units = "in")}
  }
  
  # output ----
  if(ref_points == T){plots <- list(p_obj, p_mmb, p_bmsy)}else{plots <- list(p_obj)}
  if(save_csv == T) {write_csv(out, paste0(csv_dir, "/", mle_ao$model_name, "_jitter_sd_", sd, ".csv"))}
  
  if(save_plot == F){return(c(list(out), plots))}else{return(out)}
  
}


# gmacs_do_retrospective() ----

## run retrospective analysis

### gmacs.dat - file path to gmacs.dat file
### n_peel - number of retrospective peels
### wait - passed to shell(): a logical (not NA) indicating whether the R interpreter should wait for the command to finish, or run it asynchronously.
### pin - T/F use pin file
### plot_only - T/F, only make plot (i.e. already ran retrospectives)
### plot_mmb - T/F, make plot of mmb by peel
### save_plot - T/F, create histograms, default = T
### plot_dir - file directory in which to save plots
### model_name - character string to save as object in output, later to be used for plot legends. example: "23.1b"

gmacs_do_retrospective <- function(gmacs.dat, n_peel, wait = T, pin = F, plot_only = F, plot_mmb = T, save_plot = T, 
                                   plot_dir = NULL, model_name = NULL, ylab = NULL) {
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(dirname(gmacs.dat), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # directory ----
  
  options(warn = -1) 
  # get parent directory
  dir <- dirname(gmacs.dat)
  # save working dir
  wd <- getwd()
  setwd(dir) # change wd

  # analysis ----
  if(plot_only == F) {
    # set up ----
    
    # check for other needed inputs
    if(!file.exists("gmacs.exe")){setwd(wd); stop("Cannot find gmacs.exe!!")}
    # look for gmacs_file_in.dat - if not present, run gmacs
    if(!file.exists("./gmacs_files_in.dat")) {setwd(wd); gmacs_do_exe(gmacs.dat, pin = pin, reweight = F)}
    ao_full <- gmacs_read_allout("Gmacsall.out")
    dat <- readLines("./gmacs_files_in.dat")
    if(!file.exists(file.path(dat[grep("\\.dat", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.dat", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.ctl", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.ctl", dat)]), "!!"))}
    if(!file.exists(file.path(dat[grep("\\.prj", dat)]))) {setwd(wd); stop(paste("Cannot find", file.path(dat[grep("\\.prj", dat)]), "!!"))}
    # create retrospectives dir
    dir.create("./retrospectives",recursive = T, showWarnings = F)
    files_to_copy <- c(dat[c(4, 6, 8)], "gmacs.exe", "gmacs_files_in.dat")
    # make sure pin file is being used as expected
    if(pin == T){
      if(!file.exists("gmacs.pin")) {setwd(wd); stop("Cannot find gmacs.pin!!"); files_to_copy <- c(files_to_copy, "gmacs.pin")}
    }
    # copy files to retro dir
    file.copy(files_to_copy, "./retrospectives",overwrite = T, recursive = T)
    
    # do retrospective runs ----
    
    setwd("./retrospectives")
    file.rename("gmacs_files_in.dat", "gmacs.dat")
    gfiles <- list.files()
    for (i in 1:n_peel){
      # create peel sub-directory
      dir.create(paste0("retro_", i))
      file.copy(gfiles, paste0("retro_", i))
      setwd(paste0("retro_", i))
      # set up gmacs.dat for retro analysis
      dat <- readLines("gmacs.dat")
      dat[30] <- i
      writeLines(dat, "gmacs.dat")
      # run gmacs
      shell("gmacs.exe", wait = wait)
      setwd("..")
    }      
    
  }
  
  if(plot_only == T) {
    ao_full <- gmacs_read_allout("Gmacsall.out")
    setwd("./retrospectives")  
  }

  
  # plot ----
  
  if(plot_mmb == F){setwd(wd); return(mohn_rho)}

  if(plot_mmb == T){
    ao <- list()
    for(i in 1:n_peel){
      ao[[i]] <- gmacs_read_allout(file.path(paste0("retro_", i), "Gmacsall.out"), i)
    }
    setwd(wd) # return to base working directory
    data_summary <- gmacs_get_derived_quantity_summary(ao)
    
    data_summary %>% 
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      left_join(ao_full$derived_quant_summary %>% transmute(year, ssb_full = ssb)) %>%
      filter(year == terminal_yr) %>%
      mutate(rho = abs(ssb - ssb_full) / ssb_full) %>%
      pull(rho) %>% mean -> mohn_rho
    
    data_summary %>%
      group_by(model) %>%
      mutate(terminal_yr = as.character(max(year))) %>% ungroup %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = ssb, group = terminal_yr, color = terminal_yr))+
      geom_text_npc(aes(npcx = "right", npcy = "top"),
                    label = latex2exp::TeX(paste("Mohn's $\\rho$ = ", round(mohn_rho, 3))),
                    check_overlap = T, size = 3)+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_viridis_d()+
      labs(x = NULL, y = ylab, color = "Terminal year") -> p_mmb
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, paste0(model_name,"_retrospective_mmb.png")), plot = p_mmb, height = 3, width = 6)
      return("done")
    }
    if(save_plot == F){return(list(mohn_rho, p_mmb))}
    
  }
}


# gmacs_get_catch_summary() ----

## isolate catch summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_catch_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {
      x$catch_fit_summary %>% 
        mutate(wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units),
               model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
  
}

# gmacs_get_index_summary() ----

## isolate index summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_index_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  # extract index data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$index_fit_summary %>% 
        mutate(wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units),
               model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1)) -> out
  
  return(out)
  
}

# gmacs_get_size_summary() ----

## isolate size comp summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_size_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$size_fit_summary %>% 
        mutate(model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1)) -> out
  
  
  return(out)
  
}

# gmacs_get_derived_quantity_summary() ----

## isolate derived quantity summary data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_derived_quantity_summary <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$derived_quant_summary %>%
        mutate(model = as.character(x$model_name),
               wt_units = gsub("_", " ", x$wt_units),
               n_units = gsub("_", " ", x$n_units))
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}

# gmacs_get_f() ----

## isolate fully selected fishing mortality by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_f <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$F_by_sex_fleet_year_season %>%
        mutate(model = as.character(x$model_name),
               n_sex = x$n_sex)
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}


# gmacs_get_m() ----

## isolate fully selected fishing mortality by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_m <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$M_by_class %>%
        mutate(model = as.character(x$model_name))
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}
# gmacs_get_molt_probability() ----

## isolate molt probability by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_molt_probability <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$molt_probability %>%
        mutate(model = as.character(x$model_name)) %>%
        left_join(x$molt_probability %>%
                    distinct(sex, size, molt_probability, .keep_all = T) %>% 
                    distinct(sex, year) %>% group_by(sex) %>%
                    mutate(year_lead = lead(year)) %>% ungroup() %>%
                    replace_na(list(year_lead = max(x$molt_probability$year))) %>%
                    transmute(sex, year, block = paste0(year, " - ", year_lead)), by = c("sex", "year"))
      })) %>% transmute(data) %>% unnest(data) %>%
    transmute(model, sex, year, size, molt_probability, block) -> out
  
  return(out)
  
}

# gmacs_get_n_matrix() ----

## isolate n matrix data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_n_matrix <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$n_matrix %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_slx() ----

## isolate n matrix data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_slx <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$selectivity %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_pars() ----

## isolate parameters data by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_pars <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      x$parameters %>% 
        mutate(model = as.character(x$model_name)) 
    })) %>% transmute(data) %>% unnest(data) %>%
    dplyr::select(ncol(.), 1:(ncol(.)-1))-> out
  
  return(out)
  
}

# gmacs_get_ref_points() ----

## isolate reference points table by model

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_ref_points <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(data = purrr::map(all_out, function(x) {
      
      x$reference_points %>%
        transmute(parameter_name, estimate) %>%
        pivot_wider(names_from = parameter_name, values_from = estimate) %>%
        transmute(model = as.character(x$model_name),
                  mmb = BMSY * `Bcurr/BMSY`,
                  b35 = BMSY,
                  b_b35 = `Bcurr/BMSY`,
                  male_rbar = `Male_spr_rbar`,
                  rbar_yrs = paste(x$spr_syr, "-", x$spr_nyr),
                  f35 = `Fmsy_1`,
                  fofl = x$spr_fofl * f35,
                  ofl_tot = OFL_tot)
    
    })) %>% transmute(data) %>% unnest(data) -> out
  
  return(out)
  
}

# gmacs_get_lik() ----

## output likelihood table

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_lik <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      
      x$likelihoods_by_type_and_fleet %>%
        transmute(model = x$model_name, process, net_lik) %>%
        # add number of parameters
        add_row(model = x$model_name, process = "n_pars", net_lik = x$parameters %>% filter(phase > 0) %>% nrow()) %>%
        # add total lik
        add_row(model = x$model_name, process = "total", net_lik = x$likelihoods_by_type %>% filter(process == "total") %>% pull(net_lik))
      
      
    })) %>% transmute(data) %>% unnest(data) %>%
    pivot_wider(names_from = model, values_from = net_lik) -> out
  
  return(out)
  
}

# gmacs_get_lik_type_pen() ----

## output likelihood table by type plus some penalties needed for bbrkc

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_get_lik_type_pen <- function(all_out = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract catch data ----
  
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(data = purrr::map(all_out, function(x) {
      
      x$likelihoods_by_type %>%
        transmute(model = x$model_name, process, net_lik) %>%
        # add M deviation penalty
        add_row(model = x$model_name, process = "Mdevs", net_lik = x$penalties %>% filter(penalty == "Mdevs") %>% pull(net_lik)) %>%
        # add sex ratio penalty
        add_row(model = x$model_name, process = "Sex_ratio", net_lik = x$penalties %>% filter(penalty == "Sex_ratio") %>% pull(net_lik))
      
      
    })) %>% transmute(data) %>% unnest(data) %>%
    pivot_wider(names_from = model, values_from = net_lik) -> out
  
  return(out)
  
}

# gmacs_get_recruitment_distribution() ----

## compute size distribution of recruit classes

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### n_rec_class - number of recruitment classes as list with first element vector for males, second element vector for females

gmacs_get_recruitment_distribution <- function(all_out = NULL, file = NULL, model_name = NULL, n_rec_class = NULL){
  
  if(is.null(n_rec_class)){stop("Provide number of recruitment classes; its not in Gmacsall.out")}
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # extract rec dist data ----
  tibble(mod = names(all_out),
         all_out = all_out) %>% 
    mutate(sex = purrr::map(all_out, function(x){if(x$n_sex == 2){return(c("male", "female"))}  else{return("male")}}),
           mod = purrr::map_chr(all_out, function(x){x$model_name})) %>%
    unnest(sex) %>% group_by(sex) %>% nest() %>%
    bind_cols(tibble(n_rec_class)) %>% unnest(data, n_rec_class) %>% arrange(mod) %>% #pull(all_out) %>% .[[1]] -> x
    mutate(rec_dist = purrr::pmap(list(all_out, sex, n_rec_class), function(x, sex, n_rec_class){
      
      if(sex == "male") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males", "Recruitment_rb-males")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]; rbeta <- pars[2]
        size_breaks <- x$size_bins - ((x$size_bins[2]-x$size_bins[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_bins, rec_dist = dist))
      }
      if(sex == "female") {
        # pull parameters
        x$parameters %>%
          filter(parameter %in% c("Recruitment_ra-males", "Recruitment_rb-males", "Recruitment_ra-females", "Recruitment_rb-females")) %>% pull(estimate) -> pars
        # compute rec dist
        ra <- pars[1]*exp(pars[3]); rbeta <- pars[2]*exp(pars[4])
        size_breaks <- x$size_bins - ((x$size_bins[2]-x$size_bins[1])/2)
        ralpha <- ra / rbeta
        z <- pgamma(size_breaks / rbeta, ralpha)
        rec_sdd <- z - lag(z)
        rec_sdd <- rec_sdd[-1]
        rec_sdd[(n_rec_class + 1):length(rec_sdd)] <- 0
        dist <- c(rec_sdd / sum(rec_sdd, na.rm = T), 0)
        return(tibble(size = x$size_bins, rec_dist = dist))
      }
      
    })) %>%
    transmute(model = mod, sex, rec_dist) %>%
    unnest(rec_dist) %>% ungroup -> out
  
  return(out)
  
}

# gmacs_plot_data_range() ----

# plot data range by process, fleet, type and sex

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_data_range(all_out = list(output))

gmacs_plot_data_range <- function(all_out = NULL, save_plot = T, plot_dir = NULL, file = NULL, model_name = NULL) {
  # setup ----
  # read all out 
  if(is.null(all_out)) {
    tibble(file = file,
           model_name = model_name) %>%
      mutate(ao = purrr::map2(file, model_name, function(file, model_name) {gmacs_read_allout(file, model_name)})) -> ao
  }
  if(!is.null(all_out)) {
    tibble(ao = all_out,
           model_name = purrr::map_chr(ao, function(ao) {ao$model_name})) -> ao
  }
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # get data pieces and plot ----
  
  # catch 
  ao %>% 
    mutate(catch = purrr::map(ao, function(data){
      data$catch_fit_summary %>%
        mutate(type = gsub("All", "Total", type)) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex)
      
    }),
    index = purrr::map(ao, function(data){
      data$index_fit_summary %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, series, sex), paste(fleet, series)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(fleet, sex) 
    }),
    size_composition = purrr::map(ao, function(data){
      data$size_fit_summary %>%
        mutate(type = gsub("All", "Total", type)) %>%
        rename(series = mod_series) %>%
        rowwise %>%
        mutate(group = gsub("_", " ", ifelse(data$n_sex > 1, paste(fleet, type, sex), paste(fleet, type)))) %>%
        ungroup %>%
        distinct(group, year, series, sex, fleet, type) %>%
        mutate(fleet = factor(fleet, levels = data$fleet_names),
               type = factor(type, levels = c("Retained", "Total", "Discarded")),
               sex = factor(sex, levels = c("Male", "Female", "Both"))) %>%
        arrange(type, fleet, sex) 
    })) %>%
    transmute(model_name, catch, index, size_composition) %>%
    pivot_longer(2:4, names_to = "process", values_to = "data") %>%
    unnest(data) %>%
    mutate(process = factor(str_to_title(gsub("_", " ", process)), level = c("Catch", "Index", "Size Composition"))) %>%
    arrange(model_name, process, type, fleet, sex) %>%
    mutate(group = factor(group, levels = unique(group))) %>%
    nest_by(model_name) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(model_name, data, function(model_name, data) {
      data %>%
        filter(year > 1980) %>%
        ggplot()+
        geom_point(aes(y = group, x = year, color = fleet), shape = 15, size = 3.75, show.legend = F)+
        facet_wrap(~process, ncol = 1, scales = "free_y")+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_y_discrete(limits = rev)+
        scale_color_manual(values = cbpalette)+
        labs(y = NULL, x = NULL)+
        theme(panel.border = element_blank(),
              axis.line = element_line()) -> p_dat
      
      if(save_plot == T) {
        ggsave(plot = p_dat, 
               filename = file.path(plot_dir, paste0(model_name, "_data_range.png")), 
               width = 7, 
               height = 5, units = "in")
      }
      return(p_dat)
    })) -> out
  
  # output ----
  
  if(save_plot == T){ return("done")} else{return(transmute(out, model_name, plot))}
}

# gmacs_plot_catch() ----

## plot fits to gmacs catch data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### y_labs - optional, custom y axis labels, as character vector
### data_summary - alternate way to bring in data, output of gmacs_get_catch_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_catch(all_out = list(mod_23.1b), plot_dir = "./put/file/here")

gmacs_plot_catch <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, 
                             data_summary = NULL, file = NULL, model_name = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_catch_summary(all_out, file, model_name)}
  
  # plots 
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  data_summary %>%
    nest_by(series, units, .keep = T) %>% ungroup %>%
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%
    #dplyr::slice(1) %>% # pull(data) %>% .[[1]]-> data
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {
      
      # y label
      if(is.null(y_labs)) {
      y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$wt_units), ")")
      if(unique(data$units) == "Numbers") {
        y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", gsub("All", "Total", unique(data$type)), " Catch (", unique(data$n_units), ")")
      }
      }
      
      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, units),
                              year = min(data$year):max(data$year)),
                  by = join_by(model, series, year, units)) %>%
        mutate(obs_l95 = obs_catch * exp(-1.96 * sqrt(log(1 + cv^2))),
               obs_u95 = obs_catch * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
        ggplot()+
        geom_point(aes(x = year, y = obs_catch), color = "grey40")+
        geom_errorbar(aes(x = year, ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey40")+
        geom_line(aes(x = year, y = pred_catch, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      
      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_continuous(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      
      if(save_plot == T) {
        
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("catch_fit_", tolower(unique(data$fleet)), "_", tolower(unique(data$units)), ".png")), 
               width = pwidth, 
               height = pwidth * (3/5), units = "in")
      }
      
      return(p)
      
    })) -> plots
  
  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2), 
           filename = file.path(plot_dir, "catch_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in") 
    
    return("done")
    
  } else {return(plots$plot)}
  
}

# gmacs_plot_index() ----

## plot fits to gmacs index data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### y_labs - optional, custom y axis labels, as character vector
### data_summary - alternate way to bring in data, output of gmacs_get_index_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

## example: gmacs_plot_index(all_out = list(mod_23.1b), plot_dir = "./put/file/here")

gmacs_plot_index <- function(all_out = NULL, save_plot = T, plot_dir = NULL, y_labs = NULL, 
                             data_summary = NULL, file = NULL, model_name = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_index_summary(all_out, file, model_name)}
  
  # plots 
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  data_summary %>%
    nest_by(series, sex, units, .keep = T) %>% ungroup %>%
    mutate(y_lab = ifelse(is.null(y_labs), NA, y_labs)) %>%  
    mutate(plot = purrr::map2(data, y_lab, function(data, y_lab) {
      # y label
      if(is.null(y_labs)) {
        y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index (", unique(data$wt_units), ")")
        if(unique(data$units) == "Numbers") {
          if(is.na(unique(data$n_units)) == FALSE) {
            y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index (", unique(data$n_units), ")")}
          if(is.na(unique(data$n_units)) == TRUE) {
            y_lab <- paste0(gsub("_", " ", unique(data$fleet)), " ", unique(data$sex), " Index") 
          }
        }
      }
      # plot
      data %>%
        right_join(expand_grid(distinct(., model, series, sex, units),
                               year = min(data$year):max(data$year)),
                   by = join_by(model, series, sex, year, units)) %>%
        mutate(obs_l95 = obs_index * exp(-1.96 * sqrt(log(1 + obs_cv^2))),
               obs_u95 = obs_index * exp(1.96 * sqrt(log(1 + obs_cv^2))),
               tot_l95 = obs_index * exp(-1.96 * sqrt(log(1 + tot_cv^2))),
               tot_u95 = obs_index * exp(1.96 * sqrt(log(1 + tot_cv^2)))) %>%
        ggplot()+
        geom_errorbar(aes(x = factor(year), ymin = tot_l95, ymax = tot_u95), width = 0, color = "grey70")+
        geom_errorbar(aes(x = factor(year), ymin = obs_l95, ymax = obs_u95), width = 0, color = "grey20")+
        geom_point(aes(x = factor(year), y = obs_index), color = "grey20")+
        geom_line(aes(x = factor(year), y = pred_index, group = model, color = model))+
        labs(x = NULL, color = NULL, y = y_lab)+
        scale_y_continuous(labels = scales::comma)+
        scale_color_manual(values = cbpalette)+
        coord_cartesian(ylim = c(0, NA)) -> p
      if(length(min(data$year):max(data$year)) > 10) { p + scale_x_discrete(labels = yraxis$labels, breaks = yraxis$breaks) -> p }
      if(save_plot == T) {
        pwidth <- min(max(length(min(data$year):max(data$year))*0.3, 6), 7)
        # save plot
        ggsave(plot = p, 
               filename = file.path(plot_dir, paste0("index_fit_", tolower(unique(data$fleet)), "_",
                                                     tolower(unique(data$sex)), ".png")), 
               width = pwidth, 
               height = pwidth * (4/6), units = "in")
      }
      return(p)
    })) -> plots
  # return ----
  if(save_plot == T) {
    # save plot of all stacked
    ggsave(plot = patchwork::wrap_plots(plots$plot, ncol = 2), 
           filename = file.path(plot_dir, "index_fit.png"),
           height = nrow(plots) / 2 * 4, width = 11, units = "in") 
    return("done")
  } else {return(plots$plot)}
  
}

# gmacs_plot_sizecomp() ----

## plot fits to gmacs size comp data

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### plot_nsamp_est - show stage 2 effective sample sizes on plots? T/F
### nsamp_est_model - name of model to display nsamp_est for. Only required if plotting more than a single model. Defaults to first in list.
### aggregate_series_labels - character vector of labels for aggregate series, ex: c("Male", "Female") or c("New Shell", "Old Shell");
###                           or list with elements being character vectors for each aggregate series (if you want to use different labels)
### data_summary - alternate way to bring in data, output of gmacs_get_index_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided


gmacs_plot_sizecomp <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size",
                                plot_nsamp_est = F, nsamp_est_model = NULL, aggregate_series_label = NULL, data_summary = NULL, file = NULL, 
                                model_name = NULL) {
  
  # get size summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_size_summary(all_out, file, model_name)}
  
  # check dir
  if(save_plot == T & is.null(plot_dir)) {dir.create("./plots", showWarnings = F, recursive = TRUE); plot_dir <- "./plots"}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # make plots with all models
  data_summary %>% 
    nest_by(mod_series, .keep = T) %>% ungroup %>%# pull(data) %>% .[[5]] -> data
    mutate(agg = purrr::map_lgl(data, function(data) {
      # check for aggregated comp
      data <- dplyr::select(data, where(function(x) !all(is.na(x))))
      agg <- "aggregate_series" %in% names(data)
      return(agg)
    }),
    agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
    aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
      if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
      if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
      if(agg == F) {return(NA)}
    }),
    plots = purrr::pmap(list(data, agg, aggregate_series_label), function(data, agg, aggregate_series_label){
      
      ### check nsamp_est_model
      if(is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      
      ## comp, agg comp, and residual plots
      if(agg == T) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        # adjust size bin for the secondary series
        data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        data %>%
          distinct(aggregate_series, plot_size) %>% 
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
        
        ## comp by year ----
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3))) 
        ### plot
        data %>%  
          rowwise %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
                                         paste0("N = ", round(nsamp_obs))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
          mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
          geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_year
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_year, height = height, width = width, units = "in") 
        }
        
        
        ## aggregate size comp ----
        ### plot
        data %>%  
          group_by(model, aggregate_series, size, plot_size) %>%
          summarise(obs = sum(obs), pred = sum(pred),
                    nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
          
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
          mutate(aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_bar(aes(x = plot_size, y = obs, fill = aggregate_series_label), stat = "identity", position = "identity", color = NA, width = bin_width)+
          geom_line(aes(x = plot_size, y = pred, group = interaction(aggregate_series, model), color = model))+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_agg
        ### save
        if(save_plot == T){
          ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_agg, height = 3, width = 5, units = "in") 
        }
        
        ## line residual plot ----
        
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          ggplot()+
          geom_line(aes(x = plot_size, y = residual, group = interaction(aggregate_series, model), color = model))+
          geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
          geom_vline(xintercept = divider, linetype = 2, color = "grey70")+
          scale_x_continuous(breaks = breaks, labels = labels)+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          scale_fill_grey()+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_resid_line
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_resid_line, height = height, width = width, units = "in") 
        }
        
      }
      if(agg == F) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        
        ## comp by year ----
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          rowwise() %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", round(nsamp_obs), "\nN est = ", round(nsamp_est, 1)),
                                         paste0("N = ", round(nsamp_obs))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>% ungroup %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model))+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.6, label = year), check_overlap = T, size = 3)+
          geom_text_npc(aes(npcx = "right", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_year
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_year, height = height, width = width, units = "in") 
        }
        
        
        ## aggregate size comp ----
        ### plot
        data %>%  
          group_by(model, size) %>%
          summarise(obs = sum(obs), pred = sum(pred),
                    nsamp_obs = sum(nsamp_obs), nsamp_est = sum(nsamp_est)) %>% ungroup %>%
          mutate(nsamp_annotate = ifelse(plot_nsamp_est == T,
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","), "\nN est = ", prettyNum(round(nsamp_est, 1), big.mark = ",")),
                                         paste0("N = ", prettyNum(round(nsamp_obs), big.mark = ","))),
                 nsamp_annotate = ifelse(model == nsamp_est_model, nsamp_annotate, NA)) %>%
          ggplot()+
          geom_bar(aes(x = size, y = obs), stat = "identity", position = "identity", color = NA, fill = "grey70", width = bin_width, alpha = 0.5)+
          geom_line(aes(x = size, y = pred, color = model))+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.9, label = nsamp_annotate),
                        check_overlap = T, size = 3)+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_comp_agg
        ### save
        if(save_plot == T){
          ggsave(file.path(plot_dir, paste0("aggregated_comp_fit_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_comp_agg, height = 3, width = 5, units = "in") 
        }
        
        ## line residual plot ----
        
        ### plot dimensions
        cols <- ifelse(n_yr >= 12, 3, ifelse(n_yr >= 6, 2, 1))
        rows <- ifelse(cols == 1, n_yr, ifelse(cols == 2, ceiling(n_yr/2), ceiling(n_yr/3)))  
        ### plot
        data %>%  
          ggplot()+
          geom_line(aes(x = size, y = residual, color = model))+
          geom_hline(yintercept = 0, linetype = 2, color = "grey70")+
          scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), add = c(0, 0)))+
          labs(x = size_lab, y = NULL, color = NULL, fill = NULL)+
          geom_text_npc(aes(npcx = "left", npcy = 0.8, label = year), check_overlap = T, size = 3)+
          facet_wrap(~year, nrow = rows, ncol = cols, dir = "v")+
          scale_color_manual(values = cbpalette)+
          theme(panel.spacing.x = unit(0.2, "lines"),
                panel.spacing.y = unit(0, "lines"),
                panel.border = element_blank(),
                axis.line.x = element_line(color = "grey70", size = 0.2),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size = 8),
                plot.title = element_text(hjust = 0.5),
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                panel.background = element_blank()) -> p_resid_line
        ### save
        if(save_plot == T){
          height = min(rows, 10)
          width = min(cols*3, 9)
          ggsave(file.path(plot_dir, paste0("resid_line_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_resid_line, height = height, width = width, units = "in") 
        }
        
      }
    })) -> out
  
  # make plots by model
  data_summary %>% 
    nest_by(model, mod_series, .keep = T) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(agg = purrr::map_lgl(data, function(data) {
      # check for aggregated comp
      data <- dplyr::select(data, where(function(x) !all(is.na(x))))
      agg <- "aggregate_series" %in% names(data)
      return(agg)
    }),
    agg_series_order = rank(mod_series * ifelse(agg==F, NA, 1), na.last = "keep"),
    aggregate_series_label = purrr::map2(agg, agg_series_order, function(agg, agg_series_order){
      if(agg == T & class(aggregate_series_label) == "list"){return(aggregate_series_label[[agg_series_order]])}
      if(agg == T & class(aggregate_series_label) != "list"){return(aggregate_series_label)}
      if(agg == F) {return(NA)}
    }),
    plots = purrr::pmap(list(data, agg, aggregate_series_label, model), function(data, agg, aggregate_series_label, model){
      
      ### check nsamp_est_model
      if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      if(length(unique(data$model)) > 1 & is.null(nsamp_est_model)){nsamp_est_model <- unique(data$model)[1]}
      
      if(agg == T) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        # adjust size bin for the secondary series
        data <- mutate(data, plot_size = (aggregate_series-1)*(max(size_bins)-min(size_bins)+bin_width*2) + size) 
        # get size breaks and labels for the plot
        brks <- labeling::extended(1, n_bins, m = 3); brks <- brks[brks != 0]
        data %>%
          distinct(aggregate_series, plot_size) %>% 
          nest_by(aggregate_series) %>% ungroup %>%
          mutate(breaks = purrr::map(data, function(data){data %>% dplyr::slice(brks)})) %>%
          pull(breaks) %>% unlist %>% as.numeric -> breaks
        data %>%
          distinct(size, plot_size) %>%
          filter(plot_size %in% breaks) %>% pull(size) -> labels
        data %>%
          filter(aggregate_series > 1) %>%
          group_by(aggregate_series) %>%
          summarise(divider = min(plot_size) - bin_width) %>% pull(divider) -> divider
        if(is.null(aggregate_series_label)) {aggregate_series_label <- unique(data$aggregate_series)}
        
        ## dot plot ----
        
        data %>%
          # compute residual
          mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                 residual < 0 ~ "< 0"),
                 residual = ifelse(residual == 0, NA, residual),
                 aggregate_series_label = factor(aggregate_series_label[aggregate_series], levels = aggregate_series_label)) %>%
          ggplot()+
          geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                     shape = 21, alpha = 0.5)+
          scale_fill_manual(values = c("black", "white", NA))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
          labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
          theme(legend.position = "top")+
          facet_wrap(~aggregate_series_label, ncol = 1, scales = "free_y") -> p_dot
        ### save
        if(save_plot == T){
          height = unique(data$aggregate_series) * 6
          width = min(6, n_yr * 0.5)
          ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_dot, height = height, width = width, units = "in") 
        }
      }
      if(agg == F) {
        ## setup for plotting aggregate series ----
        # get some detail about size bins
        size_bins <- data %>% pull(size) %>% unique 
        n_bins <- length(size_bins)
        n_yr <- length(unique(data$year))
        bin_width <- size_bins[2] - size_bins[1]
        
        ## dot plot ----
        
        data %>%
          # compute residual
          mutate(pos = case_when(residual >= 0 ~ "> 0", 
                                 residual < 0 ~ "< 0"),
                 residual = ifelse(residual == 0, NA, residual)) %>%
          ggplot()+
          geom_point(aes(x = year, y = size, size = abs(residual), fill = pos), 
                     shape = 21, alpha = 0.5)+
          scale_fill_manual(values = c("black", "white", NA))+
          scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels, limits = range(data$year))+
          labs(x = NULL, y = size_lab, size = NULL, fill = NULL)+
          theme(legend.position = "top") -> p_dot
        ### save
        if(save_plot == T){
          height = 6
          width = min(6, n_yr * 0.5)
          ggsave(file.path(plot_dir, paste0(model, "_resid_dot_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
                 plot = p_dot, height = height, width = width, units = "in") 
        }
      }
      ## mean size (francis) plot ---- 
      
      data %>%
        group_by(year) %>%
        summarise(obs_mean_size = weighted.mean(size, obs),
                  pred_mean_size = weighted.mean(size, pred), 
                  sd = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_obs)),
                  l95 = pred_mean_size + sd * qnorm(0.025),
                  u95 = pred_mean_size + sd * qnorm(0.975),
                  sd_est = sqrt(sum((size - pred_mean_size)^2 * pred) / sum(pred)) / sqrt(mean(nsamp_est)),
                  l95_est = pred_mean_size + sd_est * qnorm(0.025),
                  u95_est = pred_mean_size + sd_est * qnorm(0.975)) %>% ungroup %>%
        ggplot()-> tmp
      if(plot_nsamp_est == T){tmp+geom_ribbon(aes(x = year, ymin = l95_est, ymax = u95_est, group = 1), fill = "grey80", alpha = 0.5)->tmp}
      tmp+geom_ribbon(aes(x = year, ymin = l95, ymax = u95, group = 1), fill = "grey60", alpha = 0.5)+
        geom_line(aes(x = year, y = pred_mean_size, group = 1))+
        geom_point(aes(x = year, y = obs_mean_size))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
        labs(x = NULL, y = paste0("Mean ", size_lab)) -> p_mean_size
      ### save
      if(save_plot == T){
        height = 4
        width = 6
        ggsave(file.path(plot_dir, paste0(model, "_mean_size_",tolower(unique(data$fleet)),"_", tolower(unique(data$sex)),"_", tolower(unique(data$type)),".png")),
               plot = p_mean_size, height = height, width = width, units = "in") 
      }
      
      return(list(p_dot, p_mean_size))
      
    })) -> out2
  
  # output
  if(save_plot == F) {return(c(out %>% pull(plots), out2 %>% pull(plots)))} else{"done"}
  
}


# gmacs_plot_mmb() ----

## plot mature male biomass and mature male abundance trajectories

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_ci - T/F add confidence interval ribbon to ssb
### ci_alpha - alpha value for confidence interval, a = 0.05 is 95% CI
### yrs - subset a specific year range, example: c(1990:2022)
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### std_file - file path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
### std_list -  output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.

gmacs_plot_mmb <- function(all_out = NULL, save_plot = T, plot_ci = F, ci_alpha = 0.05, yrs = NULL, plot_dir = NULL, data_summary = NULL, 
                           file = NULL, model_name = NULL, std_file = NULL, std_list = NULL, ylab = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_derived_quantity_summary(all_out, file, model_name)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_ssb", par)) %>%
      transmute(ssb_se = se / (1 / exp(est)), 
                ssb_lci = exp(est) + ssb_se * qnorm(ci_alpha / 2), 
                ssb_uci = exp(est) + ssb_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(ssb_se = NA, ssb_lci = NA, ssb_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}
  # plot ssb
  data_summary %>%
    ggplot()+
    {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = ssb_lci, ymax = ssb_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
    geom_line(aes(x = factor(year), y = ssb, group = model, color = model))+
    geom_point(data = function(x) filter(x, year == max(year)),
               aes(x = factor(year), y = ssb, color = model))+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL, y = ifelse(is.null(ylab) == TRUE, paste0("MMB (", unique(data_summary$wt_units), ")"), ylab), color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mmb
  # plot ssa
  data_summary %>%
    ggplot()+
    geom_line(aes(x = factor(year), y = ssa, group = model, color = model))+
    geom_point(data = function(x) filter(x, year == max(year)),
               aes(x = factor(year), y = ssa, color = model))+
    scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
    scale_color_manual(values = cbpalette)+
    labs(x = NULL, 
         y = ifelse(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones"),
                    "MMA", paste0("MMA (", unique(data_summary$n_units), ")")),
         color = NULL)+
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) -> mma
  if(save_plot == T){
    ggsave(file.path(plot_dir, "mmb_trajectory.png"), plot = mmb, height = 4.2, width = 7, units = "in")
    ggsave(file.path(plot_dir, "mma_trajectory.png"), plot = mma, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F){return(list(mmb, mma))}
  
}

# gmacs_plot_recruitment() ----

## plot recruitment trajectories

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_ci - T/F add confidence interval ribbon to ssb
### ci_alpha - alpha value for confidence interval, a = 0.05 is 95% CI
### yrs - subset a specific year range, example: c(1990:2022)
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### std_file - file path to gmacs.std file. Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
### std_list -  output from gmacs_read_std() as nested list, e.g., std = list(std.24.0, std.16.0). Optional, if plot_ci = T, both std_file and std_list cannot be NULL.
gmacs_plot_recruitment <- function(all_out = NULL, save_plot = T, plot_ci = F, ci_alpha = 0.05, yrs = NULL, plot_dir = NULL, data_summary = NULL, file = NULL, model_name = NULL, std_file = NULL, std_list = NULL) {
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_derived_quantity_summary(all_out, file, model_name)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # add ci if plot_ci is on
  if(plot_ci == T){
    if(is.null(model_name)){model_name <- unique(data_summary$model)}
    if(is.null(std_list)){std_list <- purrr::map2(std_file, model_name, gmacs_read_std)}
    bind_rows(std_list) %>%
      filter(grepl("sd_log_recruits", par)) %>%
      transmute(rec_se = se / (1 / exp(est)), 
                rec_lci = exp(est) + rec_se * qnorm(ci_alpha / 2), 
                rec_uci = exp(est) + rec_se * qnorm(1 - ci_alpha / 2)) %>%
      bind_cols(data_summary, .) -> data_summary
  }
  if(plot_ci == F){
    data_summary %>%
      mutate(rec_se = NA, rec_lci = NA, rec_uci = NA) -> data_summary
  }
  # filter for years if specified
  if(!is.null(yrs)){data_summary %>% filter(year %in% yrs) -> data_summary}
  
  # male and female
  if("recruit_female" %in% names(data_summary)) {
    
    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- c("Total Recruitment", "Female Recruitment", "Male Recruitment")} else{
      y_labs <- paste0(c("Total Recruitment", "Female Recruitment", "Male Recruitment"), " (", unique(data_summary$n_units), ")")
    }
    data_summary %>%
      mutate(tot_recruit = recruit_male + recruit_female) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = tot_recruit, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = y_labs[1], color = NULL)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> tot
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_female, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[2], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> fem
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      labs(x = NULL, y = y_labs[3], color = NULL)+
      scale_color_manual(values = cbpalette)+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, "total_recruitment.png"), plot = tot, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "female_recruitment.png"), plot = fem, height = 4.2, width = 7, units = "in")
      ggsave(file.path(plot_dir, "male_recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = c(tot, fem, mal)}
  }
  # only male
  if(!("recruit_female" %in% names(data_summary))) {
    if(unique(data_summary$n_units) %in% c(1, "1", "1s", "one", "One", "ones", "Ones")){y_labs <- "Recruitment"} else{
      if(is.na(data_summary$n_units)[1] == TRUE){y_labs <- "Recruitment"} else{
        y_labs <- paste0("Recruitment", " (", unique(data_summary$n_units), ")")}
    }
    data_summary %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = recruit_male, group = model, color = model))+
      {if(plot_ci == T){geom_ribbon(aes(x = factor(year), ymin = rec_lci, ymax = rec_uci, group = model, fill = model), alpha = 0.2, show.legend = F)}}+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(NA, NA))+
      labs(x = NULL, y = y_labs, color = NULL)+
      scale_color_manual(values = cbpalette)+
      {if(plot_ci == T){scale_fill_manual(values = cbpalette)}}+
      theme(legend.position = c(1, 1),
            legend.justification = c(1, 1)) -> mal
    
    if(save_plot == T){
      ggsave(file.path(plot_dir, "recruitment.png"), plot = mal, height = 4.2, width = 7, units = "in")
    }
    if(save_plot == F){plots = mal}
  }
  if(save_plot == F){plots} else{"done"}
  
}


# gmacs_plot_f() ----

## plot fishing mortality by fleet

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### data_summary - alternate way to bring in data, output of gmacs_get_f()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### yrs - subset a specific year range, example: c(1990:2022)

gmacs_plot_f <- function(all_out = NULL, save_plot = T, plot_dir = NULL, data_summary = NULL, file = NULL, model_name = NULL, yrs = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_f(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot f by fleet and sex ----
  
  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified
  data_summary %>%
    group_by(model, year, sex, fleet, n_sex) %>%
    summarize(f = sum(`F`)) %>% ungroup %>%
    mutate(fleet_name = gsub("_", " ", fleet),
           sex_name = str_to_title(sex)) %>%
    nest_by(sex_name, fleet_name, .keep = T) %>%
    mutate(ylab = ifelse(length(unique(data$sex_name)) > 1, 
                         paste(unique(data$fleet_name), unique(data$sex_name), "F"), 
                         paste(unique(data$fleet_name), "F"))) %>% ungroup %>% #pull(data) %>% .[[1]] -> data
    mutate(plot = purrr::map2(data, ylab, function(data, ylab) {
      
      data %>%
        ggplot()+
        geom_line(aes(x = factor(year), y = f, color = model, group = model))+
        labs(x = NULL, y = ylab, color = NULL)+
        scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
        scale_color_manual(values = cbpalette) -> x
      if(save_plot == T){
        pwidth <- min(max(length(min(data$year):max(data$year))*0.2, 5), 7)
        ggsave(file.path(plot_dir, paste0(tolower(gsub(" ", "_", ylab)), ".png")), plot = x, 
               height = 0.6*pwidth, width = pwidth, units = "in")
      }
      return(x)
    })) -> by_fleet
  
  return(if(save_plot == T){"done"}else{by_fleet$plot})
  
}


# gmacs_plot_slx() ----

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_slx()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_slx <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_slx(all_out, file, model_name)}
  
  # create output directories
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  
  # plots
  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    nest_by(fleet, .keep = T) %>% ungroup %>% 
    mutate(capture_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>% 
        ggplot()+
        geom_line(aes(x = size, y = slx_capture, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~capture_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(capture_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Selectivity", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_capture.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$capture_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    retention_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_retention, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Retention", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_retention.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    }),
    discard_plot = purrr::map2(data, fleet, function(data, fleet){
      
      data %>%
        ggplot()+
        geom_line(aes(x = size, y = slx_discard, color = model))+
        {if(length(unique(data$sex)) == 1){facet_wrap(~ret_disc_block, nrow = 1)}}+
        {if(length(unique(data$sex)) > 1){facet_grid(cols = vars(ret_disc_block), rows = vars(sex))}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Prob. Discard", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(tolower(fleet), "_slx_discard.png")),
               height = length(unique(data$sex)) * 3, width = min(length(unique(data$ret_disc_block)) * 4, 8), units = "in") 
      }
      return(x)
      
    })) -> out
  
  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {c(out$capture_plot, out$retention_plot, out$discard_plot)}
  

  }


# gmacs_plot_m() ----

## ## plot natural mortality

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### by - vector of grouping variable. Example = c("year", "sex"). Optional, if NULL, the function will determine how M varies
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_m()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### yrs - subset a specific year range, example: c(1990:2022)

gmacs_plot_m <- function(all_out = NULL, save_plot = T, plot_dir = NULL, by = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL, yrs = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_m(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot m ----
  
  if(!is.null(yrs)){data_summary <- filter(data_summary, year %in% yrs)} # filter year range if specified
  
  if(is.null(by)){
    data_summary %>%
      distinct(M, .keep_all = T) %>%
      dplyr::select_if(function(x){length(unique(x)) > 1}) %>%
      dplyr::select(-M) %>% names -> by
  }
  by <- by[by!="model"]
  
  #plot dimensions
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(sex) %>% unique %>% length -> nsex
  data_summary %>%
    distinct(M, .keep_all = T) %>%
    pull(maturity) %>% unique %>% length -> nmat
  
  if(!("size" %in% by) & "year" %in% by) {
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = factor(year), y = M, group = model, color = model))+
      geom_point(data = function(x) filter(x, year == max(year)),
                 aes(x = factor(year), y = M, color = model))+
      scale_x_discrete(breaks = yraxis$breaks, labels = yraxis$labels)+
      scale_y_continuous(labels = scales::comma, limits = c(0, NA))+
      scale_color_manual(values = cbpalette)+
      labs(x = NULL, y = paste0("Natural mortality (M)"), color = NULL)+
      facet_wrap(by[by!="year"], ncol = 1, scales = "free_y") -> x
    
  }
  if("size" %in% by & "year" %in% by){
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, year, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = factor(year), linetype = model))+
      labs(x = size_lab, y = "Natural Mortality (M)", color = NULL, linetype = NULL)+
      facet_wrap(by[by != "size"]) -> x
    
  }
  if("size" %in% by & !("year" %in% by)){
    
    data_summary %>%
      mutate(sex = str_to_title(sex),
             sex = factor(sex, levels = c("Male", "Female"))) %>%
      mutate(maturity = case_when(maturity == 1 ~ "Mature",
                                  maturity == 2 ~ "Immature"),
             maturity = factor(maturity, levels = c("Mature", "Immature"))) %>%
      distinct(model, !!!syms(by), M) %>%
      ggplot()+
      geom_line(aes(x = size, y = M, color = model))+
      labs(x = size_lab, y = "Natural Mortality (M)", color = NULL)+
      facet_wrap(by[!(by %in% c("size", "year"))], ncol = 1) -> x
    
  }
  
  if(save_plot == T){
    ggsave(file.path(plot_dir, "natural_mortality.png"), plot = x, height = 4.2 * nsex * nmat, width = 7, units = "in")
  }
  
  return(if(save_plot == T){"done"}else{x})
  
}

# gmacs_plot_recruitment_distribution() ----

## plot recruitment distribution

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### n_rec_class - number of recruitment classes as list with first element vector for males, second element vector for females
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_derived_quantity_summary()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_recruitment_distribution <- function(all_out = NULL, save_plot = T, plot_dir = NULL, n_rec_class = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL) {
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_recruitment_distribution(all_out, file, model_name, n_rec_class = n_rec_class)}
  
  # plots ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  data_summary %>%
    mutate(sex = str_to_title(sex)) %>%
    ggplot()+
    geom_line(aes(x = size, y = rec_dist, color = model))+
    labs(x = size_lab, y = "Recruitment Proportion", color = NULL)+
    theme(legend.position = c(1, 1), legend.justification = c(1, 1))+
    scale_color_manual(values = cbpalette)+
    {if(length(unique(data_summary$sex)) > 1) {facet_wrap(~sex, ncol = 1)}} -> p
  if(save_plot == T){
    ggsave(file.path(plot_dir, "recruitment_distribution.png"), plot = p, height = 4.2, width = 7, units = "in")
    return("done")
  }
  if(save_plot == F) {return(p)}
}




# gmacs_plot_molt_probability() ----

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### size_lab - optional, custom size axis label, as character vector, example: "Carapace Length (mm)", default = "Size"
### data_summary - alternate way to bring in data, output of gmacs_get_molt_probability()
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_molt_probability <- function(all_out = NULL, save_plot = T, plot_dir = NULL, size_lab = "Size", data_summary = NULL, file = NULL, model_name = NULL){
  
  # get summary data
  if(is.null(data_summary)){data_summary <- gmacs_get_molt_probability(all_out, file, model_name)}
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # plot molt probability ----
  
  data_summary %>%
    distinct(model, sex, block, size, molt_probability, block) %>%
    nest_by(sex) %>% ungroup %>% 
    mutate(plot = purrr::map2(sex, data, function(sex, data){
      
      data %>%
        filter(!is.na(block)) %>%
        ggplot()+
        geom_line(aes(x = size, y = molt_probability, color = model))+
        {if(length(unique(data$block)) > 1) {facet_wrap(~block, nrow = 1)}}+
        scale_color_manual(values = cbpalette)+
        labs(x = size_lab, y = "Molt Probability", color = NULL) -> x
      
      if(save_plot == T) {
        # save plot of all stacked
        ggsave(plot = x, 
               filename = file.path(plot_dir, paste0(sex, "_molt_probability.png")),
               height = length(unique(sex)) * 3, width = min(length(unique(data$block[!is.na(data$block)]))*4, 8), units = "in") 
      }
      return(x)
    })) -> out
  
  # output
  if(save_plot == T) {return("done")}
  if(save_plot == F) {out$plot} 
  
}


# gmacs_plot_f_mmb() ----

## plot relationship between f and mmb

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### beta- spr beta. Default 0.25
### alpha - spr alpha. Default 0.1
### spr_targ - target spr ratio. Default 0.35
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_f_mmb <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # extract data
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
           plot = purrr::map(all_out, function(x) {
             
             
             # get directed fs
             gmacs_get_f(list(x)) %>%
               filter(sex == "male") %>%
               group_by(year) %>%
               summarise(f = sum(F)) -> fs
             
             # get mmb time series
             gmacs_get_derived_quantity_summary(list(x)) %>%
               transmute(year, mmb = ssb) -> mmb
             
             # set up mmb vector for fofl control rule line
             b <- 0:max(mmb$mmb)
             
             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot
             
             # control rule
             tibble(b = b) %>%
               mutate(f_ofl = case_when(b / btarg <= beta ~ 0,
                                        b / btarg > beta & b/btarg <= 1 ~ ftarg * ((b/btarg - alpha) / (1 - alpha)),
                                        b / btarg > 1 ~ ftarg)) -> control_rule
             
             # plot annotation
             annotation <- paste0("F", spr_targ*100, " = ", round(ftarg, 3), "\nFOFL = ", round(x$f_ofl_tot, 3))
             
             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(f > 0) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = b, y = f_ofl), size = 1)+
               geom_text(aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5)+
               geom_text(data = function(x){filter(x, year == max(x$year))}, 
                         aes(x = mmb, y = f, label = substring(year, 3, 4)), size = 2.5, color = "firebrick")+
               geom_text_npc(aes(npcx = "right", npcy = "top", label = annotation),
                             check_overlap = T, size = 3)+
               scale_x_continuous(labels = scales::comma)+
               labs(x = paste0("MMB (", x$wt_units, ")"), y = "F") -> p
             
             if(save_plot == T) {
               ggsave(plot = p, 
                      filename = file.path(plot_dir, paste0(x$model_name, "_f_mmb.png")),
                      height = 4, width = 5, units = "in") 
             }
             
             return(p)
             
           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}
  
  
}

# gmacs_plot_kobe() ----

## kobe plot

## args:
### all_out - output from gmacs_read_allout as nested list, example: all.out = list(mod_23.0a, mod_23.1b)
### save_plot - T/F save plots, default = T
### plot_dir - file directory in which to save plots
### beta- spr beta. Default 0.25
### alpha - spr alpha. Default 0.1
### spr_targ - target spr ratio. Default 0.35
### file - file paths to Gmacsall.out for each model to compare, passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided
### model_name - character string passed to gmacs_read_allout(), expressed as character vector, not needed if all.out is provided

gmacs_plot_kobe <- function(all_out = NULL, save_plot = T, plot_dir = NULL, beta = 0.25, alpha = 0.1, spr_targ = 0.35, data_summary = NULL, file = NULL, model_name = NULL){
  
  # bring in all out data ----
  
  if(!is.null(file) && is.null(all_out)) {
    if(is.null(model_name)) {stop("Must supply model name(s)!!")}
    # read all out file
    all_out <- purrr::map2(file, model_name, gmacs_read_allout); names(all_out) <- paste0("model", model_name)
  }
  
  # save plot dirs ----
  if(save_plot == T & is.null(plot_dir)) {plot_dir <- file.path(getwd(), "plots"); dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  if(!is.null(plot_dir) && !file.exists(plot_dir)) {dir.create(plot_dir, showWarnings = F, recursive = TRUE)}
  
  # extract data
  tibble(mod = names(all_out),
         all_out = all_out) %>%
    mutate(model = purrr::map_chr(all_out, function(x) {x$model_name}),
           plot = purrr::map(all_out, function(x) {
             
             
             # get directed fs
             gmacs_get_f(list(x)) %>%
               filter(sex == "male") %>%
               group_by(year) %>%
               summarise(f = sum(F)) -> fs
             
             # get mmb time series
             gmacs_get_derived_quantity_summary(list(x)) %>%
               transmute(year, mmb = ssb) -> mmb
             
             # first year of catch
             gmacs_get_catch_summary(list(x)) %>% pull(year) %>% min -> first_yr
             
             # get biomass target and fmsy
             btarg <- x$bmsy
             ftarg <- x$f_msy_tot
             
             # control rule
             tibble(status = seq(0, max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1, 0.0001)) %>%
               mutate(f_ofl = case_when(status <= beta ~ 0,
                                        status > beta & status <= 1 ~ ((status - alpha) / (1 - alpha)),
                                        status > 1 ~ 1)) -> control_rule
             # bacground polygons
             # all coords are top left clockwise
             p_xlim <- max(mmb$mmb[mmb$year >= first_yr])/btarg * 1.1
             p_ylim <- max(fs$f[mmb$year >= first_yr])/ftarg * 1.1
             glite <- tibble(x = c(1, p_xlim, p_xlim, 1), 
                             y = c(1, 1, 0, 0))
             ylite <- tibble(x = c(1, 1, beta, beta, 1, p_xlim, p_xlim, 1), 
                             y = c(1, 0, 0, ((beta - alpha) / (1 - alpha)), p_ylim, p_ylim, 1, 1), 
                             group = c(1, 1, 1, 1, 2, 2, 2, 2))
             rlite <- tibble(x = c(0, 1, 1, beta, beta, 0), 
                             y = c(p_ylim, p_ylim, 1, ((beta - alpha) / (1 - alpha)), 0, 0))
             vline <- tibble(x = c(1, 1), y = c(0, p_ylim))
             
             # plot
             left_join(fs, mmb, by = "year") %>%
               filter(year >= first_yr) %>%
               ggplot()+
               geom_line(data = control_rule, aes(x = status, y = f_ofl), linetype = 1, size = 1)+
               geom_line(data = vline, aes(x = x, y= y), linetype = 2)+
               geom_polygon(data = glite, aes(x = x, y = y), color = NA, fill = "green", alpha = 0.3)+
               geom_polygon(data = ylite, aes(x = x, y = y, group = group), color = NA, fill = "yellow", alpha = 0.3)+
               geom_polygon(data = rlite, aes(x = x, y = y), color = NA, fill = "red", alpha = 0.3)+
               geom_point(aes(x = mmb/btarg, y = f/ftarg))+
               geom_text(aes(x = mmb/btarg, y = f/ftarg, label = substring(year, 3, 4)), size = 2, nudge_y = 0.05)+
               geom_path(aes(x = mmb/btarg, y = f/ftarg), size = 0.2)+
               geom_point(data = function(x){filter(x, year == max(x$year))},
                          aes(x = mmb/btarg, y = f/ftarg), size = 3, shape = 21, fill = "white")+
               labs(x = bquote(B/B[.(spr_targ*100)~"%"]), y = bquote("F/"~F[.(spr_targ*100)~"%"])) -> p 
             
             if(save_plot == T) {
               ggsave(plot = p, 
                      filename = file.path(plot_dir, paste0(x$model_name, "_kobe.png")),
                      height = 4, width = 5, units = "in") 
             }
             
             return(p)
             
           })) -> out
  # out ----
  if(save_plot == T) {return("done")}
  if(save_plot == F) {return(out$plot)}
  
  
}


