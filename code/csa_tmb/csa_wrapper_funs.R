# notes ----
## wrapper functions SE crab CSA models
## tyler jackson
## 11/10/2022

# load ---- 
library(TMB)
library(tidyverse)
library(lubridate)
library(patchwork)

theme_sleek <- function(base_size = 12, base_family = "Times") {
  
  windowsFonts(Times=windowsFont("TT Times New Roman"))
  
  half_line <- base_size/2
  
  theme_light(base_size = base_size, base_family = base_family) +
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
yraxis <- tickr(tibble(yr = 1980:2100), yr, 5)

# load data function ----

f_load_dat <- function(path) {
  dat <- as.matrix(read.table(path, fill = T, row.names = NULL, header = F, col.names = paste0("V",seq_len(1000))))
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
  
  return(list(yrs = yrs, catch_num = catch_num, obs_index = obs_index, tau_cs = tau_cs, tau_s = tau_s, M = M, wt_survey = wt_survey, avg_wt = avg_wt))
}

# load pin file function ----

f_load_pin <- function(path, nstage, nyrs) {
  
  ln_index_init <- scan(path, skip = 2, n = nstage-1, quiet = T)
  trans_probs <- scan(path, skip = 4, n = nstage*2 - 2, quiet = T)
  ln_Rbar <- scan(path, skip = 6, n = 1, quiet = T)
  Eps_R <- scan(path, skip = 8, n = nyrs, quiet = T)
  ln_sigmaR <- scan(path, skip = 10, n = 1, quiet = T)
  preM <- scan(path, skip = 12, n = 1, quiet = T)
  ln_q <- scan(path, skip = 14, n = 1, quiet = T)
  ln_mu <- scan(path, skip = 16, n = 1, quiet = T)
  
  return( list(ln_index_init = ln_index_init, trans_probs = trans_probs, ln_Rbar = ln_Rbar,
              Eps_R = Eps_R, ln_sigmaR = ln_sigmaR, preM = preM, ln_q = ln_q, ln_mu = ln_mu) )
    

}

# fit model ----

## default is status quo three stage model as adapted from excel
f_fit_csa <- function(data, par, recruit_likelihood = 0, recruit_stage = 2, preM_tau = 0, map_set = 1, map,
                      rep = T, plots = T, path = "./output/csa_tanner", prefix, stage_labs = c("Pre-recruit", "Recruit", "Post-recruit")) {
  
  # set map for status quo version of the model
  if(map_set == 1) {map <- list(trans_probs = rep(factor(NA), 4), ln_sigmaR = factor(NA), ln_Rbar = factor(NA), ln_mu = factor(NA))}
  
  # set up model
  obj <- TMB::MakeADFun(data = c(recruit_likelihood = recruit_likelihood,
                                 recruit_stage = recruit_stage,
                                 preM_tau = preM_tau,
                                 data),
                        parameters = par,
                        DLL = "CSA_TANNER", silent = T , map = map, hessian = T)
  # optimize
  opt <- nlminb(start = obj$par, obj = obj$fn, gr = obj$gr, control = list(eval.max = 100000, iter.max = 1000))
  
  # create output list
  out <- list(input = data,
              map = map,
              par = opt$par,
              objective = opt$objective,
              convergence = opt$convergence,
              sdreport = sdreport(obj),
              report = obj$report(),
              hessian = obj$hessian,
              opt = opt,
              obj = obj) 
  
  
  # plots 
  if(plots == T) {f_csa_plots(out, path = path, prefix = prefix, stage_labs = stage_labs)}
  
  # save par and report files
  f_rep_file(out, path = path, prefix = prefix)
  f_par_file(out, path = path, prefix = prefix)
  
  # return model output
  return(out)
  
}

# plots ----

f_csa_plots <- function(fit, path, prefix, stage_labs = c("Pre-recruit", "Recruit", "Post-recruit")) {
  
  # cpue plots ----
  
  cpue_plots <- NULL
  for(i in 1:ncol(fit$input$obs_index)) {
    
    local({
      i <- i
      
      ggplot()+
        geom_point(aes(x = fit$input$yrs, y = fit$input$obs_index[,i]), shape = 21, fill = "white")+
        geom_line(aes(x = fit$input$yrs, y = fit$report$index[,i]))+
        labs(x = NULL, y = paste(stage_labs[i], "CPUE"))+
        scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels) -> x
      print(x)}) -> cpue_plots[[i]]
    
    
  }
  eval(parse(text = paste(paste0("cpue_plots[[", 1:length(cpue_plots), "]]"), collapse = "/"))) -> cpue_fit
  ggsave(file.path(path, paste0(prefix, "_index_fit.png")), cpue_fit, height = 6, width = 4, units = "in")
  
  # MMB plot ----
  
  as.data.frame(summary(sdreport(fit$obj))) %>%
    rownames_to_column(var = "par") %>%
    filter(grepl("MMB", par)) %>%
    rename_all(~c("par", "est", "se")) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + se^2/est^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + se^2/est^2)))) %>%
    ggplot()+
    geom_ribbon(aes(x = fit$input$yrs, ymin = l95, ymax = u95), fill = "lightblue", alpha = 0.5)+
    geom_line(aes(x = fit$input$yrs, y = est))+
    labs(x = NULL, y = "MMB (lb)")+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma) -> mmb
  ggsave(file.path(path, paste0(prefix, "_mmb.png")), mmb, height = 3, width = 5, units = "in")
  
  # LMB plot ----
  
  as.data.frame(summary(sdreport(fit$obj))) %>%
    rownames_to_column(var = "par") %>%
    filter(grepl("LMB", par)) %>%
    rename_all(~c("par", "est", "se")) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + se^2/est^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + se^2/est^2)))) %>%
    ggplot()+
    geom_ribbon(aes(x = fit$input$yrs, ymin = l95, ymax = u95), fill = "lightgreen", alpha = 0.5)+
    geom_line(aes(x = fit$input$yrs, y = est))+
    labs(x = NULL, y = "LMB (lb)")+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma) -> lmb
  ggsave(file.path(path, paste0(prefix, "_lmb.png")), lmb, height = 3, width = 5, units = "in")
  
  
  # preMB plot ----
  
  as.data.frame(summary(sdreport(fit$obj))) %>%
    rownames_to_column(var = "par") %>%
    filter(grepl("PMB", par)) %>%
    rename_all(~c("par", "est", "se")) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + se^2/est^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + se^2/est^2)))) %>%
    ggplot()+
    geom_ribbon(aes(x = fit$input$yrs, ymin = l95, ymax = u95), fill = "violet", alpha = 0.5)+
    geom_line(aes(x = fit$input$yrs, y = est))+
    labs(x = NULL, y = "Pre-recruit Male Biomass (lb)")+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma) -> pmb
  ggsave(file.path(path, paste0(prefix, "_pmb.png")), pmb, height = 3, width = 5, units = "in")
  
  # MMB & LMB plot ----
  
  as.data.frame(summary(sdreport(fit$obj))) %>%
    rownames_to_column(var = "par") %>%
    filter(grepl("MB", par)) %>%
    mutate(par = substring(par, 1, 3),
           yr = rep(fit$input$yrs, 3)) %>%
    filter(par %in% c("MMB", "LMB")) %>%
    rename_all(~c("par", "est", "se", "yr")) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + se^2/est^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + se^2/est^2)))) %>%
    ggplot()+
    geom_ribbon(aes(x = yr, ymin = l95, ymax = u95, fill = par), alpha = 0.5)+
    geom_line(aes(x = yr, y = est, group = par))+
    labs(x = NULL, y = "Male Biomass (lb)", fill = NULL)+
    scale_x_continuous(breaks = yraxis$breaks, labels = yraxis$labels)+
    scale_y_continuous(labels = scales::comma)+
    scale_fill_manual(values = c("lightgreen", "lightblue"), labels = c("Legal", "Mature"))+
    theme(legend.position = c(0,1), legend.justification = c(0,1)) -> mmblmb
  ggsave(file.path(path, paste0(prefix, "_mmblmb.png")), mmblmb, height = 3, width = 5, units = "in")
  
 # out ---
  return(list(cpue_fit = cpue_fit,
              mmb = mmb, lmb = lmb, pmb = pmb,
              mmblmb = mmblmb))
  
  
  
}


# output rep  and par----

f_rep_file <- function(fit, path, prefix) {
  
  as.data.frame(summary(sdreport(fit$obj))) %>%
    rownames_to_column(var = "par") %>%
    filter(grepl("MB", par)) %>%
    mutate(par = substring(par, 1, 3),
           yr = rep(fit$input$yrs, 3)) %>%
    rename_all(~c("par", "est", "se", "yr")) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + se^2/est^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + se^2/est^2)))) %>%
    dplyr::select(yr, par, est, se, l95, u95) -> biomass
  
  
  capture.output(c(fit$report, biomass = list(biomass)), file = file.path(path, paste0(prefix, ".rep")))
  
}


f_par_file <- function(fit, path, prefix) {
  
  capture.output(sdreport(fit$obj), file = file.path(path, paste0(prefix, ".par")))
  
}


