


files_longterm <- c(paste0(here::here(),"/results/rkc/Pybus/", cur_yr, "/longterm.csv"), 
           paste0(here::here(),"/results/rkc/Excursion/", cur_yr, "/longterm.csv"),
           paste0(here::here(), "/results/rkc/Gambier/", cur_yr, "/longterm.csv"), 
           paste0(here::here(), "/results/rkc/Juneau/", cur_yr, "/longterm.csv"), 
           paste0(here::here(), "/results/rkc/LynnSisters/", cur_yr, "/longterm.csv"), 
           #paste0(here::here(), "/results/rkc/Peril/", cur_yr, "/longterm.csv"), 
           paste0(here::here(), "/results/rkc/Seymour/", cur_yr, "/longterm.csv"))

longterm_all <- files_longterm %>%
  map(read.csv) 

names(longterm_all) <- c("Pybus", "Excursion", "Gambier", "Juneau", "LynnSisters", "Seymour")

longterm_trends <- do.call(rbind, Map(cbind.data.frame, name = names(longterm_all), longterm_all))

row.names(longterm_trends) <- NULL

# average regional decline in legal biomass since 2017

files_regch <- c(paste0(here::here(), "/results/rkc/Region1/2019/change_in_modeled_regional_biomass_2019.csv"), 
                    paste0(here::here(), "/results/rkc/Region1/2020/change_in_modeled_regional_biomass_2020.csv"), 
                    paste0(here::here(), "/results/rkc/Region1/2021/change_in_modeled_regional_biomass_2021.csv"), 
                    paste0(here::here(), "/results/rkc/Region1/2022/change_in_modeled_regional_biomass_2022.csv"), 
                    paste0(here::here(), "/results/rkc/Region1/2023/change_in_modeled_regional_biomass_2023.csv"))

regch_list <- files_regch %>%
  map(read.csv) 

names(regch_list) <- c("2019", "2020", "2021", "2022", "2023")

type_change <- c("type", "change")

regch_list2 <- lapply(regch_list, "[", , type_change)

regch_all <- do.call(rbind, Map(cbind.data.frame, name = names(regch_list2), regch_list2))

row.names(regch_all) <- NULL

avg_change_legal <- regch_all %>%
  filter(type == "adj_legal") %>%
  summarise(mean_change = mean(change))

avg_change_mature <- regch_all %>%
  filter(type == "adj_mature") %>%
  summarise(mean_change = mean(change))


