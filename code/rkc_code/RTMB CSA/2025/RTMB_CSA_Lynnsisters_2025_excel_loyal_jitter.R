### Lynn Sisters RTMB CSA 2025 - JITTER q (FIXED) and T12 (start) ###
## Alex Reich
## Purpose: at each run, FIX q at a jittered value and let the rest of the
##          model estimate. T12 is also jittered as a starting value.
##          Look at the jnLL surface to check for local minima.
##
## Run the data-prep section of RTMB_CSA_Lynnsisters_2025_excel_loyal.R FIRST
## (lines that build `data`, `pars`, `map`). This script picks up from there.
##############################################################################

#notes: excel q is 0.00076. RTMB q is 0.00057 (fully estimated). That's not too far apart. But results in big biomass changes
##idk, try this for seymor tomorrow


library(tidyverse)
library(RTMB)

source("code/rkc_code/RTMB CSA/RTMB_CSA_generalize_5_replicate_excelloyal_sumsq.R")

# ---- Settings ----
n_jitter   <- 700        # number of jitter runs
q_sd       <- 0.3        # sd for rnorm on log(q)   (try 0.3 - 0.5)
T12_sd     <- 0.3        # sd for rnorm on log(T12)
estimate_T12 <- FALSE    # FALSE = T12 stays fixed at its (jittered) start;
# TRUE  = also estimate T12
set.seed(123)

# ---- Base starting values ----
ln_q_base   <- pars$ln_q
ln_T12_base <- pars$ln_T12

# ---- Storage ----
out <- data.frame(
  i        = integer(n_jitter),
  q_fixed  = numeric(n_jitter),   # the fixed q value for this run
  T12_start = numeric(n_jitter),
  T12_est  = numeric(n_jitter),
  jnLL     = numeric(n_jitter),
  max_grad = numeric(n_jitter),
  converged = logical(n_jitter)
)

# Map for the jitter: FIX q (map$ln_q = factor(NA))
map_j <- map
map_j$ln_q <- factor(NA)
if (estimate_T12) map_j$ln_T12 <- NULL   # unmap T12 if you want it estimated

# ---- Loop ----
for (i in seq_len(n_jitter)) {
  pars_j <- pars
  pars_j$ln_q   <- ln_q_base   + rnorm(1, 0, q_sd)   # FIXED at this value
  pars_j$ln_T12 <- ln_T12_base + rnorm(1, 0, T12_sd) # start (or fixed if mapped)
  
  m <- try(run_csa_model(data, pars_j, map_j, newtonsteps = 0), silent = TRUE)
  
  if (inherits(m, "try-error")) {
    out[i, ] <- list(i, exp(pars_j$ln_q), exp(pars_j$ln_T12),
                     NA, NA, NA, FALSE)
    next
  }
  
  bp <- m$pop_mod$env$last.par.best
  mg <- max(abs(m$pop_mod$gr(bp)))
  
  out$i[i]         <- i
  out$q_fixed[i]   <- exp(pars_j$ln_q)
  out$T12_start[i] <- exp(pars_j$ln_T12)
  out$T12_est[i]   <- m$report$T12
  out$jnLL[i]      <- m$report$jnLL
  out$max_grad[i]  <- mg
  out$converged[i] <- mg < 0.001
}

# ---- Save ----
dir.create(paste0("results/rkc/LynnSisters/", cur_yr, "/jitter"),
           showWarnings = FALSE, recursive = TRUE)
write.csv(out,
          paste0("results/rkc/LynnSisters/", cur_yr,
                 "/jitter/jitter_q_T12_", cur_yr, ".csv"),
          row.names = FALSE)

# ---- Quick look ----
cat("Best jnLL: ", min(out$jnLL, na.rm = TRUE), "\n")
cat("Converged runs: ", sum(out$converged, na.rm = TRUE), "/", n_jitter, "\n")
print(summary(out[, c("q_fixed", "T12_est", "jnLL", "max_grad")]))

# ---- Plots ----
out_ok <- out %>% filter(!is.na(jnLL))

# Profile-style plot: jnLL vs fixed q
g1 <- ggplot(out_ok, aes(q_fixed, jnLL, color = converged)) +
  geom_point() +
  labs(title = "jnLL vs fixed q", x = "q (fixed)", y = "jnLL") +
  theme_minimal()+
  geom_vline(xintercept = 0.00076, linetype = 2, color = "red") + #excel q estimate for lynn sisters
  geom_vline(xintercept = 0.00057, linetype = 2, color = "blue") #rtmb q estimate for lynn sisters

# jnLL vs T12 (start or estimated)
g2 <- ggplot(out_ok, aes(T12_start, jnLL, color = converged)) +
  geom_point() +
  labs(title = "jnLL vs T12 start", x = "T12 start", y = "jnLL") +
  theme_minimal()

# T12: estimated vs start (only meaningful if estimate_T12 = TRUE)
g3 <- ggplot(out_ok, aes(T12_start, T12_est, color = converged)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(title = "T12 est vs start", x = "T12 start", y = "T12 est") +
  theme_minimal()

# 2D: q vs T12 colored by jnLL
g4 <- ggplot(out_ok, aes(q_fixed, T12_start, color = jnLL)) +
  geom_point(size = 2) +
  scale_color_viridis_c() +
  labs(title = "jnLL surface: q (fixed) x T12 (start)",
       x = "q (fixed)", y = "T12 start") +
  theme_minimal()

library(patchwork)
gall <- (g1 | g2) / (g3 | g4)
print(gall)

ggsave(paste0("figures/rkc/", cur_yr, "/jitter/CSA_LS_jitter_q_T12_finescale.png"),
       plot = gall, width = 10, height = 8, dpi = 300)

#q has a really flat likelihood surface, and small changes in q (a really small number) are a big deal for calculating the biomass
##add the jitter graphs to the ppt- both fine scale and course/wide scale.


