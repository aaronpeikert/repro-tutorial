library(tidyverse)
source(here::here("R", "simulation_funs.R"))
# if you have access to a hpc envir specify this in R/hpc.R
# see https://github.com/aaronpeikert/repro-tutorial/blob/hpc/R/hpc.R
# or `git checkout hpc R/hpc.R`
# if no hpc is availible we use local multicore with all available cores
# to speed up consider reduce nsim; increase nstep ↓↓↓

hpc_config <- here::here("R", "hpc.R")
if(fs::file_exists(hpc_config)){
  source(hpc_config)
} else {
  plan(list(transparent,
            tweak(multisession)))
}

setup <- tidyr::expand_grid(
  n = c(10, seq(100, 1000, 100)),
  df = 8, # skew = sqrt(8/df)
  d = seq(0, .5, 0.05),
  i = 10)
res_raw %<-% simulation_study(setup, 10000, 1235)

res <- res_raw %>% 
  group_by(across(-results)) %>% 
  unnest_wider(results) %>% 
  summarise(power = mean(p_value < 0.025),
            cohend_mean = mean(cohend),
            cohend_sd = sd(cohend),
            .groups = "drop")
fs::dir_create(here::here("data"))
write_csv(res, here::here("data", "simulation_results.csv"))

res %>% 
  ggplot(aes(n, power, color = d, group = d)) +
  geom_line() + 
  theme_minimal() +
  NULL
