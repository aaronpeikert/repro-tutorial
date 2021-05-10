library(tidyverse)
library(here)
library(lavaan)
library(furrr)
source(here("R", "simulation_funs.R"))
# if you have access to a hpc envir specify this in R/hpc.R
# if not we use local multicore
# to speed up consider reduce nsim; increase nstep ↓↓↓

hpc_config <- here::here("R", "hpc.R")
if(fs::file_exists(hpc_config)){
  source(hpc_config)
} else {
  plan(list(transparent,
            tweak(multisession, workers = 4L)))
}
# debug:
#   plan(transparent)

nmin <- 100
nmax <- 300
nstep <- 100
nsim <- 2
seed <- 1234
n_obs <- seq(nmin, nmax, nstep)

to_export <- ls_funs() %>% map(get)
to_export <-
  c(to_export, list(
    n_obs = seq(nmin, nmax, nstep),
    n_sim = nsim
  ))
res_raw %<-% simulation_study(n_obs, n_sim,
                              furrr_options(
                                globals = to_export,
                                seed = seed,
                                packages = c("furrr", "lavaan", "tidyverse")
                              ))
invisible(res_raw)
readr::write_csv(res_raw, "results.csv")
readr::write_rds(res_raw, "results.csv")

if(fs::file_exists(hpc_config)){
  list(local = sessioninfo::session_info(),
       login_node = value(future(sessioninfo::session_info())),
       worker = value(future(value(future(sessioninfo::session_info())))))
} else {
  sessioninfo::session_info()
}
