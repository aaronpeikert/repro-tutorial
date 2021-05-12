suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(lavaan))
suppressPackageStartupMessages(library(furrr))

source(here("R", "simulation_funs.R"))
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
# debug:
#   plan(transparent)

nmin <- 100
nmax <- 10000
nstep <- 100
n_sim <- 1000
seed <- 1234
n_obs <- seq(nmin, nmax, nstep)

options(scipen = 999)
message("This may take a while! You fit approximately ", n_sim*length(n_obs)*5*2, " SEMs.")

to_export <- ls_funs() %>% map(get)
to_export <-
  c(to_export, list(
    n_obs = seq(nmin, nmax, nstep),
    n_sim = n_sim
  ))
out_file <- here::here("data", "simulation_results.csv")
if(!fs::file_exists(out_file)){
  res_raw %<-% simulation_study(n_obs, n_sim,
                                furrr_options(
                                  globals = to_export,
                                  seed = seed,
                                  packages = c("furrr", "lavaan", "tidyverse"),
                                  scheduling = 10
                                ))
  invisible(res_raw)
  fs::dir_create("data")
  readr::write_csv(res_raw, here::here("data", "simulation_results.csv"))
  readr::write_rds(res_raw, here::here("data", "simulation_results.rds"))
}

if(fs::file_exists(hpc_config)){
  list(local = sessioninfo::session_info(),
       login_node = value(future(sessioninfo::session_info())),
       worker = value(future(value(future(sessioninfo::session_info())))))
} else {
  sessioninfo::session_info()
}
