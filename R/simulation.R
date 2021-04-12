source(here::here("R", "lavaan_helper.R"))
source(here::here("R", "modind.R"))
library(tidyverse)
library(lavaan)
library(furrr)
# if you have access to a hpc envir specify this in R/hpc.R
# if not local multicore is used
hpc_config <- here::here("R", "hpc.R")
if(fs::file_exists(hpc_config)){
  source(hpc_config)
} else {
  plan(list(transparent,
       tweak(multisession, workers = 4L)))
}
# debug:
#   plan(transparent)

#----jones_paulhus----
loadings_jones_paulhus <- c(38, 31, 40, 52, 59, 71, 62, 46, 51)/100
cohend_jones_paulhus <- c(24, 29, 35)/100

#----model-truth----
model_truth <- combine(
  measurement(
    "MACH",
    items("x", 9),
    loadings = round(loadings_jones_paulhus * 0.7, 2)
  ),
  intercepts("MACH", list(0, 0.2)),
  variances("MACH", list(1, 1))
)

#----models----
model_same <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(0,0)*1
  MACH ~~ c(1, NA)*MACH"

model_differ <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(0, NA)*1 + c(zero, diff)*1
  MACH ~~ c(1, NA)*MACH"

#----generate----
n = 50000
data <- lavaan::simulateData(
  model_truth,
  sample.nobs = c(n/2, n/2), 
  standardized = TRUE)

#----analyze----
res_same <- cfa(
  model_same3, 
  data, group = "group", 
  group.equal = c("loadings", "intercepts"), 
  std.lv= TRUE)
summary(res_same)
res_differ <- cfa(
  model_differ3, 
  data, 
  group = "group", 
  group.equal = c("loadings", "intercepts"), 
  std.lv= TRUE)
summary(res_differ)

#----sim-functions----
competing_models <- function(data, same, differ, ...){
  list(same = cfa(same, data, ...),
       differ = cfa(differ, data, ...))
}

is_converged <- function(fit)lavInspect(fit, "converged")
all_converged <- function(fits)all(purrr::map_lgl(fits, is_converged))

get_fit <- function(fits, measure){
  stopifnot(length(fits) == 2L, length(measure) == 1L)
  if(!all_converged(fits))return(NA)
  diff <- fitMeasures(fits$differ, measure) - fitMeasures(fits$same, measure)
  as.numeric(diff)
}
get_bic <- function(fits)get_fit(fits, "BIC")
get_aic <- function(fits)get_fit(fits, "AIC")
get_lrt_p <- function(fits){
  if(!all_converged(fits))return(NA)
  lavaan::anova(fits$same, fits$differ)$`Pr(>Chisq)`[2]
}
get_parameter <- function(fits, what, label, how){
  if(!all_converged(fits))return(NA)
  pars <- do.call(how, list(fits$differ))
  which <- which(pars$label == label)
  if(length(which) == 0L)stop("No parameter labeled '", label, "'.")
  out <- pars[which, what]
  out
}

get_estimate <- function(fits, label = "diff"){
  get_parameter(fits, "est", label, how = parameterestimates)
}
get_delta_p <- function(fits, label = "diff"){
  get_parameter(fits, "pvalue", label, how = parameterestimates)
}
get_std_delta_p <- function(fits, label = "diff"){
  get_parameter(fits, "pvalue", label, how = standardizedSolution)
}
get_std_estimate <- function(fits, label = "diff"){
  get_parameter(fits, "est.std", label, how = standardizedSolution)
}
generate_data_ <- function(n_obs, truth, same, differ, extract_fns, ...){
  data <- lavaan::simulateData(
    truth,
    sample.nobs = rep(n_obs, 2), 
    standardized = TRUE)
  fits <- competing_models(data, same, differ, group = "group", ...)
  as_tibble(map(extract_fns, exec, fits))
}

generate_data <- function(n_sim, setup, .furrr_options = furrr_options()){
  without_id <- select(setup, -matches("^id\\d*$"))
  future_map(seq_len(n_sim),
             ~ mutate(
               select(setup, -extract_fns),
               results = future_pmap(without_id, generate_data_, .options = .furrr_options)
             ),
             .options = .furrr_options)
}

ls_funs <- function(pos = parent.frame(), ...){
  everything <- ls(pos = pos, ...)
  names(everything) <- everything
  out <- names(keep(map(everything, get), is.function))
  names(out) <- out
  out
}

#----sim----
extract_fns <- list(bic = get_bic, 
                    aic = get_aic,
                    lrt_p = get_lrt_p,
                    delta_p = get_delta_p,
                    estimate = get_estimate,
                    std_delta_p = get_std_delta_p,
                    std_estimate = get_std_estimate)
#quick test
#n_obs <- c(10000)

n_obs <- seq(100, 10000, 100)
setup <- expand_grid(n_obs = n_obs,
                     truth = model_truth,
                     same = model_same,
                     differ = model_differ,
                     extract_fns = list(extract_fns),
                     group.equal = list(c("loadings", "intercepts")),
                     auto.fix.first = FALSE)
n_sim <- 100
#quick test
#n_sim <- 2

#rm(res_raw)
to_export <- ls_funs() %>% map(get)
to_export <- c(list(setup = setup), to_export)
#local test
#plan(transparent)
res_raw %<-% 
  generate_data(n_sim,
                setup,
                furrr_options(
                  globals = to_export,
                  seed = TRUE,
                  packages = c("furrr", "lavaan", "tidyverse")
                ))
write_rds(res_raw, "res_raw.rds")

res <- res_raw  %>%
  map(bind_rows) %>% 
  bind_rows() %>% 
  select(n_obs, results) %>% 
  unnest(results)

res <- mutate(
  res,
  dec_bic = bic < 0,
  dec_aic = aic < 0,
  dec_aic2 = aic < -2,
  dec_lrt_p = lrt_p < 0.05,
  dec_delta_p = delta_p < 0.05)

interval <- function(x, alpha = c(0.05)){
  lower_alpha <- alpha/2
  upper_alpha <- 1 - lower_alpha
  tibble(alpha = alpha,
         lower = quantile(x, lower_alpha),
         upper = quantile(x, upper_alpha))
}

res_interval <- res %>%
  select(n_obs, std_estimate) %>% 
  filter(!is.na(std_estimate)) %>% 
  group_by(n_obs) %>% 
  summarise(interval = list(interval(std_estimate, seq(0.05, .2, 0.01))), 
            .groups = "drop") %>% 
  unnest(c(interval)) %>% 
  mutate(width = upper - lower)

plot_interval <- res_interval %>% 
  ggplot(aes(n_obs, ymin = lower, ymax = upper, fill = alpha, group = alpha)) + 
  geom_ribbon() +
  scale_fill_viridis_c() +
  scale_y_continuous(breaks = seq(-0.2, 0.7, .1)) +
  theme_minimal()

plot_interval
ggsave("interval.png")

plot_width <- res_interval %>% 
  ggplot(aes(n_obs, width, color = alpha, group = alpha)) +
  geom_line() +
  scale_color_viridis_c() +
  theme_minimal() +
  scale_y_log10(breaks = seq(0, 0.8, .1)) +
  theme(axis.text.x = element_text(angle = -90)) +
  NULL

plot_width
ggsave("width.png")
