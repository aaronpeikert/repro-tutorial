source(here::here("R", "lavaan_helper.R"))
library(tidyverse)
library(lavaan)

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
# MACH ~~ 1*MACH
# mean(res_data$dec_p)
# MACH ~~ c(1, NA)*MACH
# [1] 0.466
# mean(res_data$dec_p)
# [1] 0.464

model_differ <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(0, NA)*1 + c(zero, diff)*1
  MACH ~~ c(1, NA)*MACH"


#----generate----
n = 5000
data <- lavaan::simulateData(
  model_truth,
  sample.nobs = c(n/2, n/2), 
  standardized = TRUE)

#----analyze----
res_same <- cfa(
  model_same, 
  data, group = "group", 
  group.equal = c("loadings", "intercepts"), 
  std.lv= TRUE)
summary(res_same)
res_differ <- cfa(
  model_differ, 
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
get_parameter <- function(fits, what, label){
  if(!all_converged(fits))return(NA)
  pars <- parameterestimates(fits$differ)
  which <- which(pars$label == label)
  if(length(which) == 0L)stop("No parameter labeled '", label, "'.")
  pars[which, what]
}
get_estimate <- function(fits, label = "diff")get_parameter(fits, "est", label)
get_delta_p <- function(fits, label = "diff")get_parameter(fits, "pvalue", label)

generate_data_ <- function(n_obs, truth, same, differ, extract_fns, ...){
  data <- lavaan::simulateData(
    truth,
    sample.nobs = rep(n_obs, 2), 
    standardized = TRUE)
  fits <- competing_models(data, same, differ, group = "group", ...)
  as_tibble(map(extract_fns, exec, fits))
}

generate_data <- function(n_sim, setup){
  map(seq_len(n_sim), ~mutate(setup, results = pmap(setup, generate_data_)))
}

#----sim----
extract_fns <- list(bic = get_bic, 
                    aic = get_aic,
                    lrt_p = get_lrt_p,
                    delta_p = get_delta_p,
                    estimate = get_estimate)
n_obs <- 1:5*100
setup <- expand_grid(n_obs = n_obs,
                     truth = model_truth,
                     same = model_same,
                     differ = model_differ,
                     extract_fns = list(extract_fns),
                     group.equal = list(c("loadings", "intercepts")), 
                     std.lv = TRUE)
res_raw <- generate_data(1000, setup)
res <- res_raw %>% 
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

res %>%
  group_by(n_obs) %>% 
  summarise(across(starts_with("dec"), mean, na.rm = TRUE)) %>% 
  pivot_longer(c(-n_obs), names_to = "metric", values_to = "power") %>% 
  ggplot(aes(x = n_obs, y = power, color = metric)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = n_obs)

res %>%
  ggplot(aes(x = estimate, color = factor(n_obs))) +
  geom_density() +
  theme_minimal()
