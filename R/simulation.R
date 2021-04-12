source(here::here("R", "lavaan_helper.R"))

#----sim-functions----
competing_models <- function(data, same, differ, ...){
  list(same = cfa(same, data, ...),
       differ = cfa(differ, data, ...))
}

filter_dots <- function(..., fun, what = names(formals(fun))){
  dots <- list(...)
  if(any(names(dots) == ""))stop("filter_dots does not allow unnamed args!")
  exists <- setdiff(what, setdiff(what, names(dots)))
  dots[exists]
}

safe_cfa <- function(...){
  valid_args <-
    c(names(formals(lavaan::lavaanify)),
      names(formals(lavaan::lavaan)),
      names(lavOptions()))
  dots <- filter_dots(..., fun = lavaan::cfa, what = valid_args)
  do.call("cfa", dots, envir = getNamespace("lavaan"))
}

competing_models_mod <- function(data, same, differ, mod_load = 0L, mod_int = 0L, pars = "", ...){
  res_mod <- set_free_mod(differ, data, pars, op = "=~", depth = mod_load, ...)
  res_mod <- set_free_mod(differ, data, res_mod$pars, op = "~1", depth = mod_int, ...)
  fits <- list(same = safe_cfa(model = same, data = data, group.partial = res_mod$pars, ...),
               differ = res_mod$differ)
  
}

set_free_mod <- function(differ, data, pars, depth = 3, op = c("~1", "=~"), ...){
  differ_fit <- safe_cfa(model = differ, data = data, group.partial = pars, ...)
  depth <- depth - 1
  if(depth < 0){return(list(pars = pars, differ = differ_fit))}
  
  parest <- parameterEstimates(differ_fit)
  testscores <- lavTestScore(differ_fit)
  sorted_pars <- testscores$uni %>% 
    rename(label = lhs) %>% 
    select(-c(op, rhs, df)) %>% 
    left_join(parest, by = c("label")) %>% 
    filter(group == 1, .data$op %in% !!op) %>% 
    arrange(desc(X2))
  sorted_pars <- sorted_pars[1,]
  
  if(pull(sorted_pars, X2) > qchisq(0.95, 1)){
    pars <- append(pars, stitch(sorted_pars))
    set_free_mod(differ, data, pars, depth, op = op, ...)
  }else{
    return(list(pars = pars, differ = differ_fit))
  }
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
generate_data_setup_ <- function(n_obs, truth, same, differ, extract_fns, ...){
  data <- lavaan::simulateData(
    truth,
    sample.nobs = rep(n_obs, 2), 
    standardized = TRUE)
  fits <- competing_models_mod(data, same, differ, group = "group", ...)
  as_tibble(map(extract_fns, exec, fits))
}

generate_data_setup <- function(setup, .furrr_options = furrr_options()){
  .furrr_options$seed <- TRUE
  mutate(select(setup, -extract_fns),
    results = future_pmap(setup, generate_data_setup_, .options = .furrr_options))
}

generate_data <- function(n_sim, setup, .furrr_options = furrr_options()){
  future_map(seq_len(n_sim),
             ~generate_data_setup(setup, .furrr_options = .furrr_options),
             .options = .furrr_options)
}

ls_funs <- function(pos = parent.frame(), ...){
  everything <- ls(pos = pos, ...)
  names(everything) <- everything
  out <- names(keep(map(everything, get), is.function))
  names(out) <- out
  out
}

interval <- function(x, alpha = c(0.05)){
  lower_alpha <- alpha/2
  upper_alpha <- 1 - lower_alpha
  tibble(alpha = alpha,
         lower = quantile(x, lower_alpha),
         upper = quantile(x, upper_alpha))
}
