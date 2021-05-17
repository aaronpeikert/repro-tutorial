source(here::here("R", "standardizedSolution.R"))

#----sim-functions----
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
      names(lavaan::lavOptions()))
  dots <- filter_dots(..., fun = lavaan::cfa, what = valid_args)
  do.call("cfa", dots, envir = getNamespace("lavaan"))
}

stitch <- function(pars){
  with(pars, str_c(lhs, op, rhs))
}

partial_measurement_invariance <- function(data, same, differ, mod_load = 0L, mod_int = 0L, pars = "", ...){
  res_mod <- set_free_mod(differ, data, pars, op = "=~", depth = mod_load, ...)
  res_mod <- set_free_mod(differ, data, res_mod$pars, op = "~1", depth = mod_int, ...)
  fits <- list(same = safe_cfa(model = same, data = data, group.partial = res_mod$pars, ...),
               differ = res_mod$differ)
  
}

set_free_mod <- function(differ, data, pars, depth = 3, op = c("~1", "=~"), ...){
  differ_fit <- safe_cfa(model = differ, data = data, group.partial = pars, ...)
  depth <- depth - 1
  if(depth < 0){return(list(pars = pars, differ = differ_fit))}
  if(!is_converged(differ_fit)){return(list(pars = pars, differ = differ_fit))}
  parest <- lavaan::parameterEstimates(differ_fit)
  testscores <- lavaan::lavTestScore(differ_fit)
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

is_converged_ <- function(fit)lavaan::lavInspect(fit, "converged")
is_converged <- function(fit){
  if(inherits(fit, "lavaan"))return(is_converged_(fit))
  else return(FALSE)
}
all_converged <- function(fits)all(purrr::map_lgl(fits, is_converged))

get_fit <- function(fits, measure){
  stopifnot(length(fits) == 2L, length(measure) == 1L)
  if(!is_converged(fits$differ))return(NA)
  diff <- lavaan::fitMeasures(fits$differ, measure)
  as.numeric(diff)
}

get_rmsea <- function(fits)get_fit(fits, "rmsea")
get_cfi <- function(fits)get_fit(fits, "cfi")
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

get_std_estimate <- function(fits, label = "diff"){
  get_parameter(fits, "est.std", label, how = standardizedSolution)
}

#----generate_data----
generate_data <- function(n_obs){
  truth <- "MACH =~ 0.27*x1 + 0.22*x2 + 0.28*x3 + 0.36*x4 + 0.41*x5 + 0.5*x6 +
  0.43*x7 + 0.32*x8 + 0.36*x9
  MACH ~ c(0, 0.2)*1
  MACH ~~ c(1, 1)*MACH"
  lavaan::simulateData(
    truth,
    sample.nobs = rep(n_obs, 2), 
    standardized = TRUE)
}

#----planed_analysis----
planed_analysis <- function(data){
  # force loadings to be freely estimated
  model <- "MACH =~  c(NA, NA)*x1 + c(NA, NA)*x2 + c(NA, NA)*x3 + c(NA, NA)*x4 +
  c(NA, NA)*x5 + c(NA, NA)*x6 + c(NA, NA)*x7 + c(NA, NA)*x8 + c(NA, NA)*x9
  # model is identified via latent variance set to 1
  MACH ~~ c(1, NA)*MACH"
  # intercept is set to zero
  same <- paste0(model, "
  MACH ~ c(0,0)*1")
  # intercept is free in one group
  differ <-  paste0(model, "
  MACH ~ c(0, NA)*1 + c(zero, diff)*1")
  # fit strong measurement invariance, then free <= 3 loadings and/or intercepts
  partial_measurement_invariance(
    data,
    same,
    differ,
    mod_load = 3L,
    mod_int = 3L,
    group = "group",
    group.equal = c("loadings", "intercepts")
  )
}

#----extract_results----
extract_results <- function(fits) {
  # extract_fns are helper that extract variables of interest, like p value etc.
  extract_fns <-
    list(
      std_estimate = get_std_estimate,
      lrt_p = get_lrt_p,
      rmsea = get_rmsea,
      cfi = get_cfi
    )
  # we use purrr::safely so that the simulation continues
  # even when some models fail
  save_fns <- map(extract_fns, purrr::safely, otherwise = NA)
  results <- map(save_fns, exec, fits)
  as_tibble(map(results, "result"))
}

#----simulation_study_funs----
simulation <- function(n_obs){
  fits <- purrr::safely(planed_analysis)(generate_data(n_obs))$result
  extract_results(fits)
}

simulation_study_ <-
  function(n_obs, .options = furrr::furrr_options()) {
    .options$seed <- TRUE
    names(n_obs) <- n_obs
    # repeat simulation for different n
    future_map_dfr(n_obs,
                   ~ simulation(.x),
                   .id = "n_obs",
                   .options = .options)
  }

#----simulation_study----
simulation_study <- function(n_obs, n_sim, .options = furrr::furrr_options()){
  # `map` is equivalent to lapply or for-loop
  # `future` is a package for parallelization, see High Performance Computing
  # repeat simulation n_sim times
  future_map_dfr(
    seq_len(n_sim),
    # repeat simulation for different n
    ~ simulation_study_(n_obs, .options = .options),
    .options = .options
  )
}


#----other-funs----
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
