stitch <- function(pars){
  with(pars, str_c(lhs, op, rhs))
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
