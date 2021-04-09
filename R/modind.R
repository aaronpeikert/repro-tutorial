stitch <- function(pars){
  with(pars, str_c(lhs, op, rhs))
}

competing_models_mod <- function(data, same, differ, ...){
  pars <- c()
  res_mod <- set_free_mod(differ, data, pars, op = "=~", ...)
  res_mod <- set_free_mod(differ, data, res_mod$pars, op = "~1", ...)
  
  fits <- list(same = cfa(same, data, group.partial = res_mod$pars, ...),
               differ = res_mod$differ)
  
}

set_free_mod <- function(differ, data, pars, depth = 3, op = c("~1", "=~"), ...){
  
  differ_fit <- cfa(differ, data, group.partial = pars, ...)
  
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

res <- competing_models_mod(data, model_same3, model_differ3, 
                            group.equal = c("loadings", "intercepts"),
                            auto.fix.first = FALSE,
                            std.lv = TRUE,
                            group = "group")
