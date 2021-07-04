#----simulate_data----
simulate_data <- function(n, df, d, i){
  stopifnot(n %% 2L == 0L)
  group <- rep(c(1L, 0L), each = n/2)
  rand <-  matrix(rchisq(n * i, df/i), ncol = i) # sum of n chisq has df of n*df
  d_scaled <- d * sqrt(2*df)/i # scale d to express sd units (var(chisq) = 2df)
  #browser()
  effect <- rand + d_scaled * group
  colnames(effect) <- paste0("x", seq_len(i))
  effect <- as.data.frame(effect)
  effect$group <- group
  return(effect)
}
#----planned_analysis----
planned_analysis <- function(data, use_rank = "skew", skew_cutoff = 1){
  y <- rowMeans(data["group" != names(data)])
  x <- as.factor(data$group)
  skew <- moments::skewness(y)
  # skewness cutoff
  if(use_rank == "skew")use_rank <- abs(skew) > skew_cutoff
  if(use_rank){
    y <- rank(y)
  }
  test <- t.test(y ~ x)
  list(test = test, skew = skew, use_rank = use_rank, n = length(y))
}
#----extract_funs----
#t2d <- function(t, n1, n2)t * sqrt(((n1 + n2)/(n1 * n2)) * (n1 + n2)/(n1 + n2 -2))
t2d <- function(test){
  unname(2 * test$statistic/sqrt(test$parameter))
}
parameter_recovery <- function(n, df, d, i, rank = FALSE){
  t2d(planned_analysis(simulate_data(n, df, d, i), rank)$test$statistic, n/2, n/2)
}

extract_results <- function(analysis){
  list(cohend = t2d(analysis$test),
       p_value = analysis$test$p.value,
       skew = analysis$skew)
}

#----simulation_study----
simulation_study_ <- function(setup){
  all_steps <- purrr::compose(extract_results, planned_analysis, simulate_data)
  out <- dplyr::mutate(setup, results = furrr::future_pmap(setup, all_steps, .options = furrr::furrr_options(seed = TRUE)))
  #tidyr::unnest(out, results)
  out
}

simulation_study <- function(setup, k, seed = NULL){
  furrr::future_map_dfr(seq_len(k), function(irrelevant)simulation_study_(setup), .options = furrr::furrr_options(seed = seed))
}
