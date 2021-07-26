#----simulate_data----
simulate_data <- function(n, df, d, i){
  stopifnot(n %% 2L == 0L)
  gender <- rep(c(0L, 1L), each = n/2)
  rand <-  matrix(rchisq(n * i, df/i), ncol = i) # sum of n chisq has df of n*df
  d_scaled <- d * sqrt(2*df)/i # scale d to express sd units (var(chisq) = 2df)
  effect <- rand + d_scaled * gender
  colnames(effect) <- paste0("mach", seq_len(i))
  effect <- as.data.frame(effect)
  effect$gender <- factor(gender, levels = c(1L, 0L), labels = c("male", "female"))
  return(effect)
}

#----planned_analysis----
planned_analysis <- function(data, use_rank = "skew", skew_cutoff = 1){
  machiavellianism <- rowMeans(data["gender" != names(data)], na.rm = TRUE)
  data <- data[!is.na(machiavellianism),]
  machiavellianism <- machiavellianism[!is.na(machiavellianism)]
  gender <- as.factor(data$gender)
  skew <- moments::skewness(machiavellianism)
  # skewness cutoff
  if(use_rank == "skew")use_rank <- abs(skew) > skew_cutoff
  if(use_rank){
    machiavellianism <- rank(machiavellianism)
  }
  test <- t.test(machiavellianism ~ gender)
  list(test = test, skew = skew, use_rank = use_rank, n = length(gender))
}

#----report_analysis----
report_analysis <- function(analysis, cat = TRUE) {
  params <- report::report_parameters(analysis$test)
  table <- attributes(params)$table
  model <- report::report_model(analysis$test, table = table)
  if (analysis$use_rank)
    model <-
    stringr::str_replace(model,
                fixed("Welch Two Sample t-test"),
                "Mann--Whitney--Wilcoxon test")
  out <- stringr::str_c("The ", model, " suggests that the effect is ", params, sep = "")
  if(cat){
    cat(out)
    return(invisible(out))
  } else {
    return(out)
  }
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
