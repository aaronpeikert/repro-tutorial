simulate_data <- function(n, df, d, i){
  stopifnot(n %% 2L == 0L)
  group <- rep(c(0L, 1L), each = n/2)
  rand <-  matrix(rchisq(n * i, df/i), ncol = i) # sum of n chisq has df of n*df
  d_scaled <- d * sqrt(2*df)/i # scale d to express sd units (var(chisq) = 2df)
  #browser()
  effect <- rand + d_scaled * group
  colnames(effect) <- paste0("x", seq_len(i))
  effect <- as.data.frame(effect)
  effect$group <- group
  return(effect)
}

planned_analysis <- function(data, use_rank = "skew"){
  x <- rowMeans(data["group" != names(data)])
  y <- as.factor(data$group)
  skew <- moments::skewness(x)
  # skewness cutoff
  if(use_rank == "skew")use_rank <- abs(skew) > 1
  if(use_rank){
    x <- rank(x)
  }
  test <- t.test(x ~ y)
  list(test = test, skew = skew, use_rank = use_rank, n = length(x))
}

extract_results <- function(analysis){
  list(p_value = analysis$test$p.value,
       skew = analysis$skew)
}

t2d <- function(t, n1, n2)t * sqrt(((n1 + n2)/(n1 * n2)) * (n1 + n2)/(n1 + n2 -2))
parameter_recovery <- function(n, df, d, i, rank = FALSE){
  t2d(planned_analysis(simulate_data(n, df, d, i), rank)$test$statistic, n/2, n/2)
}

test <- simulate_data(1000000, 100, 0.1, 10)
m1 <- filter(test, group == 1) %>% 
  select(-group )%>% 
  rowSums() %>% 
  mean()
m0 <- filter(test, group == 0) %>% 
  select(-group )%>% 
  rowSums() %>% 
  mean()
sd <- test %>% rowSums() %>% sd()

(m1 - m0)/sd

t2d(planned_analysis(test)$test$statistic, 1000000/2, 1000000/2)
parameter_recovery(1000000, 100, 0.1, 10)

simulation_study <- function(setup){
  all_steps <- purrr::compose(extract_results, planned_analysis, simulate_data)
  out <- dplyr::mutate(setup, results = purrr::pmap(setup, all_steps))
  #tidyr::unnest(out, results)
  out
}

setup <- tidyr::expand_grid(
  n = seq(10, 1000, 10),
  df = 8, # skew = sqrt(8/df)
  d = seq(0, .5, 0.1),
  i = 10)

res <- simulation_study(setup)

test <- res %>% 
  group_by(across(-results)) %>% 
  unnest_wider(results) %>% 
  summarise(power = mean(p_value < 0.05), .groups = "drop")
