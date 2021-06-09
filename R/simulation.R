with_lecuyer <- function(seed, code) {
  withr::with_seed(
    seed,
    code,
    .rng_kind = "L'Ecuyer-CMRG"
  )
}

generate_seeds <- function(k, seed) {
  with_lecuyer(
    seed,
    purrr::accumulate(seq_len(k), function(x, y) # discard y
      parallel::nextRNGStream(x), .init = .Random.seed)
  )
}

error <- function(i){
  as.numeric(scale(rnorm(i), scale = FALSE)) # only centered
}

simulate_data_ <- function(n, df, d, i){
  stopifnot(n %% 2L == 0L)
  group <- rep(c(0L, 1L), each = n/2)
  rand <-  matrix(rchisq(n * i, df/i), ncol = i) # sum of n chisq has df of n*df
  effect <- rand + d * group
  colnames(effect) <- stringr::str_c("x", seq_len(i))
  dplyr::mutate(tibble::as_tibble(effect),
                group = group)
}

simulate_data <- function(seed, n, df, d, i = 9, ...){
  stopifnot(length(list(...)) == 0L)
  with_lecuyer(seed, simulate_data_(n, df, d, i))
}

planned_analysis <- function(data){
  x <- rowMeans(dplyr::select(data, dplyr::starts_with("x")))
  y <- as.factor(data$group)
  skew <- moments::skewness(x)
  # skewness cutoff
  use_rank <- abs(skew) > 1
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

simulation_study <- function(setup){
  all_steps <- purrr::compose(extract_results, planned_analysis, simulate_data)
  out <- dplyr::mutate(setup, results = purrr::pmap(setup, all_steps))
  #tidyr::unnest(out, results)
  out
}

setup <- tidyr::expand_grid(
  seed = generate_seeds(10, 1002),
  n = seq(10, 1000, 10),
  df = 8, # skew = sqrt(8/df)
  d = seq(0, .5, 0.1)
)

res <- simulation_study(setup)

test <- res %>% 
  select(-seed) %>% 
  group_by(across(-results)) %>% 
  unnest_wider(results) %>% 
  summarise(power = mean(p_value < 0.05), .groups = "drop")
