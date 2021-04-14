source(here::here("R", "standardizedSolution.R"))

stitch <- function(pars){
  with(pars, str_c(lhs, op, rhs))
}

items <- function(item, n){
  stringr::str_c(item, seq_len(n))
}

intercepts <- function(variables, intercepts){
  stopifnot(is.character(variables))
  stringr::str_c(variables, " ~ ", modifier(intercepts), "1", collapse = "\n")
}

modifier <- function(values){
  if (missing(values)) {
    values <- ""
  } else if (is.list(values)) {
    values <- do.call(stringr::str_c, c(values, sep = ", "))
    values <- stringr::str_c("c(", values, ")*")
  } else {
    values <- stringr::str_c(values, "*")
  }
  return(values)
}

measurement <- function(latent, variables, loadings){
stopifnot(is.character(latent),
            is.character(variables),
            length(latent) == 1)
  loadings <- modifier(loadings)
  variables <- stringr::str_c(loadings, variables, collapse = " + ")
  stringr::str_c(latent, " =~ ", variables)
}

varying_modifer <- function(modifier, n, rel_change, abs_change){
  if(missing(n))n <- ifelse(missing(abs_change), length(rel_change), length(abs_change))
  pos <- sample(seq_along(modifier), n)
  varied <- modifier
  if(missing(abs_change)){
    varied[pos] <- modifier[pos] * (1 + rel_change)
    return(list(modifier, varied))
  } else {
    varied[pos] <- modifier[pos] + abs_change
    return(list(modifier, varied))
  }
}

covariances <- function(x, y, covariances){
  stopifnot(is.character(x), is.character(y))
  stringr::str_c(x, " ~~ ", modifier(covariances), y, collapse = "\n")
}

variances <- function(x, variances)covariances(x, x, covariances = variances)

combine <- function(...)stringr::str_c(..., sep = "\n", collapse = "\n")
shuffle <- function(x)sample(x, length(x))
pad <- function(x, replace, length)c(x, rep(replace, length))
pad_zero <- function(x, length)pad(x, 0, length)
