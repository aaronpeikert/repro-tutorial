intercepts <- function(variables, intercepts){
  stopifnot(is.character(variables))
  stringr::str_c(variables, " ~ ", modifier(intercepts), "1")
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


