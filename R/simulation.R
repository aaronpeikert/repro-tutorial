source(here::here("R", "lavaan_helper.R"))
loadings_jones_paulhus <- c(38, 31, 40, 52, 59, 71, 62, 46, 51)/100
cohend_jones_paulhus <- c(24, 29, 35)/100
#----model-truth----
model_truth <- combine(
  measurement(
    "MACH",
    items("x", 9),
    loadings = round(loadings_jones_paulhus * 0.7, 2)
  ),
  intercepts("MACH", list(0, 0.1)),
  variances("MACH", list(1, 1))
)
#----model----


#----generate----
generate <- function(n, diff, vary_intercepts, vary_loadings, skewness, ...){
  latent <- "MACH"
  items <- items("x", 9)
  model <-
    combine(
      measurement(
        latent,
        items,
        varying_modifer(loadings_jones_paulhus, rel_change = vary_loadings)
      ),
      intercepts(latent, list(0, diff)),
      intercepts(items, varying_modifer(rep(0, 9), 3, abs_change = vary_intercepts)),
      variances(latent, list(1, 1))
    )
  skewness <- shuffle(pad_zero(skewness, length(items)))
  lavaan::simulateData(model, skewness = skewness, sample.nobs = c(n/2, n/2), standardized = TRUE, ...)
}
#----analyze----
analyze <- function(simulated){
  latent <- "MACH"
  items <- items("x", 9)
  model <-
    combine(
      measurement(
        latent,
        items
      ),
      intercepts(latent, list(0, "NA")),
      variances(latent, list(1, 1))
    )
  browser()
    lavaan::cfa(
      model,
      data = simulated,
      group = "group",
      meanstructure = TRUE,
      std.lv = TRUE,
      group.equal = c("intercepts", "loadings")
    )
}
#----summarise----
summarise <- function(fit){
  
}
