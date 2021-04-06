source(here::here("R", "lavaan_helper.R"))
library(lavaan)
loadings_jones_paulhus <- c(38, 31, 40, 52, 59, 71, 62, 46, 51)/100
cohend_jones_paulhus <- c(24, 29, 35)/100
#----model-truth----
model_truth <- combine(
  measurement(
    "MACH",
    items("x", 9),
    loadings = round(loadings_jones_paulhus * 0.7, 2)
  ),
  intercepts("MACH", list(0, 0.5)),
  variances("MACH", list(1, 1))
)
#----models----
model_same <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(a,a)*1
  x1 ~ 0*1"

model_differ <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(a,b)*1
  x1 ~ 0*1"


#----generate----
n = 100 
data <- lavaan::simulateData(
  model_truth,
  sample.nobs = c(n/2, n/2), 
  standardized = TRUE)

#----analyze----
res_same <- cfa(model_same, data, group = "group", group.equal = c("loadings", "intercepts"))

summary(res_same)

res_differ <- cfa(model_differ, data, group = "group", group.equal = c("loadings", "intercepts"))

summary(res_differ)

fitMeasures(res_same, "BIC")
fitMeasures(res_differ, "BIC")

generate_data <- function(n_obs, n_sim, model_truth, model_1, model_2, ...){
  res_n <- list()
  for (i in 1:length(n_obs)){
    res <- c()
    for (j in 1:n_sim){
      data <- lavaan::simulateData(
        model_truth,
        sample.nobs = c(n_obs[i]/2, n_obs[i]/2), 
        standardized = TRUE)
      
      res_1 <- cfa(model_1, data, group = "group", ...)
      res_2 <- cfa(model_2, data, group = "group", ...)
      if(lavInspect(res_1, "converged") & lavInspect(res_2, "converged"))
        res[j] <- (fitMeasures(res_1, "BIC") - fitMeasures(res_2, "BIC")) > 0
      else
        res[j] <- NA
    }
    res_n[[i]] <- res
  }
  return(res_n)
}

n_obs = c(50, 100, 200, 300, 500, 1000)

res <- generate_data(
  n_obs, 
  100, 
  model_truth, 
  model_same, 
  model_differ, 
  group.equal = c("loadings", "intercepts"))

res_data <- data.frame(n = n_obs)

res_data$result = res
res_data$p <- purrr::map_dbl(res_data$result, mean, na.rm = TRUE)
res_data$n_conv <- purrr::map_dbl(res_data$result, ~sum(is.na(.x)))

res_data$n_conv

library(ggplot2)

ggplot(res_data, aes(x = n, y = p)) + geom_point() + geom_line() + 
  scale_x_continuous(breaks = n_obs)
  

#----some functions----
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
