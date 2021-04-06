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
  intercepts("MACH", list(0, 0.2)),
  variances("MACH", list(1, 1))
)
#----models----
model_same <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(0,0)*1
  MACH ~~ 1*MACH"

model_differ <- 
  "MACH =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
  MACH ~ c(0,NA)*1
  MACH ~~ 1*MACH"


#----generate----
n = 500
data <- lavaan::simulateData(
  model_truth,
  sample.nobs = c(n/2, n/2), 
  standardized = TRUE)

#----analyze----
res_same <- cfa(
  model_same, 
  data, group = "group", 
  group.equal = c("loadings", "intercepts"), 
  std.lv= TRUE)

summary(res_same)

res_differ <- cfa(
  model_differ, 
  data, 
  group = "group", 
  group.equal = c("loadings", "intercepts"), 
  std.lv= TRUE)

summary(res_differ)

fitMeasures(res_same, "BIC")
fitMeasures(res_differ, "BIC")

lavaan::anova(res_same, res_differ)$`Pr(>Chisq)`[2]

dplyr::filter(parameterEstimates(res_differ), lhs == "MACH", op == "~1", group == 2)$est
dplyr::filter(parameterEstimates(res_differ), lhs == "MACH", op == "~1", group == 2)$se
dplyr::filter(parameterEstimates(res_differ), lhs == "MACH", op == "~1", group == 2)$pvalue


generate_data <- function(n_obs, n_sim, model_truth, model_1, model_2, ...){
  bic_n <- list()
  aic_n <- list()
  p_aov_n <- list()
  se_n <- list()
  est_n <- list()
  p_delta_n <- list()
  for (i in 1:length(n_obs)){
    bic <- c()
    aic <- c()
    p_aov <- c()
    se <- c()
    est <- c()
    p_delta <- c()
    for (j in 1:n_sim){
      data <- lavaan::simulateData(
        model_truth,
        sample.nobs = c(n_obs[i], n_obs[i]), 
        standardized = TRUE)
      
      res_1 <- cfa(model_1, data, group = "group", ...)
      res_2 <- cfa(model_2, data, group = "group", ...)
      if(lavInspect(res_1, "converged") & lavInspect(res_2, "converged")){
        bic[j] <- fitMeasures(res_1, "BIC") - fitMeasures(res_2, "BIC")
        aic[j] <- fitMeasures(res_1, "AIC") - fitMeasures(res_2, "AIC")
        p_aov[j] <- lavaan::anova(res_1, res_2)$`Pr(>Chisq)`[2]
        
        parrow <-dplyr::filter(parameterEstimates(res_2), lhs == "MACH", op == "~1", group == 2)
        est[j] <- parrow$est
        se[j] <- parrow$se
        p_delta[j] <- parrow$pvalue
      }else{
        bic[j] <- NA
        aic[j] <- NA
        est[j] <- NA
        p_aov[j] <- NA
        se[j] <- NA
        p_delta[j] <- NA
      }
    }
    bic_n[[i]] <- bic
    aic_n[[i]] <- aic
    p_aov_n[[i]] <- p_aov
    se_n[[i]] <- se
    est_n[[i]] <- est
    p_delta_n[[i]] <- p_delta
  }
  return(dplyr::tibble(
    n = n_obs, 
    bic = bic_n, 
    aic = aic_n, 
    p_aov = p_aov_n, 
    se = se_n,
    est = est_n))
}

n_obs = c(25, 50, 100, 200, 300, 500)

res <- generate_data(
  n_obs, 
  20, 
  model_truth, 
  model_same, 
  model_differ, 
  group.equal = c("loadings", "intercepts"),
  std.lv = TRUE)

res_data <- tidyr::unnest(res, c(bic, aic, p_aov, se, est))

library(tidyverse) #reminder for the good old days with M.Z.

res_data <- mutate(
  res_data,
  dec_bic = bic > 0,
  dec_aic = aic > 0,
  dec_p = p_aov < 0.05)

res_data %>% group_by(n) %>% 
  summarise(bic = mean(dec_bic, na.rm = T),
            aic = mean(dec_aic, na.rm = T),
            p_aov = mean(dec_p, na.rm = T)) %>% 
  pivot_longer(c(bic, aic, p_aov), names_to = "metric", values_to = "power") %>% 
  ggplot(aes(x = n, y = power, color = metric)) + geom_point() + geom_line() +
  theme_minimal() + scale_x_continuous(breaks = n_obs)


res_data %>% ggplot(aes(x = est, color = factor(n))) + geom_density() +
  theme_minimal()

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
