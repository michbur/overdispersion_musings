#density ZIP
dzipois <- function(x, lambda, p, log = FALSE) {
  ifelse(x == 0, 1 - p + p * exp(-lambda), p * (lambda^x) * exp(-lambda)/(factorial(x)))
}


dzipois(5, 2, 1)
dpois(5, 2)

library(ggplot2)
library(reshape2)
library(dplyr)

compare_dens <- function(lambdas = c(0.1, 0.5, 1, 2), quantiles = 0L:10, ps = 0:4/4) {
  melt(do.call(rbind, lapply(lambdas, function(single_lambda) {
    df_dens <- data.frame(factor(rep(single_lambda, length(quantiles))), 
                          factor(quantiles), 
                          sapply(ps, function(single_p)
                            dzipois(quantiles, single_lambda, single_p)))
    
    colnames(df_dens) <- c("lambda", "quant", ps)
    df_dens
  })), variable.name = "p")
}

mdf_dens <- compare_dens(lambdas = c(0.01, 0.1, 0.5, 1, 2),
                         quantiles = 0L:10,
                         ps = 0:4/2)
ggplot(mdf_dens, aes(x = quant, y = value, fill = p)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ lambda, nrow = 1)


#probability function from Wang 2012
#p overdispersed/underdispersed Poisson
#if p > 1, underdispersion
doupois <- function(x, lambda, p) {
  if(p > (1 - exp(-lambda))^-1)
    stop("Probability distribution no longer valid.")
  sapply(x, function(ith_x) {
    if(ith_x == 0) {
      (1 - p) + p*exp(-lambda)
    } else {
      p * ((lambda^ith_x) * exp(-lambda))/(factorial(ith_x))
    }
  })
}
# Example:
#doupois(5, 1, 1)
#dpois(5, 1)


k_vector <- 0L:7
p_vector <- seq(0.70, 1.15, by = 0.15)
lambda_vector <- 1L:8/5
res <- do.call(rbind, lapply(lambda_vector, function(single_lambda) {
  dens <- sapply(p_vector, function(single_p)
    doupois(k_vector, single_lambda, single_p))
  mdens <- melt(dens, varnames = c("k", "p"))
  mdens[["p"]] <- factor(mdens[["p"]], labels = p_vector)
  mdens[["k"]] <- factor(mdens[["k"]], labels = k_vector)
  data.frame(lambda = single_lambda, mdens)
})) %>% mutate(lambda_fac = as.factor(paste0("lambda = ", lambda)))
library(ggplot2)


ggplot(res, aes(x = k, fill = p, y = value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ lambda_fac, ncol = 2)

bias_res <- mutate(res, expected = as.numeric(as.character(k)) * value) %>% 
  group_by(p, lambda) %>%
  summarize(est_mean = (sum(expected))) %>%
  mutate(relbias = (est_mean - lambda)/lambda,
         bias = est_mean - lambda)


ggplot(bias_res, aes(x = as.factor(lambda), fill = p, y = relbias)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(expression(lambda)) +
  scale_y_continuous("Relative bias")
