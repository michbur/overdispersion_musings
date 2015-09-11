#density ZIP
dzipois <- function(x, lambda, p, log = FALSE) {
  ifelse(x == 0, 1 - p + p * exp(-lambda), p * (lambda^x) * exp(-lambda)/(factorial(x)))
}


dzipois(5, 2, 1)
dpois(5, 2)

library(ggplot2)
library(reshape2)

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
