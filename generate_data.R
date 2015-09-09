s_lambda <- 1.2

#sample inflation factor
inf_p <- 0.1

size <- 100


generate_inflated <- function(lambda, inf_p, size) {
  orig_data <- rpois(size, lambda)
  inf_data <- orig_data * rbinom(size, 1, prob = 1 - inf_p)
  data.frame(orig = orig_data, inf = inf_data)
}

dpcr_dat <- generate_inflated(1.1, 0.1, 765)
table(dpcr_dat[["orig"]] > 0)
table(dpcr_dat[["inf"]] > 0)
