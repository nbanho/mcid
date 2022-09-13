#' Incubation period
#' - assume based on meta-analysis from McAloon et al
#' - https://bmjopen.bmj.com/content/bmjopen/10/8/e039652.full.pdf
#' - lognormal distribution
#' - log mu: 1.63 (1.51, 1.75)
#' - log sigma: 0.50 (0.45, 0.55)
#' 


p_in_mu_m <- 1.63
p_in_mu_s <- (1.75 - 1.51) / (2 * qnorm(0.975))
p_in_sigma_m <- 0.50
p_in_sigma_s <- (0.55 - 0.45) / qnorm(0.975)

p_in <- function(x, log_mu = 1.63, log_sigma = 0.50) { 
  plnorm(x, log_mu, log_sigma)
}

# Vectorize delay function (e.g. p_g)
vp <- function(xT, from0, FUN, ...) {
  x <- seq(0,xT)
  y <- length(x)
  if (from0) {
    y[1] <- FUN(0.5, ...)
    y[2] <- FUN(1.5, ...) - FUN(0.5, ...)
  } else {
    y[1] <- 0
    y[2] <- FUN(1.5, ...)
  }
  for (i in 3:length(x)) {
    y[i] = FUN(x[i] + 0.5, ...) - FUN(x[i] - 0.5, ...)
  }
  return(y)
}
