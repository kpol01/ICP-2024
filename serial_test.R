serial_test <- function (u, d) 
{
  if (length(u)/d - as.integer(length(u)/d) > 0) 
    stop("the length of 'u' must be a multiple of d")
  pair <- matrix(floor(u * d), length(u)/2, 2)
  poly <- pair[, 1] * d + pair[, 2]
  obsnum <- sapply(0:(d^2 - 1), function(x) sum(poly == x))
  expnum <- length(u)/(2 * d^2)
  residu <- (obsnum - expnum)/sqrt(expnum)
  stat <- sum(residu^2)
  pvalue <- pchisq(stat, d^2 - 1, lower.tail = FALSE)
  options(digits = 2)
  
  res <- list(statistic = stat, df = d^2 - 1, p.value = pvalue, 
              observed = obsnum, expected = expnum, residuals = residu)
  return(res)
}

serial_test(runif(1000), d = 8)