freq_test <- function (u, d) 
{
  seq = 0:(d-1)
  integernum <- floor(u * length(seq) + min(seq))
  obsnum <- sapply(seq, function(x) sum(integernum == x))
  expnum <- length(u)/length(seq)
  residu <- (obsnum - expnum)/sqrt(expnum)
  stat <- sum(residu^2)
  pvalue <- pchisq(stat, length(seq) - 1, lower.tail = FALSE)
  options(digits = 2)
  df <- data.frame(1:length(obsnum), obsnum, expnum)
  colnames(df) <- c("Y_j", "Observed Freq", "Expected Freq")
  
  res <- list(statistic = stat, parameter = length(seq) - 1, 
              p.value = pvalue, df)
  return(res)
}

freq_test(runif(1000), 10)