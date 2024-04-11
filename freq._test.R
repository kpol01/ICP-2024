freq_test <- function (u, d) 
{
  seq = 0:(d-1)
  integernum <- floor(u * d)
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

sampling.dep <- function(n)
{
  sam <- 0
  sam[1] <- runif(1)
  for(i in (2:n))
  {
    if(i %% 2 == 0){
    sam[i] <- runif(1, 0, sam[i-1])}
    else{
      sam[i] <- runif(1, sam[i-1], 1)
    }
  }
  return(sam)
}

sampling.dep(1000)

freq_test(sampling.dep(1000), 10)
