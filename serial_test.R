rm(list = ls())

serial.test <- function(seq, d)
{
  if(length(seq) %% 2 == 1)
  {
    seq <- seq[1 : (n-1)]
  }
  n <- length(seq)
  M <- matrix(seq, ncol = 2, byrow = T)
  tup <- apply(M, MARGIN = 1, function(Vec) paste(Vec, collapse = ","))
  df <- as.data.frame(tup) 
  colnames(df) <- "tup"
  freq_tab <- as.data.frame(tup) %>% group_by(tup) %>%
        summarise(frequency = n()) %>%
       arrange(desc(frequency)) 
  freq <- freq_tab$frequency
  
  chi_sq <- sum((freq - rep(n/2, n/2)/d^2)^2) * 2 * d^2 /n
  return(chi_sq)
}

serial.test(sample(1:20, 100, replace = T), 20)
qchisq(0.95, 49)
