gap_test <- function (u, lower, upper) 
{
  gap <- (!((u <= upper) & (u >= lower))) * 1
  n <- length(u)
  p <- upper - lower
  indexzero <- (gap == 1) * 1:n
  indexzero <- indexzero[indexzero != 0]
  indexzero <- c(0, indexzero, n + 1)
  lindzero <- length(indexzero)
  lengthsize <- indexzero[2:lindzero] - indexzero[2:lindzero - 
                                                    1] - 1
  lengthsize <- lengthsize[lengthsize != 0]
  maxlen <- max(lengthsize)
  maxlen <- max(maxlen, floor((log(10^(-1)) - 2 * log(1 - p) - 
                                 log(n))/log(p)))
  obsnum <- sapply(1:maxlen, function(t) sum(lengthsize == 
                                               t))
  expnum <- (1 - p)^2 * p^(1:maxlen) * n
  residu <- (obsnum - expnum)/sqrt(expnum)
  stat <- sum(residu^2)
  pvalue <- pchisq(stat, maxlen - 1, lower.tail = FALSE)
  options(digits = 2)
  
  df <- data.frame(1:length(obsnum), obsnum, expnum)
  colnames(df) <- c("Gap Length", "Observed Freq", "Expected Freq")
  res <- list(statistic = stat, parameter = maxlen - 1, p.value = pvalue, 
              df)
  return(res)
}

gap_test(runif(1000), 0.3, 0.7)
gap.test(runif(1000), 0.1, 0.9)


library(DiagrammeR)
graph <- "
digraph flowchart {

  # Node definitions
  Initialize [shape=box, label=\"Initialize\", color=\"blue\"];
  rtozero [shape=box, label=\"Set r to zero\", color=\"blue\"];
  checking [shape=ellipse, label=\"G3: a <= U<sub>j <= b\", color=\"orange\"];
  increaser [shape=ellipse, label=\"Increase r\", color=\"orange\"]
  recordlength [shape=ellipse, label=\"record the gap length\", color=\"orange\"]
  numberofgaps [shape=ellipse, label=\"n gaps found?\", color=\"orange\"]
  
  
  
  
  # Edges to indicate the flow
  Initialize -> rtozero
  rtozero -> checking
  checking -> increaser [label = \"No\"]
  increaser -> checking
  checking -> recordlength [label = \"Yes\"]
  recordlength -> numberofgaps
  numberofgaps -> rtozero [label = \"No\"]
  numberofgaps -> Initialize [label=\"Yes\"]
  

}

"

grViz(graph)

