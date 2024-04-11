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

freq_test(runif(1000), 100)

sampling.dep <- function(n)
{
  sam <- 0
  sam[1] <- runif(1)
  for(i in (2:n))
  {
    if(i %% 2 == 0){
    sam[i] <- runif(1, 0, 0.25)}
    else{
      sam[i] <- runif(1, 0.25, 1)
    }
  }
  return(sam)
}

sampling.dep(1000)
set.seed(seed = 1234)
freq_test(sampling.dep(1000), 100)

sam <- runif(1000)
freq.test(sam,0:499)
d <- seq(10, 1000, 50)

pvalue <- sapply(d, function(x) freq_test(sam,x)$p.value)
df <- data.frame(d,pvalue)

library(ggplot2)
ggplot(data = df) + geom_point(mapping = aes(x = d, y = pvalue))

library(DiagrammeR)
graph <- "
digraph flowchart {

  # Node definitions
  A [shape=box, label=\"U_n\", color=\"blue\"];
  B [shape=box, label=\"Y_n\", color=\"blue\"];
  C [shape=box, label=\"count[r] = 0\", color=\"blue\"];
  D [shape=ellipse, label=\"Is r = Y_i?\", color=\"orange\"]
  E [shape=box, label=\"count[r] = count[r] + 1\", color=\"blue\"]
  F [shape=box, label=\"i = i+1\", color=\"blue\"]
  G [shape=ellipse, label=\"Is i <= n?\", color=\"orange\"]
  H [shape = box, label=\"r = r+1\", color=\"blue\"]
  I [shape=ellipse, label=\"Is r<d?\", color=\"orange\"]
  J [shape=box, label=\"End\", color=\"blue\"]
  
  
  
  
  # Edges to indicate the flow
  A -> B [label=\"Divide by d and take floor\"]
  B -> C [label=\"set r=0\"]
  C -> D [label = \"set i=1\"]
  D -> E [label=\"yes\"]
  E -> F 
  D -> F [label=\"no\"]
  F -> G 
  G -> D [label=\"yes\"]
  G -> H [label=\"no\"]
  H -> I 
  I -> C [label=\"yes\"]
  I -> J [label=\"no\"]
  

}

"

grViz(graph)
