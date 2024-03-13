rm(list = ls())

gap_test <- function(sample, n, t, alpha, beta)
{
  count = rep(0, t+1)
  s = 0
  j <- min(which(sample >= alpha & sample < beta))
  while(s < n)
  {
    r = 0
    for(i in c(j : length(sample)))
    {
      if(sample[i+1] >= alpha & sample[i+1] < beta)
      {
        j = i+1
        break
      }
      else
      {
        r = r+1
      }
    }
    s = s+1
    if(r < t)
    {
      count[r+1] = count[r+1] + 1
    }
    else
      count[t+1] = count[t+1] + 1
  }
  return (count)
}

sample = sample((1:5), 10, replace = T);sample
gap_test(sample, 3, 3, 1, 3)

