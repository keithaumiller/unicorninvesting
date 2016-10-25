#GA Implementation to expore the network parameter space
library(GA)

#binary2decimal(x)
#decimal2binary(x,length)
allstocklist <<- head(read.csv('data/exchangedata/all_stocks.csv')[,1])
totalsearchspacelength = 200 #length(levels(allstocklist))
binaryresults = seq(1, totalsearchspacelength, by=1)
binaryresults[] = 0

fitnesfunction<-function(x){
  return(sum(x))
}

monitor <- function(obj)
{
  cat(obj@iter)
  plot(obj, main = paste("iteration =", obj@iter))
  #points(obj@population, obj@fitness, pch = 20, col = 2)
  #rug(obj@population, col = 2)
  Sys.sleep(0.1)
}

GA<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = 1000, run = 20) #type = c("binary", "real-valued", "permutation"),