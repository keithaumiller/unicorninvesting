#GA Implementation to expore the network parameter space
library(GA)

#binary2decimal(x)
#decimal2binary(x,length)
allstocklist <<- head(read.csv('data/exchangedata/all_stocks.csv')[,1])
featureslist <- read.csv('data/features/featureslist.csv')[,1]
totalsearchspacelength = length(levels(featureslist))
binaryresults = seq(1, totalsearchspacelength, by=1)
binaryresults[] = 0
numberofstockstouse=8

fitnesfunction<-function(x){
  featurelistforNN <- convertobjecttonetinputlist(x)
  #Generate trainNN
  runid = Sys.time()
  performance = modelexplorer(runid, featurelistforNN)
  return(performance)
}

convertobjecttonetinputlist <- function(XX){
#  cat("THISONE:\n", XX,"\n\n\n\n\n\n")
  convertedlist = featureslist[XX == 1]
  return(convertedlist)
}


monitor <- function(obj)
{
  cat(obj@fitnessValue)
  cat(paste("Iteration", obj@iter, '\n'))
  plot(obj, main = paste("iteration =", obj@iter))
#  points(obj@population, obj@fitness, pch = 20, col = 2)
#  points(obj@iter, fitnesfunction(obj@population), pch = 20, col = 5)
  #rug(obj@population, col = 2)
  Sys.sleep(0.05)
}

GA<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = 2, run = 20) #type = c("binary", "real-valued", "permutation")
