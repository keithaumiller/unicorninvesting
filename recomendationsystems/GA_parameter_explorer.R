#GA Implementation to expore the network parameter space
library(GA)

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")

rebuildstocklistfeatures()

#binary2decimal(x)
#decimal2binary(x,length)
allstocklist <<- head(read.csv('data/exchangedata/all_stocks.csv')[,1])
featureslist <- read.csv('data/features/featureslist.csv')[,1]
totalsearchspacelength = length(levels(featureslist))
binaryresults = seq(1, totalsearchspacelength, by=1)
binaryresults[] = 0
numberofstockstouse=20
bestperformance<<- -100000


fitnesfunction<-function(x){
  featurelistforNN <<- convertobjecttonetinputlist(x)
  #Generate trainNN
  runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"
  performance <- modelexplorer(runid, featurelistforNN)
  #comment out line above and uncomment this line below to play with the GA feature selector.
  #  performance <<- sum(x)
  if (performance>bestperformance){
    bestperformance<<-performance
    temporarystring = paste(featurelistforNN, collapse = ',')
    portfolioid = 1
    thisGARun=paste(runid, performance, portfolioid, temporarystring,sep = ",")
    write(thisGARun,file="data/GAResults.csv",append=TRUE)
  }
  return(performance)
}

convertobjecttonetinputlist <- function(XX){
#  cat("THISONE:\n", XX,"\n\n\n\n\n\n")
#  TESTING<<-XX
  convertedlist = featureslist[XX == 1]
  #cat(convertedlist,"\n")
  return(convertedlist)
}


monitor <- function(obj)
{
  plot(obj, main = paste("iteration =", obj@iter))
#  points(obj@population, obj@fitness, pch = 20, col = 2)
#  points(obj@iter, fitnesfunction(obj@population), pch = 20, col = 5)
  #rug(obj@population, col = 2)
  Sys.sleep(0.05)
}

cat("Features to use:", length(featureslist))
GA<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = 30, run = 5) #type = c("binary", "real-valued", "permutation")
