#GA Implementation to expore the network parameter space
library(GA)

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")



#rebuildstocklistfeatures()

#binary2decimal(x)
#decimal2binary(x,length)
#allstocklist <<- head(read.csv('data/exchangedata/all_stocks.csv')[,1])
featureslist <<- loadfeaturelist()
totalsearchspacelength = length(featureslist)
binaryresults = seq(1, totalsearchspacelength, by=1)
binaryresults[] = 0
numberofstockstouse=20
bestperformance<<- -100000
NNrunid=0

portfolionickname <<- 'Energyportfolio1'
outputdirectory <<- paste("data/results/runs/", portfolionickname, sep = "")

if (!dir.exists(outputdirectory)){dir.create(outputdirectory)}
file.copy("data/exchangedata/portfolio.csv", outputdirectory)


fitnesfunction<-function(x){
  NNrunid<<-NNrunid+1
  mydebug("GA Fitnessfunction Called")
  featurelistforNN <<- convertobjecttonetinputlist(x)
  #Generate trainNN
  runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"
  performance <<- modelexplorer(runid, featurelistforNN)
  #comment out line above and uncomment this line below to play with the GA feature selector.
  #  performance <<- sum(x)
      
  thisGARun=paste(portfolionickname,NNrunid, runid, performance,sep = ",")
  GARunresultsfile = paste(outputdirectory, 'GAResults.csv', sep = "/")
  write(thisGARun,file=GARunresultsfile,append=TRUE)
  
    if (performance>bestperformance){
    bestperformance<<-performance
    featurelistfilename = paste(bestperformance, NNrunid, runid, "featurelist.csv", sep = "_")
    backupoffeaturelist = paste(outputdirectory, featurelistfilename, sep = "/")
    write(featurelistforNN, file = backupoffeaturelist)
  }
  print(paste("NNrunID:", NNrunid))
  return(performance)
}

convertobjecttonetinputlist <- function(XX){
  
  mydebug("GA convertobjecttonetinputlist Called")
#  cat("THISONE:\n", XX,"\n\n\n\n\n\n")
#  TESTING<<-XX
  convertedlist = featureslist[XX == 1]
  #cat(convertedlist,"\n")
  return(convertedlist)
}


monitor <- function(obj)
{
  mydebug("GA Monitor Called")
  plot(obj, main = paste("iteration =", obj@iter))
#  points(obj@population, obj@fitness, pch = 20, col = 2)
#  points(obj@iter, fitnesfunction(obj@population), pch = 20, col = 5)
#  rug(obj@population, col = 2)
  Sys.sleep(0.05)
}

cat("Features to use:", length(featureslist))
GA<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = 30, run = 5) #type = c("binary", "real-valued", "permutation")
