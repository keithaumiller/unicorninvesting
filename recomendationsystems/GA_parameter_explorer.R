#GA Implementation to expore the network parameter space
library(GA)

rm(list = ls())
setwd("./")
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
portfoliolist <<- loadportfoliolist()
totalsearchspacelength = length(featureslist)
binaryresults = seq(1, totalsearchspacelength, by=1)
binaryresults[] = 0
numberofstockstouse=20
bestperformance<<- -100000
NNrunid=0

portfolionickname <<- 'Energyportfolio1'
outputdirectory <<- paste("data/results/runs/", portfolionickname, sep = "")
runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"

if (!dir.exists(outputdirectory)){dir.create(outputdirectory)}
file.copy("data/exchangedata/portfolio.csv", outputdirectory)
gaoutputlocation <<- paste(outputdirectory, "/", runid, "GA", sep="")


fitnesfunction<-function(x){
  NNrunid<<-NNrunid+1
  mydebug("GA Fitnessfunction Called")
  featurelistforNN <<- convertobjecttonetinputlist(x)

  
  # this limits the number of features to 350 because more than that is obscene and takes too long... Maybe after I set this thing to scale. ;)
  print(paste("Number of features to Use: ", length(featurelistforNN)))
  if((length(featurelistforNN) > 350)) {return((length(featurelistforNN) * -1)-10000)}
  
  #Generate trainNN
  performance <<- modelexplorer(runid, featurelistforNN)
  #comment out line above and uncomment this line below to play with the GA feature selector.
  #  performance <<- sum(x)
      
  thisGARun=paste(portfolionickname,NNrunid, runid, performance,sep = ",")
  GARunresultsfile = paste(outputdirectory, 'GAResults.csv', sep = "/")
  write(thisGARun,file=GARunresultsfile,append=TRUE)
  
    if (performance>bestperformance){
    bestperformance<<-performance
    featurelistfilename = paste(runid, NNrunid,bestperformance, "featurelist.csv", sep = "_")
    backupoffeaturelist = paste(outputdirectory, featurelistfilename, sep = "/")
    write(featurelistforNN, file = backupoffeaturelist)
    if(exists('GA')){save(GA, ascii=FALSE, file=gaoutputlocation)}
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
  plot(obj)#, main = paste(obj@iter))
#  points(obj@population, obj@fitness, pch = 20, col = 2)
#  points(obj@iter, fitnesfunction(obj@population), pch = 20, col = 5)
#  rug(obj@population, col = 2)
  Sys.sleep(0.05)
}

loadexistingGA <- function(x){
  filetoload = x
  return(theGAfromfile)
}

#initialize it towards the low side so we don't get too bogged down in giant nets... Once it is scaled this won't be needed for that, but it'll be used for re-loading previously successful runs.
suggestions = data.frame(matrix(data = rbinom(totalsearchspacelength, size = 1, prob = .25),nrow = 100, ncol = totalsearchspacelength))

cat("Features to use:", length(featureslist))
#FYI, don't turn on the multi-core/multi-processor functionality.  It tries splitting it up and then craps out.... :)
# I'm thinking I just save the phone GA to a file and let multiple machines pick it up and fire it back of again on their own with periodic checks to see if they have a better "Best"
GA<<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = 200, run = 20, optim = TRUE, popSize = 100, keepBest = TRUE, parallel = FALSE, suggestions = suggestions) #type = c("binary", "real-valued", "permutation")

gaoutputlocation <<- paste(outputdirectory, "/GA", runid, sep="")

############
#save the GA
if(exists('GA')){save(GA, ascii=FALSE, file=gaoutputlocation)}
#load the GA
#load(gaoutputlocation)



#Defect where.. THe lower the number of features, the higher the performance...?