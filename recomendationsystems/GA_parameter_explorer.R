#GA Implementation to expore the network parameter space
launchaGAportfolio <- function(){
  library(GA)

rm(list = ls())
#setwd("./")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")

clear <- function() cat("\014")

fitnesfunction<-function(x){
  NNrunid<<-NNrunid+1
  print(paste("NNrunID:", NNrunid))
  mydebug("GA Fitnessfunction Called")
  featurelistforNN <<- convertobjecttonetinputlist(x)
  
  # this limits the number of features to 350 because more than that is obscene and takes too long... Maybe after I set this thing to scale. ;)
  mydebug(paste("Number of Features used on this NN: ", length(featurelistforNN)))
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
     featurelistfilename = "bestfeaturelist.csv"
     backupoffeaturelist = paste(outputdirectory, featurelistfilename, sep = "/")
     write(featurelistforNN, file = backupoffeaturelist)
     save(bestperformance, ascii=FALSE, file=bestperformancefile)
#     save(GA, ascii=FALSE, file=gaoutputlocation)
   }
   
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
  tempx = c(1:length(obj@fitness))
  tempx[] = obj@iter
  points(tempx, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  Sys.sleep(0.05)
}

postFitness <- function(theGA){
#  print("IN THE POST FITNESS FUNCTION")
    GA = theGA
    
    save(GA, ascii=FALSE, file=gaoutputlocation)

  return(GA)
}

#rebuildstocklistfeatures()

#binary2decimal(x)
#decimal2binary(x,length)
#allstocklist <<- head(read.csv('data/exchangedata/all_stocks.csv')[,1])
clear()
portfolionickname <<- 'Energyportfolio1'
outputdirectory <<- paste("data/results/runs/", portfolionickname, sep = "")
runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"
featureslist <<- loadfeaturelist(outputdirectory)
portfoliolist <<- loadportfoliolist(outputdirectory)
#binaryresults[] = 0
#numberofstockstouse=20
NNrunid=0
populationsize=50
maxiter=50
run=5
totalsearchspacelength <- length(featureslist)
#binaryresults = seq(1, totalsearchspacelength, by=1)


#set up your running directory so you can save/restore/restart.
if (!dir.exists(outputdirectory)){dir.create(outputdirectory)}
gaoutputlocation <<- paste(outputdirectory, "/", "GA", sep="")
if(file.exists(gaoutputlocation)){
  print("GA File Found. LOADING IT")
  load(gaoutputlocation)}


#Load your best performance ever for reference
bestperformancefile <<- paste(outputdirectory, "bestperformance", sep = "/")
bestperformance <<- -1000
if(file.exists(bestperformancefile)){bestperformance <<- load(bestperformancefile)}



#initialize the suggested population towards the low side so we don't get too bogged down in giant nets... Once it is scaled this won't be needed for that, but it'll be used for re-loading previously successful runs.
suggestions = data.frame(matrix(data = rbinom(totalsearchspacelength, size = 1, prob = .25),nrow = populationsize, ncol = totalsearchspacelength))
#load previous GA run if available
if(exists("GA")){suggestions = GA@population}

#previousbests = tail(GA@bestSol,populationsize)
#previousbests = as.data.frame(matrix(unlist(previousbests),nrow=length(GA@bestSol)))
#colnames(previousbests) <- colnames(suggestions)
#suggestions = rbind(suggestions,previousbests)
#if there is a previous run started on this, use it's best as a starting point.
#Don't be a dummy, don't start over every time. ;)

cat("Total Features:", length(featureslist))
#FYI, don't turn on the multi-core/multi-processor functionality.  It tries splitting it up and then craps out.... :)
# I'm thinking I just save the phone GA to a file and let multiple machines pick it up and fire it back of again on their own with periodic checks to see if they have a better "Best"
GA<<-ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = maxiter, run = run, optim = TRUE, popSize = populationsize, keepBest = TRUE, parallel = FALSE, postFitness = postFitness, suggestions = suggestions) #type = c("binary", "real-valued", "permutation")
# if you get this error:
####Error in ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength,  : 
####Provided suggestions (ncol) matrix do not match number of variables of the problem!
#Then you probably changed your feature/portfolio.csv files and didn't remove the old GA file. ;)
#
############
#Save the last version of the GA.
save(GA, ascii=FALSE, file=gaoutputlocation)
}
