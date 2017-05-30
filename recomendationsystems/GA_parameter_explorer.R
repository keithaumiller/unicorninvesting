#GA Implementation to expore the network parameter space

convertobjecttonetinputlist <- function(XX){
  
  mydebug("GA convertobjecttonetinputlist Called")
  #  cat("THISONE:\n", XX,"\n\n\n\n\n\n")
  #  TESTING<<-XX
  convertedlist = featureslist[XX == 1]
  #cat(convertedlist,"\n")
  return(convertedlist)
}

postFitness <- function(theGA){
  #  print("IN THE POST FITNESS FUNCTION")
  GA = theGA
  #    saveRDS(GA, ascii=FALSE, file=gaoutputlocation,refhook = 'GA')
  return(GA)
}
plotmyNN <- function(myfilesavelocation,performance){
  png(filename = myfilesavelocation, width = 900, height = 900)
  plot(NNperformancechart)
  # Create a title with a red, bold/italic font
  maintitle = paste("From $1000 to $", round(performance,2), " with ", length(featurelistforNN), " features!", sep = '')
  title(main=maintitle, col.main="Blue", font.main=4)
  # Label the x and y axes with dark green text
  title(xlab= "          Days 1:365", col.lab=rgb(0,0.5,0))
  #    title(ylab= "Total bankroll if starting with 1000$", col.lab=rgb(0,0.5,0))
  dev.off()
}

updateportfolioGAplot <- function(obj){
  myfilesavelocation = paste(outputdirectory, "/plots/portfoliohistory.png", sep = '')
  png(filename = myfilesavelocation, width = 900, height = 900)
  plot(obj)#, main = paste(obj@iter))
  summarymean = obj@summary[,"mean"]
  tempx = c(1:length(summarymean))
  points(tempx, summarymean, pch = 4, col = 9)
  tempx = c(1:length(obj@fitness))
  tempx[] = obj@iter
  points(tempx, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  dev.off()
  Sys.sleep(0.05)
}

combineGAsummarys <- function(GA1, GA2){
  GA3 = GA2
  GA3@summary = rbind(GA1@summary,GA2@summary)
  GA1dims = dim(GA1@summary)[1]
  GA2dims = dim(GA2@summary)[1]
  print(GA1dims)
  print(GA2dims)
  
  GA3dims = GA1dims+GA2dims
  #GA3@iter = GA1@iter + GA2@iter 
  GA3@iter = GA3dims
  print(GA3dims)
  rownames(GA3@summary)<- 1:GA3dims
  return(GA3)
}

creatpopulation <- function(object, ...){
  population <- matrix(as.double(NA), nrow = object@popSize, 
                       ncol = object@nBits)
  for (j in 1:object@nBits) {
    population[, j] <- round(runif(object@popSize))
  }
  if(exists("GA1"))
  {
    suggestionsadd = data.frame(matrix(data = rbinom(totalsearchspacelength, size = 1, prob = probability),nrow = populationsize, ncol = totalsearchspacelength-dim(GA1@population)[2]))
    population = GA1@population
  }
  return(population)
}
monitor <- function(obj)
{
  #  monitoredGA<--obj
  mydebug("GA Monitor Called")
  gatempid =  gsub(" ", "-", gsub(":", "-", Sys.time()))
  tempgaoutputlocation = paste(outputdirectory, "/plots/Generations-GArunid-", runid, "-GAinterim-", gatempid, sep = '')
  saveRDS(obj, ascii=FALSE, file=tempgaoutputlocation,refhook = 'GAinterim')
  
  myfilesavelocation = paste(outputdirectory, "/plots/Generations-GArunid-", runid, ".png", sep = '')
  png(filename = myfilesavelocation, width = 900, height = 900)
  plot(obj)#, main = paste(obj@iter))
  
  summarymean = obj@summary[,"mean"]
  tempx = c(1:length(summarymean))
  points(tempx, summarymean, pch = 4, col = 9)
  
  tempx = c(1:length(obj@fitness))
  tempx[] = obj@iter
  points(tempx, obj@fitness, pch = 20, col = 2)
  rug(obj@population, col = 2)
  dev.off()
  Sys.sleep(0.05)
}

fitnesfunction<-function(x){
  NNperformancechart<<- 1000
  NNrunid<<-NNrunid+1
  print(paste("NNrunID:", NNrunid))
  print(Sys.time())
  #Load your best performance ever for reference
  bestperformance <<- -1000
  
  if(!dir.exists(paste(outputdirectory,"/portfoliosbest",sep = '')))
  {dir.create(paste(outputdirectory,"/portfoliosbest",sep = ''), recursive = TRUE)}
  #Where to put the "Best Net" created.... for in theory use of trade management...
  bestnetfile <<- paste(outputdirectory, "/portfoliosbest/bestnetfile", sep = "/")
  #  bestperformancefile <<- paste(outputdirectory, "portfoliosbest/bestperformance", sep = "/")
  myfilesavelocation = paste("./", outputdirectory, "/plots/GArunid-", runid, "-NNrunid-" ,NNrunid, "netperformance.png", sep = '')
  bestNNplotfilesavelocation = paste("./", outputdirectory, "/plots/BestNN-netperformance.png", sep = '')
  
  
  
  #  if(file.exists(bestperformancefile)){bestperformance = readRDS(bestperformancefile)}
  portfoliodetails = load_unicorn_portfolios_details(userid, portfolioid)
  bestperformance = portfoliodetails[,'bestperformance']
  if(is.na(bestperformance)){
    bestperformance=-1000
  }
  print("Bestperformance:")
  print(bestperformance)
  
  featurelistforNN <<- convertobjecttonetinputlist(x)
  #  print(paste("Number of features on this net: ", length(featurelistforNN), sep = ''))
  # this limits the number of features to 30 stocks because more than that is obscene and takes too long... Maybe after I set this thing to scale. ;)
  
  print(paste("Number of Features used on this NN: ", length(featurelistforNN)))
  
  maxfeatureslimit = 40
  #  isforex = file.exists(paste(outputdirectory,"/isforex",sep = ''))
  isforex = portfolioisforex(userid, portfolioid)
  
  if(isforex == TRUE){maxfeatureslimit = 1200 }
  
  if((length(featurelistforNN) > maxfeatureslimit)) {
    print(paste("TOO LONG: ", length(featurelistforNN), sep=''))
    return((length(featurelistforNN)*-.0001))
  }
  
  #Generate trainNN
  performance <<- modelexplorer(runid, featurelistforNN,outputdirectory)
  #comment out line above and uncomment this line below to play with the GA feature selector.
  #  performance <<- sum(x)
  if(performance > 1500){
    #    plotmyNN(myfilesavelocation,performance)
  }
  
  thisGARun=paste(portfolioid,NNrunid, runid, performance,sep = ",")
  GARunresultsfile = paste(outputdirectory, 'GAResults.csv', sep = "/")
  write(thisGARun,file=GARunresultsfile,append=TRUE)
  
  if (performance>bestperformance){
    plotmyNN(myfilesavelocation,performance)
    file.copy(myfilesavelocation,bestNNplotfilesavelocation,overwrite = TRUE)
    bestperformance = performance
    featurelistfilename = "featurelist.csv"
    portfoliolistfilename = "portfolio.csv"
    #     file.copy(paste(outputdirectory,"/portfolio.csv", sep = ""), paste(outputdirectory, "/portfoliosbest/portfolio.csv", sep = ""),overwrite = TRUE)
    #     backupoffeaturelist = paste(outputdirectory,"/portfoliosbest/", featurelistfilename, sep = "")
    #     write.csv(featurelistforNN, file = backupoffeaturelist,row.names = FALSE,quote = FALSE)  #until we get it in the DB
    
    insert_into_unicorn_best_featurelist(userid,portfolioid,featurelistforNN)
    #     saveRDS(featurelistforNN, file = backupoffeaturelist)
    #     print("saving bestperformance")
    #     print(bestperformance)
    
    #saveRDS(bestperformance, ascii=FALSE, file=bestperformancefile, refhook = 'bestperformance')
    insert_into_unicorn_portfolios_details(userid,portfolioid,bestperformance)
  }
  
    return(performance)
  }
  
launchaGAportfolio <- function(userid, portfolioid, outputdirectory){
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

clear()
NNperformancechart<<- 1000
portfolioid <<- portfolioid
outputdirectory = outputdirectory
runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"
featureslist <<- loadfeaturelist(userid,portfolioid)
portfoliolist <<- loadportfoliolist(userid,portfolioid)
#binaryresults[] = 0
#numberofstockstouse=20
NNrunid<<-0
populationsize=300
maxiter=100
run=200
averagefitnessbyiteration = c(seq(1:maxiter))
averagefitnessbyiteration[] = 0
totalsearchspacelength <- length(featureslist)
#binaryresults = seq(1, totalsearchspacelength, by=1)


#set up your running directory so you can save/restore/restart.
if (!dir.exists(outputdirectory)){dir.create(outputdirectory)}

gaoutputlocation <<- paste(outputdirectory, "/portfoliosbest/GA", sep="")

# if plot direcotry isn't there, create it
plotdir = paste(outputdirectory, "/plots", sep = '')
if (!dir.exists(plotdir)){dir.create(plotdir)}


if(file.exists(gaoutputlocation)){
  print("GA File Found. LOADING IT")
  GA1 <- readRDS(gaoutputlocation)
  }




#initialize the suggested population towards the low side so we don't get too bogged down in giant nets... Once it is scaled this won't be needed for that, but it'll be used for re-loading previously successful runs.
#load previous GA run if available

#Initialize it so roughly this many are set as features... 
probability = 10/totalsearchspacelength
if(exists("GA1"))
  {
  suggestionsadd = data.frame(matrix(data = rbinom(totalsearchspacelength, size = 1, prob = probability),nrow = populationsize, ncol = totalsearchspacelength-dim(GA1@population)[2]))
  suggestions = GA1@population
  suggestions = cbind(suggestions,suggestionsadd)
  }
  else
  {
    suggestions = data.frame(matrix(data = rbinom(totalsearchspacelength, size = 1, prob = probability),nrow = populationsize, ncol = totalsearchspacelength))
  }

#previousbests = tail(GA@bestSol,populationsize)
#previousbests = as.data.frame(matrix(unlist(previousbests),nrow=length(GA@bestSol)))
#colnames(previousbests) <- colnames(suggestions)
#suggestions = rbind(suggestions,previousbests)
#if there is a previous run started on this, use it's best as a starting point.
#Don't be a dummy, don't start over every time. ;)

print(paste("Total Features:", length(featureslist)))
#FYI, don't turn on the multi-core/multi-processor functionality.  It tries splitting it up and then craps out.... :)
# I'm thinking I just save the phone GA to a file and let multiple machines pick it up and fire it back of again on their own with periodic checks to see if they have a better "Best"
GA<<-ga(type = "binary",population = creatpopulation, fitness = fitnesfunction, nBits = totalsearchspacelength, monitor = monitor, maxiter = maxiter, run = run, optim = TRUE, popSize = populationsize, keepBest = TRUE, parallel = FALSE, postFitness = postFitness, suggestions = suggestions, pmutation = .2) #type = c("binary", "real-valued", "permutation")
# if you get this error:
####Error in ga(type = "binary", fitness = fitnesfunction, nBits = totalsearchspacelength,  : 
####Provided suggestions (ncol) matrix do not match number of variables of the problem!
#Then you probably changed your feature/portfolio.csv files and didn't remove the old GA file. ;)
#
############
#Save the last version of the GA.
#print(paste("Final Monitor GA :", GA@iter, dim(GA@summary)))
#print(paste("Final Monitor GA1 :", GA1@iter, dim(GA1@summary)))

if(exists('GA1')){
  GA <- combineGAsummarys(GA1, GA)
}
updateportfolioGAplot(GA)
saveRDS(GA, ascii=FALSE, file=gaoutputlocation,refhook = 'GA')
}