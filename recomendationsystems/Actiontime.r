rm(list = ls())

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")
if(!exists("combinestocksfunction", mode="function")) source("./datasetcreation/Combinestocks.R")

convertnetresultsintoaction <- function(userid,portfolio){
#  userid = 1
#  portfolio = 'Energyportfolio1'
  portfolionickname <<- portfolio
  outputdirectory = paste("data/results/runs/", portfolionickname, "/portfoliosbest", sep = "")
  neuralnetfile = paste(outputdirectory, '/bestnetfile', sep = "")
  if(file.exists(neuralnetfile)){
    print("NN File Found. LOADING IT")
    load(neuralnetfile)   #loads(mymlpnet_clean)
  } else {
    print("NN File Not Found")
    return(1)
  }
  
  #Logic needs to improve to pull based off date instead of position in file.
  allocationmatrix =loadallocationfile(portfolionickname)
  allocationmatrix = allocationmatrix[,2:length(allocationmatrix)]
  yesterdaysallocation = round(tail(allocationmatrix,1),5)
  #if yesterdaysallocation == 100 then this is the first run...

  thisportfoliodailydata = loadthisportfoliodailydata(portfolio)
  thisportfoliodailydata = thisportfoliodailydata[,grep('.output',colnames(thisportfoliodailydata),value = TRUE, invert = TRUE)]
  todaysallocation = round(createdailyallocation(mymlpnet_clean$net,thisportfoliodailydata),5) 

  
#First time?  This'll finish you  
  if(length(allocationmatrix) == 1)
  {
    print("Its your first time!  Use the % allocation provided")
    return(todaysallocation)
  }

 #generate % change  #compare day's and tomorrows allocation
 #
  todayspercentchangeofallocation = todaysallocation - yesterdaysallocation
#  currentbalance = loadportfoliobalance() #load current balance
  #loadportfoliobalance
  #generate order
  return(todayspercentchangeofallocation)
}

Endofdayprocessing<- function(){

  portfoliofiles = list.files("./data/results", recursive = TRUE ,pattern = 'portfolio.csv')
  featurelistfiles = list.files("./data/results", recursive = TRUE ,pattern = 'featurelist.csv')
  fileslist = c(portfoliofiles,featurelistfiles)
  stocklist = vector()
  
  setwd("./data/results")
  for (thisfile in fileslist)
  {
    thisfilesdata <- read.csv(thisfile, header = FALSE)[,1]
    thisfilesdata = levels(thisfilesdata)
    stocklist = unique(c(stocklist,thisfilesdata))
#    thisfilesdata <- c(thisfilesdata,read.csv("data/results/runs/Energyportfolio1/portfoliosbest/featurelist.csv", header = FALSE)[,1])
  }
  setwd("../../")
  
  pullstocklist(stocklist)
  
  for (userid in list.files("./data/results"))
  {
    for (portfolio in list.files(paste("./data/results/", userid, sep = "")))
      print(paste("DailyCalculations started for User: ", userid, " Portfolio: ", portfolio, sep = ''))
    thisallocation = convertnetresultsintoaction(userid,portfolio)
    print(thisallocation)
  }


  #  }
#}
  
}

loadallocationfile <- function(portfolionickname){
  allocationfile <<- paste("data/results/runs/", portfolionickname, "/portfoliosbest/bestNNallocationrecordfile.csv", sep = "")
  if(file.exists(allocationfile)){
#    historicalallocation <- read.csv(allocationfile, row.names=1, header = TRUE)
    historicalallocation <- read.csv(allocationfile, header = TRUE)
    return(historicalallocation)
  } else{ return(100)}
}

#take a NN and run a record through it to get the output... then output that record to the portfolios record keeping
#we need to make a note that if an allocation is missing... something is really wrong.
#our base assumption in this function is that all portfolio members are valid.
#I know from earlier experience that isn't always true.
# perhaps a check on the Frontend to make sure only valid portfolio member choicess are made
createdailyallocation <- function(neuralnet, dailydata){
  #dothe evaluation
#  dailydata = thisportfoliodailydata
#  neuralnet = mymlpnet_clean$net
  portfoliohome = paste("data/results/runs/", portfolionickname, sep ='')
  outputdirectory <<- paste(portfoliohome, "/portfoliosbest", sep = "")

    portfoliolist =     loadportfoliolist(portfoliohome)
  
  todaysallocationtemp = mlp_eval(neuralnet,data.matrix(dailydata))
  rownames(todaysallocationtemp)<- rownames(dailydata)
  colnames(todaysallocationtemp)<- portfoliolist
  
  todaysallocation = convertNNoutputtoallocation(todaysallocationtemp)
  
  
  outputableallocation = paste(rownames(todaysallocation),paste(todaysallocation,collapse= ","),sep = ',')
  #write it to a file. It'simportant that this file only contain data for days that the net adding to it did not get trained on.
  #otherwise, the results will be skewed
  bestNNallocationrecordfile = paste(outputdirectory,"/bestNNallocationrecordfile.csv", sep = '')
  translatedtototalpercentperasset = convertNNoutputtoallocation(todaysallocation)
  
  if(file.exists(bestNNallocationrecordfile)){
    print("Allocation File Found")
  } else {
    print("Allocation File Not Found")
    headerrow = paste('Date',paste(colnames(todaysallocation), collapse = ','),sep = ',')
    write(headerrow,bestNNallocationrecordfile,append = FALSE)
  }
 write(outputableallocation,file=bestNNallocationrecordfile,append=TRUE)
 
 
 
 return(todaysallocation)
}



#This will only work after market close.  Otherwise you get all NAs
loadthisportfoliodailydata <- function(portfolio){

  #this pulls down the latest data for this portfolio... We need to fix this.. very inefficient. another terrible global function
  # We are going to want to add some error checking to make sure that before we return the "Current Day" that the dat ain that dataset is "Current day"
  # For now I'm doing happy path coding.
  outputdirectory = paste("data/results/runs/", portfolio, "/portfoliosbest", sep = '')
  
  #This whole function is crap and sets a bunch of global variables and constructs some data sets...
  #makes me sick to go through
  #rather than fix it right now, I'm just going to call it, swallow my pride and use the globals.
  numbertopull = 0 #this doesn't even get used int he function anymore...
  featurelistforNN = loadfeaturelist(outputdirectory)
  
  #make sure the data is updated...this needs to be removed soon and put in a batch job that pulls all needed data in one job...
#  pullstocklist(featurelistforNN)
  numberofstockscombined = combinestocksfunction(numbertopullparam, featurelistforNN, outputdirectory)

  #global variables are defined now. ugh
  currentdate = Sys.Date()
  # comment out this line when running in prod.  because current day data isn't available until after Eod close for markets
  currentdate = currentdate-1
  todaysdata = percentchangedcombined[rownames(percentchangedcombined) == currentdate,]
  return(todaysdata)
}

convertNNoutputtoallocation <- function(todaysallocation){

  returnabletodaysallocation = todaysallocation/sum(todaysallocation)
  
  return(returnabletodaysallocation)
}

writethisportfolioshistoryofallocation <- function(returnabletodaysallocation){
  returnabletodaysallocation = todaysallocation
  
  historyofallocationfile = paste(outputdirectory, "/historyofallocationfile.csv", sep='')
 if(!exists(historyofallocationfile)){
  headerrow = paste('Date',paste(colnames(returnabletodaysallocation), collapse = ','),sep = ',')
  write(headerrow,historyofallocationfile,append = FALSE)
 }  
  
write(returnabletodaysallocation,file=historyofallocationfile,append=TRUE)
}