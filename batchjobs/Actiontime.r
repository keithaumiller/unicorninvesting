rm(list = ls())
library(FCNN4R)
library(parallel)

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
#  print("WTF")
  portfolionickname <<- portfolio
#    userid = "runs"
#    portfolionickname = 'Energyportfolio1'
  outputdirectory = paste("data/results/",userid,"/", portfolio, "/portfoliosbest", sep = "")
  neuralnetfile = paste(outputdirectory, '/bestnetfile', sep = "")
  print(neuralnetfile)
  if(file.exists(neuralnetfile)){
    print("NN File Found. LOADING IT")
    mymlpnet_clean = readRDS(neuralnetfile)   #loads(mymlpnet_clean)
  } else {
    print("NN File Not Found")
    return(1)
  }
  
#  Logic needs to improve to pull based off date instead of position in file.
#  allocationmatrix = loadallocationfile(userid,portfolio)
    allocationmatrix = load_unicorn_allocationhistory(userid,portfolio)
#  allocationmatrix = allocationmatrix[,2:length(allocationmatrix)]
  
  yesterdaysallocation = allocationmatrix[1,]
  #if yesterdaysallocation == 100 then this is the first run...

  thisportfoliodailydata = loadthisportfoliodailydata(userid,portfolio)
  thisportfoliodailydata = thisportfoliodailydata[,grep('.output',colnames(thisportfoliodailydata),value = TRUE, invert = TRUE)]
  
  todaysallocation <<- createdailyallocation(userid,portfolio, mymlpnet_clean$net,thisportfoliodailydata)

  insert_into_unicorn_allocationhistory(userid,portfolio,todaysallocation)
  
#  print("DOING STUFF")
#First time?  This'll finish you  
  if(length(allocationmatrix) == 2)
  {
    print("Its your first time!  Use the % allocation provided")
    return(todaysallocation)
  }

 #generate % change  #compare day's and tomorrows allocation
 #
  todayspercentchangeofallocation = todaysallocation - yesterdaysallocation[1,2:length(yesterdaysallocation[1,])]
  todayspercentchangeofallocation = round(todayspercentchangeofallocation,4)
  #  currentbalance = loadportfoliobalance() #load current balance
  #loadportfoliobalance
  #generate order
  return(todayspercentchangeofallocation)
}

Endofdayprocessing<- function(){
  #Deprecated and moved to Batchscriptmaster.R so it can be called from System easily
  #logic has changed and is better too
  
  userids = load_unicorn_useridlist()
  portfolios = load_unicorn_portfoliolist()
  
#  userids = list.files("./data/results")
  for (userid in userids)
  {
#    print(userid)
#    portfolios = list.files(paste("./data/results/", userid, sep = ""))
    for (portfolio in portfolios)
    {
#    print(portfolio)
    print(paste("DailyCalculations started for User: ", userid, " Portfolio: ", portfolio, sep = ''))
    thisallocation <- mcparallel(convertnetresultsintoaction(userid,portfolio))
#    print("DONEwiththis")
#    mccollect(thisallocation)
    print(thisallocation)
    }
  }
  #  }
#}
  
# last but not least, make sure all of the model generators are still chugging away.
  for (userid in userids)
  {
    #    print(userid)
    #    portfolios = list.files(paste("./data/results/", userid, sep = ""))
    for (portfolio in portfolios)
    {
      #    print(portfolio)
      print(paste("Daily Model Generation Started for: ", userid, " Portfolio: ", portfolio, sep = ''))
      outputdirectory = paste("data/results/",userid,"/", portfolio, sep = "")
      stuff <- mcparallel(launchaGAportfolio(userid,portfolio,outputdirectory))
    }
    mccollect(stuff)
  }
  
  
}


#This is Defunct now.
loadallocationfile <- function(userid,portfolio){
  allocationfile <<- paste("data/results/", userid, "/", portfolio, "/portfoliosbest/bestNNallocationrecordfile.csv", sep = "")
  if(file.exists(allocationfile)){
#    historicalallocation <- read.csv(allocationfile, row.names=1, header = TRUE)
    historicalallocation <- read.csv(allocationfile, header = TRUE)
    return(historicalallocation)
  } else{
    historicalallocation = data.frame(date=100,holder=100)
    return(historicalallocation)}
}

#take a NN and run a record through it to get the output... then output that record to the portfolios record keeping
#we need to make a note that if an allocation is missing... something is really wrong.
#our base assumption in this function is that all portfolio members are valid.
#I know from earlier experience that isn't always true.
# perhaps a check on the Frontend to make sure only valid portfolio member choicess are made
createdailyallocation <- function(userid, portfolionickname, neuralnet, dailydata){
  #dothe evaluation
#  dailydata = thisportfoliodailydata
#  neuralnet = mymlpnet_clean$net
#  userid="runs" 
#  portfolio="Energyportfolio1"
  portfoliohome = paste("data/results/",userid,"/", portfolionickname, sep ='')
  outputdirectory = paste(portfoliohome, "/portfoliosbest", sep = "")

    portfoliolist = loadportfoliolist(userid,portfolionickname)
  
  todaysallocationtemp = mlp_eval(neuralnet,data.matrix(dailydata))
  rownames(todaysallocationtemp)<- rownames(dailydata)
  colnames(todaysallocationtemp)<- portfoliolist
  
  todaysallocation = convertNNoutputtoallocation(todaysallocationtemp)
  
  
  outputableallocation = paste(rownames(todaysallocation),paste(todaysallocation,collapse= ","),sep = ',')

 return(todaysallocation)
}

generateallocationfile <- function(allocation){
  success = FALSE
  #  bestNNallocationrecordfile.csv

  return(success)
}


#This will only work after market close.  Otherwise you get all NAs
loadthisportfoliodailydata <- function(userid, portfolio){

  #this pulls down the latest data for this portfolio... We need to fix this.. very inefficient. another terrible global function
  # We are going to want to add some error checking to make sure that before we return the "Current Day" that the dat ain that dataset is "Current day"
  # For now I'm doing happy path coding.
  outputdirectory = paste("data/results/", userid,"/", portfolio, "/portfoliosbest", sep = '')
  
  #This whole function is crap and sets a bunch of global variables and constructs some data sets...
  #makes me sick to go through
  #rather than fix it right now, I'm just going to call it, swallow my pride and use the globals.
  numbertopull = 0 #this doesn't even get used int he function anymore...
#  featurelist<<-  loadfeaturelist(outputdirectory)
  featurelist<<- load_unicorn_best_featurelist(userid,portfolio)

  #make sure the data is updated...this needs to be removed soon and put in a batch job that pulls all needed data in one job...
#  pullstocklist(featurelistforNN)
  numberofstockscombined = combinestocksfunction(numbertopullparam, featurelist, outputdirectory)

  #global variables are defined now. ugh
#  currentdate = Sys.Date()
  # comment out this line when running in prod.  because current day data isn't available until after Eod close for markets
#  currentdate = currentdate-1
#  todaysdata = percentchangedcombined[rownames(percentchangedcombined) == currentdate,]
   todaysdata = tail(percentchangedcombined,1)
  
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