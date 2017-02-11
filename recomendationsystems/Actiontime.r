convertnetresultsintoaction <- function(userid,portfolio){
  userid = 1
  portfolio = 'Energyportfolio1'
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
  yesterdaysallocation = round(tail(allocationmatrix,1),20)

  thisportfoliodailydata = loadthisportfoliodailydata(portfolio)
  todaysallocation = round(createdailyallocation(mymlpnet_clean,thisportfoliodailydata),20) 

  
#First time?  This'll finish you  
  if(length(allocationmatrix) == 1)
  {
    print("Its your first time!  Use the % allocation provided")
    yesterdaysallocation = todaysallocation
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
#  for each user {
  #for each portfolio{
  convertnetresultsintoaction()
#  }
#}
  
}

loadallocationfile <- function(portfolionickname){
  allocationfile <<- paste("data/results/runs/", portfolionickname, "/portfoliosbest/bestNNallocationrecordfile.csv", sep = "")
  if(exists('allocationfile')){
    historicalallocation <- read.csv(allocationfile, row.names=1, header = TRUE)
    return(historicalallocation)
  } else{ return(1)}
}

#take a NN and run a record through it to get the output... then output that record to the portfolios record keeping
#we need to make a note that if an allocation is missing... something is really wrong.
#our base assumption in this function is that all portfolio members are valid.
#I know from earlier experience that isn't always true.
# perhaps a check on the Frontend to make sure only valid portfolio member choicess are made
createdailyallocation <- function(neuralnet, dailydata){
  #dothe evaluation
  dailydata = head(evalmatrix[,1:inputlayersize],1)
  neuralnet = mymlpnet_clean$net
  portfoliolist =     loadportfoliolist(portfoliohome)
  
  todaysallocationtemp = mlp_eval(neuralnet,dailydata)
  rownames(todaysallocationtemp)<- rownames(dailydata)
  colnames(todaysallocationtemp)<- portfoliolist
  
  todaysallocation = convertNNoutputtoallocation(todaysallocationtemp)
  
  
  outputableallocation = paste(rownames(todaysallocation),paste(todaysallocation,collapse= ","),sep = ',')
  portfoliohome = paste("data/results/runs/", portfolionickname, sep ='')
  outputdirectory <<- paste(portfoliohome, "/portfoliosbest", sep = "")
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



#this is still in progress...after running this, we should have a percent changed for the current date assuming its run after market close and data has been updated on Yahoo
loadthisportfoliodailydata <- function(portfolio){

  #this pulls down the latest data for this portfolio... We need to fix this.. very inefficient. another terrible global function
  # We are going to want to add some error checking to make sure that before we return the "Current Day" that the dat ain that dataset is "Current day"
  # For now I'm doing happy path coding.
  outputdirectory = paste("data/results/runs/", portfolio,sep = '')
  
  #This whole function is crap and sets a bunch of global variables and constructs some data sets...
  #makes me sick to go through
  #rather than fix it right now, I'm just going to call it, swallow my pride and use the globals.
  numbertopull = 0 #this doesn't even get used int he function anymore...
  featurelistforNN = loadfeaturelist(outputdirectory)
  
  #make sure the data is updated...this needs to be removed soon and put in a batch job that pulls all needed data in one job...
  pullstocklist(featurelistforNN)
  numberofstockscombined = combinestocksfunction(numbertopullparam, featurelistforNN)

  #global variables are defined now. ugh
  currentdate = Sys.Date()
  todaysdata = percentchangedcombined[currentdate,]
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