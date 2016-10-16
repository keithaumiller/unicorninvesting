combinestocksfunction <- function(numbertopullparam){
print(paste("Combining: ", numbertopullparam,sep = ""))
numbertopull=numbertopullparam
#datetoread=Sys.Date()
datetoread="2016-05-16"

  library(quantmod)
  library(plyr)

  
  amex = read.csv('data/exchangedata/amex.csv')  # read csv file
  amexstocks = amex[,1]
  nasdaq = read.csv('data/exchangedata/nasdaq.csv')  # read csv file
  nasdaqstocks = nasdaq[,1]
  nyse = read.csv('data/exchangedata/nyse.csv')  # read csv file
  nysestocks = nyse[,1]
  
  #TODO:  Add logic to updat ethe \^ stocks and strip spaces out of the symbol names\
  
  rm(amex,nasdaq,nyse)
  symbolsavailable = list.files(path = 'data/stockdata')
  
  #stocklist = c(as.vector(nysestocks))+*+
  
  
#  stocklist = head(symbolsavailable,numbertopull)
  stocklist <<- head(read.csv('data/exchangedata/stockstouse.csv')[,1],numbertopull)
  #datetoread = '2016-05-10'
  stockstocombine <<- data.frame()
  rm(percentchangedcombined)
  percentchangedcombined <<- data.frame()
#  print(paste("COLUMNCHECK: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
  count = 0
  for (i in (stocklist))
  {
    
    filetoread = paste('data/stockdata/', i, "/", paste(i,datetoread, sep ="_"), sep = "")
    #  print(filetoread)
    print(paste("Reading in file :", count, filetoread, sep=' '))
    tempstockdata = read.csv(filetoread)
    #  print(head(tempstockdata))
    if(is.data.frame(stockstocombine) && nrow(stockstocombine)==0){
      stockstocombine = tempstockdata
    }
    stockstocombine = merge.data.frame(stockstocombine,tempstockdata, all = TRUE)
    count = count + 1
    #  print(i)
    
    
    #  print(head(tempsymbolholder))
    #  print(is.data.frame(tempsymbolholder))
    #  stockstocombine[i] <- cbind.data.frame(tempsymbolholder)
  }

  print("Stock Frames Combined")
#  print(paste("COLUMNCHECK: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
  names(stockstocombine)[names(stockstocombine)=="X"] <- "Date"
  row.names(stockstocombine)<- stockstocombine$Date
  stockstocombine = stockstocombine[,-1]   #strip date now that it is the row name
  percentchangedcombined <<- stockstocombine[-1,]  # strip the first record for the percentchanged matrix
  combinedstockdatadimensions = dim(stockstocombine)
  #add a dummy record to the end of the percentchangematrix
  temprow <- c(1:combinedstockdatadimensions[2])
  percentchangedcombined<<-rbind(percentchangedcombined,temprow)
  percentchangedcombined<<-(percentchangedcombined/stockstocombine -1)
  
#  print(paste("COLUMNCHECKmid2: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
  adjustedcolumnnames <<- grep('Adjusted',colnames(percentchangedcombined),value =TRUE)
  adjustedmatrix <<- data.frame()
  adjustedmatrix <<- percentchangedcombined[,adjustedcolumnnames]
  
  rm(tempstockdata)
  rm(i)
  rm(temprow)
  rm(filetoread)
  rm(amexstocks)
  rm(nysestocks)
  rm(nasdaqstocks)
  trainingmatrix <<- adjustedmatrix
#  print(paste("COLUMNCHECKmid: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
  trainingmatrix <<- adjustedmatrix
  #setting this really low so that it causes the MSE for allocations to really hit those bad allocations hard.
  trainingmatrix[]<<--10
  temptrainingmatrix <<- adjustedmatrix
  #originally I was just trying to make it bet on the best stock...penalizing more extreamly for placing bets on secondary etc. but that wasn't having a positive result. still was -.02 return
  #Now I'm thinking I do the same kind of calculation I do for the performance() function
  #This is the "Given what I know, what should I have been allocated in function....
  #simpilest was just all my money in the highest positive return...
  #more complex trading methodology will yield better results.
  #find the top 10 movers for a day positive (change to either direction eventually) and split the allocation among them
  allocation=c(.25,.15,.1,.1,.1,.1,.1,.05,.03,.02)    
  for (i in (1:nrow(adjustedmatrix))){
    #print(trainingmatrix[i,])
    #find the max and set it's location to 1st, then 0 it out and latter allocate it as .25
    #this logic sucks, but for now...
    for(a in (allocation)){
      placetosetintrainingset<<-which.max(temptrainingmatrix[i,])
      trainingmatrix[i,placetosetintrainingset]<<-a
      temptrainingmatrix[i,placetosetintrainingset]<--5000
    }
  }
  rm(temptrainingmatrix)
  
  #This section is to combine the training output matrix with the percentchangedcombined matrix before cleaning it and splitting it for training and evaluation
  
  colnames(trainingmatrix) <<- sub('Adjusted','output',colnames(trainingmatrix))
  
  #print(paste("COLUMNCHECKgeneratingtraining: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  diminputpercentagematrix<<-dim(percentchangedcombined)
  rm(percentchangedcombinedtemp)
  percentchangedcombinedtemp <<- merge.data.frame(percentchangedcombined,trainingmatrix, by=0, all = TRUE)
  rownames(percentchangedcombinedtemp)<<-percentchangedcombinedtemp[,1]
  percentchangedcombinedtemp <<- percentchangedcombinedtemp[,-1]   #strip date now that it is the row name
  percentchangedcombined <<- percentchangedcombinedtemp
  percentchangedcombined <<- head(percentchangedcombined,-1)    #strip last row since it is crap
  #print(paste("COLUMNCHECKgeneratingtraining2: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  seventyfive=as.integer(nrow(percentchangedcombined)*.75)
  
  is.nan.data.frame <- function(x){
    do.call(cbind, lapply(x, is.nan))
  }
  
  is.na.data.frame <- function(x){
    do.call(cbind, lapply(x, is.na))
  }
  
  is.infinite.data.frame <- function(x){
    do.call(cbind, lapply(x, is.infinite))
  }
  percentchangedcombined[is.nan.data.frame(percentchangedcombined)] <- -10
  percentchangedcombined[is.na.data.frame(percentchangedcombined)] <- -10
  percentchangedcombined[is.infinite.data.frame(percentchangedcombined)] <- -10
  
  percentchangedcombined_train <<- head(percentchangedcombined,seventyfive)
  percentchangedcombined_eval <<- head(percentchangedcombined,-seventyfive)
  
  
}
