#not sure I need these still, but meh.
is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}
is.na.data.frame <- function(x){do.call(cbind, lapply(x, is.na))}
is.infinite.data.frame <- function(x){do.call(cbind, lapply(x, is.infinite))}

getgloballistofstocks <- function(numbertopullparam){
}

combinestocksfunction <- function(numbertopullparam, featurelistforNN){
mydebug("Combining Stocks")
#numbertopull=numbertopullparam
#datetoread=Sys.Date()
#datetoread="2016-10-16"

  library(quantmod)
  library(plyr)

  
#  amex = read.csv('data/exchangedata/amex.csv')  # read csv file
#  amexstocks = amex[,1]
#  nasdaq = read.csv('data/exchangedata/nasdaq.csv')  # read csv file
#  nasdaqstocks = nasdaq[,1]
#  nyse = read.csv('data/exchangedata/nyse.csv')  # read csv file
#  nysestocks = nyse[,1]
#TODO:  Add logic to updat ethe \^ stocks and strip spaces out of the symbol names\
#rm(amex,nasdaq,nyse)

  #symbolsavailable = list.files(path = 'data/stockdata')
#  portfoliolist = loadportfoliolist()
  stocklist <<- unique(c(portfoliolist,featurelistforNN))
  numberofstockscombined <<- length(stocklist)
  numberofstockscombined_final <<- numberofstockscombined

  stockstocombine <<- data.frame()
#  rm(percentchangedcombined)
  percentchangedcombined <<- data.frame()
#  print(paste("COLUMNCHECK: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
  count = 0
  for (i in (stocklist))
  {
    
    filetoread = paste('data/stockdata/', i, "/", "stockdata.csv", sep = '') #used to be paste(i,datetoread, sep ="_")
#    print(paste(count, "of", numberofstockscombined, "loaded", sep = " "))
#    print(paste("Reading in file :", filetoread, sep=' '))
    tryCatch({
      tempstockdata<-0
#      print(paste(count, filetoread))
      tempstockdata <- read.csv(filetoread, row.names=1, header = TRUE)
#      print(paste(colnames(tempstockdata)))
#      print(paste(colnames(stockstocombine)))    
      if(is.data.frame(stockstocombine) && nrow(stockstocombine)==0){
        stockstocombine <<- tempstockdata
        }
      else{
#          print(dim(stockstocombine))
          stockstocombine <<- merge.data.frame(stockstocombine,tempstockdata, by="row.names", all.x = TRUE, all.y = TRUE)
          rownames(stockstocombine) <<- stockstocombine[,1]
          stockstocombine <<-stockstocombine[,-1]
#          print(dim(stockstocombine))
          }
#      print(paste("rownames: ", head(rownames(stockstocombine))))

    },
    error = function(e){
      warning('ERROR Reading File\n', call. = TRUE)
      numberofstockscombined_final <-- numberofstockscombined_final-1

      # remove stock i from both featurelist and portfolio list since we don't have the data
      mydebug(length(portfoliolist))
      mydebug(length(featurelist))
      portfoliolist <<- portfoliolist[portfoliolist != i]
      featurelist <<- featurelist[featurelist != i]
      mydebug(length(portfoliolist))
      mydebug(length(featurelist))
    },
    warning 
    )
    
    #  print(head(tempstockdata))
#      print(combined)
    count = count + 1
    #  print(i)

    #  print(head(tempsymbolholder))
    #  print(is.data.frame(tempsymbolholder))
    #  stockstocombine[i] <- cbind.data.frame(tempsymbolholder)
  }
  
#  Cleaning out all of the NA/Infinite/Nan so that when I generate the percent changed they are accurate.



    stockstocombine[is.nan(stockstocombine)] <<- 0
  stockstocombine[is.na(stockstocombine)] <<- 0
  stockstocombine[is.infinite(stockstocombine)] <<- 0
  
#  print("Stock Frames Combined")
#  print(paste("COLUMNCHECK: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))

#  Moving this logic into the loop so that the merg works right.
#  names(stockstocombine)[names(stockstocombine)=="X"] <<- "Date"
#  row.names(stockstocombine)<<- stockstocombine$Date
#  stockstocombine <<- stockstocombine[,-1]   #strip date now that it is the row name

    percentchangedcombined <<- stockstocombine[-1,]  # strip the first record for the percentchanged matrix
  combinedstockdatadimensions <<- dim(stockstocombine)
  #add a dummy record to the end of the percentchangematrix
 temprow <- c(1:combinedstockdatadimensions[2])
  percentchangedcombined<<-rbind(percentchangedcombined,temprow)
  percentchangedcombined<<-(percentchangedcombined/stockstocombine -1)
  

#  print(paste("COLUMNCHECKmid2: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))

  adjustedcolumnnames <<- grep('Adjusted',colnames(percentchangedcombined),value =TRUE)
# print(length(adjustedcolumnnames))
  portfoliolistcolumnnames <<- vector()

    for (portfoliostock in (portfoliolist)){
    portfoliostockdot = paste(portfoliostock, '.', 'Adjusted', sep = '')
    portfoliolistcolumnnames <<- c(portfoliolistcolumnnames,portfoliostockdot)
    mydebug(portfoliostock)
    #print(length(portfoliolistcolumnnames))
    #print(portfoliolistcolumnnames)
    }
  mydebug(length(portfoliolistcolumnnames))
  mydebug(portfoliolistcolumnnames)
  
  adjustedmatrix <<- data.frame()
  adjustedmatrix <<- percentchangedcombined[,portfoliolistcolumnnames]
#  print(head(adjustedmatrix))

  rm(tempstockdata)
  rm(i)
  rm(temprow)
  rm(filetoread)
#  rm(amexstocks)
#  rm(nysestocks)
#  rm(nasdaqstocks)
#  print(paste("COLUMNCHECKmid: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  
#  trainingmatrix <<- adjustedmatrix
  
  #This is the output matrix utilized for training the neural net.... I think this needs to move to the performance function somehow.
  #setting this really low so that it causes the MSE for allocations to really hit those bad allocations hard.
#  temp = dim(adjustedmatrix)[1]
  trainingmatrix <<- adjustedmatrix
  colnames(trainingmatrix) <<- colnames(adjustedmatrix)
  trainingmatrix <<- generatetrainingmatrix(adjustedmatrix)
  mydebug("Training Matrix Completed:")
  
  #This section is to combine the training output matrix with the percentchangedcombined matrix before cleaning it and splitting it for training and evaluation
  colnames(trainingmatrix) = sub('Adjusted','output',colnames(trainingmatrix))
#  print(head(trainingmatrix))
  
  #print(paste("COLUMNCHECKgeneratingtraining: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  diminputpercentagematrix<<-dim(percentchangedcombined)
#  rm(percentchangedcombinedtemp)
  percentchangedcombinedtemp <<- merge.data.frame(percentchangedcombined,trainingmatrix, by=0, all = TRUE)
  rownames(percentchangedcombinedtemp)<<-percentchangedcombinedtemp[,1]
  percentchangedcombinedtemp <<- percentchangedcombinedtemp[,-1]   #strip date now that it is the row name
  percentchangedcombined <<- percentchangedcombinedtemp
  percentchangedcombined <<- head(percentchangedcombined,-1)    #strip last row since it is crap
  #print(paste("COLUMNCHECKgeneratingtraining2: ",grep("AKR.Adjusted",colnames(percentchangedcombined)), sep = ''))
  seventyfive=as.integer(nrow(percentchangedcombined)*.75)
  twentyfive=as.integer(nrow(percentchangedcombined)-seventyfive)
 
# Originally I didn't want to handle the strange data scenarios data so I did this.  
# In order to get rid of this we'll need to come up with a beter obj_func for the training of the NN
# Currently the MSE calculation craps out when it runs into non numbers.
# Hopefully setting to 0 will have minimal impact in other functions but still serve our purpose for now.
# Maybe if I do this up front to the original combined instead?
 percentchangedcombined[is.nan(percentchangedcombined)] <- 0
 percentchangedcombined[is.na(percentchangedcombined)] <- 0
 percentchangedcombined[is.infinite(percentchangedcombined)] <- 0
  
  percentchangedcombined_train <<- head(percentchangedcombined,seventyfive)
  percentchangedcombined_eval <<- tail(percentchangedcombined, twentyfive)
#  print("Exiting Combine Stocks Function")
  
  return(numberofstockscombined_final)
}
