library(taRifx) # adds shift() functionality
library(PerformanceAnalytics)  # will be used at some point in the future... after I learn it.

conversionlookup <- function(fromcurency,tocurrency,date){
  transactiontype = paste(fromcurency,tocurrency,".Adjusted",sep = '')
  conversionrate = stockstocombine[date,transactiontype]
  return(conversionrate)
}

convertportfoliotoUSD <- function(portfolio,date,currencylist){
  
  #      print("Finalday")
  #      print(balancematrix[i,])
#  portfolio = balancematrix[i,]
#  date = "2016-10-29"
  tempbalance = portfolio
  tocurrency = "USD"
  for(eachcurrency in currencylist)
  {
    #        print(eachcurrency)
    fromcurrency = eachcurrency
    conversionrate = conversionlookup(fromcurrency,tocurrency,date)
    amounttotransferfrom = (as.double(portfolio[fromcurrency])) 
    amounttotransferto = amounttotransferfrom * as.double(conversionrate)
    tempbalance[fromcurrency] <- tempbalance[fromcurrency] - amounttotransferfrom
    tempbalance[tocurrency] <- tempbalance[tocurrency] + amounttotransferto
#    print(tempbalance)
  }
#  print(tempbalance[tocurrency])
  return(tempbalance[tocurrency])
}

forexperformance <- function(mlpeval_eval,adjustedinput,saveit){

#  mlpeval_eval = objectivefunctionnetoutput
#  adjustedinput = input[,portfoliolistcolumnnames]
#  saveit = FALSE

#  conversionmatrix = 
  
  datasetlength <- dim(adjustedinput)[1]  # how many days of data do I have
  daystouse = 252 #252  # use only the final X days... shortens the execution, 252 is how many exchange days there are in a year
  if(daystouse > datasetlength)
  {
    daystouse = datasetlength
  }

  seedmoney = 1000  # money to invest
  print("Seedmoney set")
  
  # portfolio.csv will have the full list of curency combinations in it.
  #So....the actual values of "stockstocombine" contains the currency conversion info
  #first three letters are the from currency
  #second three letters are the to currency
  #so these pairs are representative of a transaction type, not a holding
  #translate the transaction transaction types to a currency holding by starting with USD Base
  #Convert to other currencies
  #Generate currency portfoliolist
  
  currencylist = unique(c(substr(portfoliolistcolumnnames,4,6), substr(portfoliolistcolumnnames,1,3)))
  transactionslist = unique(substr(colnames(stockstocombine),1,6))
  balancematrix = matrix(0,nrow=daystouse,ncol=length(currencylist))
  colnames(balancematrix) <- currencylist
  rownames(balancematrix) <- tail(rownames(adjustedinput),daystouse)
#print(balancematrix)
  balancematrix[1,'USD'] = seedmoney
print
  currencypairs = substr(portfoliolistcolumnnames,1,6)
  currencypairpushratios = currencypairs
  currencypairpushratios[] = 0
  allocation = tail(mlpeval_eval,daystouse)
  colnames(allocation)<- currencypairs
  
  print("after allocation set")
  
  #normalize the allocations per day
#  normalizedallocation = allocation
#  normalizedallocation[] = normalizedallocation[]/rowSums(allocation)

  for (i in 1:dim(balancematrix)[1]){
#    i=1
    cppad = currencypairs # currencypairpercentpushanddirectionholder
    if (i==1){
      tempbalance = balancematrix[i,]
    }
    else{
      tempbalance = balancematrix[i-1,]
    }
#    print("NEXTROW")
#    print("###################")
    
    for (j in 1:length(currencypairs)){
#      print(tempbalance)
      thistransaction = currencypairs[j]
      fromcurrency = substr(thistransaction,1,3)
      tocurrency = substr(thistransaction,4,6)
      inversetransaction =  paste(tocurrency,fromcurrency,sep='')
#      print(inversetransaction)
      cppad[j]= allocation[1,thistransaction] - allocation[1,inversetransaction]
      if (as.double(cppad[j]) < 0){
        cppad[j] = 0
      }
#      print(cppad[j])
#      print(rownames(balancematrix)[i]) # date we are doing...
      conversionrate = conversionlookup(fromcurrency,tocurrency,rownames(balancematrix)[i])
#      print(paste("converstionrate: ", conversionrate, sep = ''))
      amounttotransferfrom = (as.double(cppad[j]) * as.double(balancematrix[i,fromcurrency])) 
      amounttotransferto = amounttotransferfrom * as.double(conversionrate)
      tempbalance[fromcurrency] = tempbalance[fromcurrency] - amounttotransferfrom
      tempbalance[tocurrency] = tempbalance[tocurrency] + amounttotransferto
      balancematrix[i,] <- tempbalance
          #        if (i == 1){
          # print(thistransaction)
          # print(paste("Currency pair push and direction: ",as.double(cppad[j]),sep = ''))
          # print(paste("Converstion rate: ", as.double(conversionrate), sep = ""))
          # print(paste(fromcurrency, tempbalance[fromcurrency], sep = ':'))
          # print(paste(tocurrency, tempbalance[tocurrency], sep = ':'))#        } 
    }
    if(saveit == TRUE)
    {
      NNperformancechart <<- c(NNperformancechart,convertportfoliotoUSD(balancematrix[i,],rownames(balancematrix)[i],currencylist)) 
    }
#    print("middle")
#    print(i)
#    print(dim(balancematrix)[1])
    if (i == daystouse)
    {
      finalUSDValue = convertportfoliotoUSD(balancematrix[i,],rownames(balancematrix)[i],currencylist)
      balancematrix[i,] = 0
      balancematrix[i,"USD"] = finalUSDValue
      #  print(tail(balancematrix,5))
      USDvaluetoreturn = tail(balancematrix[,"USD"],1)
        if (USDvaluetoreturn == 1000)
        {
 #         print(paste("Finalday:", daystouse,Sys.time(),USDvaluetoreturn,saveit, sep = ' '))
          return(900)
#          print(tail(balancematrix))
#          print(head(balancematrix))
#          print(head(mlpeval_eval))
#          print(head(adjustedinput))
        }
      return(USDvaluetoreturn)
    }
  }
#  print(tail(balancematrix,5))
  USDvaluetoreturn = tail(balancematrix[,"USD"],1)
#  print(paste(Sys.time(),USDvaluetoreturn,saveit, sep = ' '))
  return(USDvaluetoreturn)
}

#mlpeval_eval is the output of the NN, adjustedinput is the "Adjusted" values for the stock
modelperformance <- function(mlpeval_eval,adjustedinput,saveit)
{
  #if this is a forex portfolio...different performance eval than normal
  if (file.exists(paste(outputdirectory,"/isforex",sep = '')))
  {
    
    forexreturn = forexperformance(mlpeval_eval,adjustedinput,saveit)
    print(forexreturn)
    return(forexreturn)
  }
  
#  print("In new model performance")
#  mlpeval_eval = objectivefunctionnetoutput
#  adjustedinput = input[,portfoliolistcolumnnames]
  
  #parameters Explained
  #mlpeval_eval  ---- this is a matrix of the output from the NN i.e.
  #          Stock.IBM.Weight    Stock.Yahoo.Weight
  #Day 1         .8                     .6
  #Day 2         .7                     .9
  #Day 3         .7                     .2
  #adjustedinput  --------this is the matrix of the % change for the stocks from the previous day
  #             IBM.Adjusted           Yahoo.Adjusted
  #Day 1         1.02                   1.03
  #Day 2         1.03                   .90
  #Day 3         .98                    .94
  #
  #saveit   ----parameter on if to save the progression of the nets performance over the days
  #--- I disable it for training
  
  
  datasetlength <- dim(adjustedinput)[1]  # how many days of data do I have
  balance <<- vector(mode = "double", length = datasetlength)  # not used
  daystouse = 252 #252  # use only the final X days... shortens the execution, 252 is how many exchange days there are in a year
  seedmoney = 1000  # money to invest 
  runningtotal <<- seedmoney  # updated daily in the loop to show how much there is to spread the next day

  adjustedmatrix_eval<-adjustedinput # our matrix of % change values for the day
  modelallocation<-mlpeval_eval # our alogrithms output how it wants money allocated at the beginning of the next day

  #adjusted the modelallocation to make sure that each row sums up to 100% allocation and not more than 100%
   modelallocation[]<-mlpeval_eval[]/rowSums(mlpeval_eval)
  
   # calculate the portfolio weight percent changed matrix  
   #using the previous EOD allocations with current days final numbers 
   #to get the % change on on the previous EOD balance
  portfolioweightwithpercentchanged <- (modelallocation[1:datasetlength-1,]) * (adjustedmatrix_eval[2:datasetlength,])

  # trim the matrix down to the last X days you want to measure performance on when doing X validation
  # maybe I should do this when constructing the datasets upfront..
    if(saveit ==TRUE){
      portfolioweightwithpercentchanged <- tail(portfolioweightwithpercentchanged,daystouse)
    }

  #loop through to update the runningbalance for the portfolio during the timeperiod
  #can't do this as a matrix calcuation since each row is dependant on the previous days results
      for (x in 1:dim(portfolioweightwithpercentchanged)[1]){
 #     print(paste(dim(portfolioweightwithpercentchanged)[1]))
      # each day's return is each stocks 
      # portfolioweightwithpercentchange from above, 
      #  * the runningtotal 
      # and then Sum them all up
#        portfolioweightwithpercentchanged
        sumofpwwpc <<- sum((portfolioweightwithpercentchanged[x,]))
        thisdaysreturn <<- sumofpwwpc * runningtotal
#       if (thisdaysreturn/runningtotal > 1.01)
#       {
#           print("Thisdays calcs")
#           print(paste("thisdaysreturn = sum((portfolioweightwithpercentchanged[x,]) * (runningtotal))"))
#            print(paste("runningtotal:", runningtotal))
#            print(paste("sumofpwwpc:", sumofpwwpc))
#            print(paste("THISDAYSRETURN:", thisdaysreturn))

#            print(paste(dim(portfolioweightwithpercentchanged)[1])))

#           print(paste("portfolioweightwithpercentchanged: ", sum(portfolioweightwithpercentchanged[x,]), "runningtotal:", runningtotal))
#           print(paste(portfolioweightwithpercentchanged[x,]))
#           print(paste("model:",modelallocation[x,]))
#           print(paste("adjustedmatrix:", adjustedmatrix_eval[x+1,]))
#      }
      #update the running total for the next iteration in the loop
      runningtotal <<- thisdaysreturn
#      print(paste("X: ", x, " Thisdaysreturn: ", thisdaysreturn,  " Runningtotal: ", runningtotal, sep = ''))
#      saveit = TRUE
#      If this is a performance test and not training, it'll save it to this vector and plot it later'
      if(saveit == TRUE)
         {
             NNperformancechart <<- c(NNperformancechart,runningtotal) 
       }
    }

  thisfunctionsperformance = runningtotal
  return(thisfunctionsperformance)  
}




#Why this is important... This is the function that your Neural Net is evaluated based off of.
#Take the input of X features, and output a matix of actions that the net should have taken given that input.
#i.e. avoid huge drops, and allocate a % to high upticks.
#this training generation should intheory get really complicated based on what you think would be the best move.
#This could also be fed into a seperate feedback loop. ;)
#this output should be what % of the portfolio should be allocated to that stock that is in the portfolio

generatetrainingmatrix <-function(trainingmatrix){
  ###THIS doesn't get used anymore, but I just haven't removed it from datasets because it's a pain in the ass

    temptrainingmatrix <<- trainingmatrix
  temptrainingmatrix[,] <<- NA
  #more complex trading methodology will yield better results.... 
  #I need more levers and this is how they get fed back into the training
  #find the top 10 movers for a day positive (change to either direction eventually) and split the allocation among them
  allocation=c(.25,.15,.1,.1,.1,.1,.1,.05,.03,.02)    
  for (i in (1:nrow(trainingmatrix))){
    #print(trainingmatrix[i,])
    #find the max and set it's location to 1st, then 0 it out and latter allocate it as .25
    #this logic sucks, but for now...
    for(a in (allocation)){
      placetosetintrainingset<<-which.max(trainingmatrix[i,])
      temptrainingmatrix[i,placetosetintrainingset] <<- a
      #so we can make sure that we are not just setting the same one over and over.
      trainingmatrix[i,placetosetintrainingset]= -5000
#      print(paste("Now Setting:", a, "for", i, "at", placetosetintrainingset, sep=" "))
    }
  }
  return(temptrainingmatrix)
}


trainingobjectivefunction <- function(nettotrain,inputmatrix,outputmatrix){
  
  return(score)
}



