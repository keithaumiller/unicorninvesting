library(taRifx) # adds shift() functionality
library(PerformanceAnalytics)  # will be used at some point in the future... after I learn it.

#mlpeval_eval is the output of the NN, adjustedinput is the "Adjusted" values for the stock
modelperformance <- function(mlpeval_eval,adjustedinput,saveit)
{
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
  runningtotal = seedmoney  # updated daily in the loop to show how much there is to spread the next day

  adjustedmatrix_eval<-adjustedinput # our matrix of % change values for the day
  modelallocation<-mlpeval_eval # our alogrithms output how it wants money allocated at the beginning of the next day

  #adjusted the modelallocation to make sure that each row sums up to 100% allocation and not more than 100%
   modelallocation[]<-mlpeval_eval[]/rowSums(mlpeval_eval)
  
   # calculate the portfolio weight percent changed matrix  
   #using the previous EOD allocations with current days final numbers 
   #to get the % change on on the previous EOD balance
  portfolioweightwithpercentchanged <- (modelallocation[1:datasetlength-1,]) * (adjustedmatrix_eval[2:datasetlength,])

  # trim the matrix down to the last X days you want to measure performance on.  
  # maybe I should do this when constructing the datasets upfront..
  portfolioweightwithpercentchanged <- tail(portfolioweightwithpercentchanged,daystouse)

  #loop through to update the runningbalance for the portfolio during the timeperiod
  #can't do this as a matrix calcuation since each row is dependant on the previous days results
      for (x in 1:dim(portfolioweightwithpercentchanged)[1]){
      # each day's return is each stocks 
      # portfolioweightwithpercentchange from above, 
      #  * the runningtotal 
      # and then Sum them all up
      thisdaysreturn = sum((portfolioweightwithpercentchanged[x,]) * (runningtotal)) 
#       if (thisdaysreturn/runningtotal > 1.01)
#       {
#         print("Thisdays cals")
#         print(paste("thisdaysreturn = sum((portfolioweightwithpercentchanged[x,]) * (runningtotal))"))
#         print(paste("thisdaysreturn:", thisdaysreturn))
#         print(paste("Altthisdaysreturn:", sum((portfolioweightwithpercentchanged[x,])) * (runningtotal)))
#         print(paste("portfolioweightwithpercentchanged: ", sum(portfolioweightwithpercentchanged[x,]), "runningtotal:", runningtotal))
#       }
      #update the running total for the next iteration in the loop
      runningtotal = thisdaysreturn
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



