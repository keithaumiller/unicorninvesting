# This is where I will need to implement: https://en.wikipedia.org/wiki/Modern_portfolio_theory
# Currently modelperformance is simplistic at best and is heavily dicted by how the training output is structured.
# I'm not a fan of rollup metrics like "Risk Level" but it could be a good feature.

modelperformance <- function(mlpeval_eval){
  
#these three lines just establish the % allocate for each stock given the output from the NN
adjustedmatrix_eval<-evalmatrix[,portfoliolistcolumnnames]
modelallocation<-mlpeval_eval
modelallocation[]<-(mlpeval_eval[]/rowSums(mlpeval_eval))

#now that I have the portfolio allocation I need to bounce it against what the market actually did that day and give a Total return on the day.
# formula for return = allocation * % change in a day * amount invested.

#modelallocation<-modelallocation*adjustedmatrix_eval
daystouse = 365 # make sure you are only using X days for the total return calculation
seedmoney = 1000
runningtotal <- seedmoney #seed money
evalperformance<-modelallocation*adjustedmatrix_eval
evalperformance <- tail(evalperformance,daystouse) 

for (daysreturn in evalperformance){
  runningtotal <- runningtotal + (daysreturn * runningtotal)
#  print(paste("Daysreturn: ", daysreturn, " Runningtotal: ", runningtotal, sep = ''))
  
}


#So this is the old method of just totaling everything in the matrix and considering it the performance....
# I think instead I'll div by number of rows to give better pic of the per day average return.
# In theory if that is positive you are good... but in reality order matters.
#rowSums(evalperformance)

#performance=sum(evalperformance)/nrow(mlpeval_eval)
performance = runningtotal
paste("Performance: ",performance)
print(paste("Total return on ", seedmoney, " after ", daystouse, " : ", runningtotal, sep = ''))
return(performance)
}

#Why this is important... This is the function that your Neural Net is evaluated based off of.
#Take the input of X features, and output a matix of actions that the net should have taken given that input.
#i.e. avoid huge drops, and allocate a % to high upticks.
#this training generation should intheory get really complicated based on what you think would be the best move.
#This could also be fed into a seperate feedback loop. ;)
#this output should be what % of the portfolio should be allocated to that stock that is in the portfolio

generatetrainingmatrix <-function(trainingmatrix){
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