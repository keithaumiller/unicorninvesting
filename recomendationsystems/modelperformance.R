# This is where I will need to implement: https://en.wikipedia.org/wiki/Modern_portfolio_theory
# Currently modelperformance is simplistic at best and is heavily dicted by how the training output is structured.
# I'm not a fan of rollup metrics like "Risk Level" but it could be a good feature.

modelperformance <- function(mlpeval_eval) {
adjustedmatrix_eval<-evalmatrix[,portfoliolistcolumnnames]
#merge.data.frame(percentchangedcombined,trainingmatrix, by=0, all = TRUE
#temptotalls=as.data.frame(rowSums(mlpeval_bp))
#tempactualallocation=merge.data.frame(mlpeval_bp,temptotalls,by=0,all=TRUE)
modelallocation=mlpeval_eval
modelallocation[]=(mlpeval_eval[]/rowSums(mlpeval_eval))
modelallocation=modelallocation*adjustedmatrix_eval
evalperformance=modelallocation*mlpeval_eval
performance=sum(evalperformance)
paste("Performance: ",performance)
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