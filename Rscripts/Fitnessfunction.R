#Doesnt' look like I'm going to be using this one.....



# output of NN is each node represents the % commitment to that stock. 0-1
# Calculation is all stocks with a valid % change
# Add them together for the total commitment
# Divide an individual stocks commitment by total comitment gives the % of portfolio for that day
# Each stocks % change for each day needs stored seperately
# 0 to IPO equals no change.
# % change total
# higher the % chagne the better
#

fitnesscalc <-function(todayschanges,outputnodes)
  {
#  print(outputnodes*todayschanges)
fitness=sum(outputnodes*todayschanges,na.rm = TRUE)
return(fitness)
}

#Little test function
#outputnodes<-c(1,1,1,1,1,1,1,1,1,1)
#length(outputnodes)
#testmatrix = head(adjustedmatrix)
#testmatrixdim = dim(testmatrix)
#fitnesslist = c(1:testmatrixdim[1])
#
#for (i in (fitnesslist))
#{
#  print(i)
#  fitnesslist[i] = fitnesscalc(adjustedmatrix[i,],outputnodes)
#}
