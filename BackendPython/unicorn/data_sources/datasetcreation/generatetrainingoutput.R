trainingmatrix = adjustedmatrix

for (i in (1:nrow(trainingmatrix))){
  #print(trainingmatrix[i,])
  temp=which.max(trainingmatrix[i,])
#  print(temp)
  trainingmatrix[i,]=0
  trainingmatrix[i,temp]=1
}

trainingmatrixcolnames = sub('Adjusted','output',colnames(trainingmatrix))
colnames(trainingmatrix)<-trainingmatrixcolnames

diminputpercentagematrix=dim(percentchangedcombined)

percentchangedcombinedtemp = merge.data.frame(percentchangedcombined,trainingmatrix, by=0, all = TRUE)
rownames(percentchangedcombinedtemp)<-percentchangedcombinedtemp[,1]
percentchangedcombinedtemp = percentchangedcombinedtemp[,-1]   #strip date now that it is the row name
percentchangedcombined = percentchangedcombinedtemp
percentchangedcombined = head(percentchangedcombined,-1)    #strip last row since it is crap

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




percentchangedcombined[is.nan.data.frame(percentchangedcombined)] <- 0
percentchangedcombined[is.na.data.frame(percentchangedcombined)] <- 0
percentchangedcombined[is.infinite.data.frame(percentchangedcombined)] <- 0

percentchangedcombined_train = head(percentchangedcombined,seventyfive)
percentchangedcombined_eval = head(percentchangedcombined,-seventyfive)