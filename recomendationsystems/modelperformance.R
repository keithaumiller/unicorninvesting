
modelperformance <- function(mlpeval_eval) {
adjustedmatrix_eval<-evalmatrix[,adjustedcolumnnames]
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