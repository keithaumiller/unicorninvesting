library(forecast)
#http://www.statmethods.net/advstats/timeseries.html
regressionthists <- function(thists){
  tomorrowsvalue=0
  timeseriesversion = ts(adjustedmatrix[,])
  # simple exponential - models level
  fit <- HoltWinters(timeseriesversion, beta=FALSE, gamma=FALSE)
  # double exponential - models level and trend
  fit2 <- HoltWinters(timeseriesversion, gamma=FALSE)
  # triple exponential - models level, trend, and seasonal components
  fit3 <- HoltWinters(timeseriesversion)
  return(tomorrowsvalue)
}