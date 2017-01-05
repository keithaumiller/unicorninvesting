# Depricating this for now.  Manually maintain the featureslist.csv instead.

rebuildstocklistfeatures <- function() {
  stocklist <- read.csv('data/exchangedata/stockstouse.csv')[,1]
  stocklist <- levels(stocklist)
  #Each stock should be considered a feature.  If certain characteristics of stocks are generally considered not needed they will be exclueded at the net level.
#  stockfeatures <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
#  tableofcombos <-  expand.grid(stocklist,stockfeatures)
#  stockfeatureslist <- paste(tableofcombos$Var1,tableofcombos$Var2, sep = '.')
  #Write out to directory
#  write(stockfeatureslist, file = 'data/features/featureslist.csv')
  write(stocklist, file = 'data/features/featureslist.csv')
  # Add addtional data sources below this line to add the fetures list to the file to be pulled into the Global features list search 
  # Or just add them manually yourself to the featureslist file.. ;)
  #If you run this function you will wipe all of them out though.
  
  

}
