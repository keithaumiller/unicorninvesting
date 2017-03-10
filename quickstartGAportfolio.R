#uncomment and run the next line if you are getting library not found errors
#install.packages("quantmod","FCNN4R","plyr","GA","forecast")

#clean your environment
rm(list = ls())
#setwd("/home/keith/unicorninvesting/unicorninvesting")

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")
if(!exists("launchaGAportfolio", mode="function")) source("./recomendationsystems/GA_parameter_explorer.R")



#loads your list of features from data/features/featurelist.csv
#Currently this just pulls the 6 basic data points of EOD stats for the list of stocks and uses them as features.
#loads your portfoliolist from data/exchangedata/portfolio.csv
#just a list of stock assets... These will also be added to the featurelist if not already in there.
#portfolionickname <<- 'Energyportfolio1'
portfolionickname <<- 'forex1'
outputdirectory <<- paste("data/results/runs/", portfolionickname, sep = "")
featurelist <<- loadfeaturelist(outputdirectory)
portfoliolist <<- loadportfoliolist(outputdirectory)

#downloads the stocks that are default in the featurelist.csv
#this saves the stock data to the data/stockdata directory structure for reference
#I would comment this out after the first run. ;)
stocklist <<- featurelist
#pullstocklist(stocklist)

# This is the model explorer script.  Modify this function in modelexploration.R 
#to play around with the NN model. i.e. add or remove layers, 
#change learning algorythm etc. all of it is in predictiveanalytics\modelexploration.R
# I would like to put this in a config script at some point, but not worth it currently.

############################################################################
#Beautiful thing about how I've set this up is that you can ################
#kill it at any time and it'll pick up from the last generation ############
#as long as you didn''t change the porfolio.csv or features.csv#############
############################################################################

for (i in 1:200){
launchaGAportfolio(portfolionickname)
}
#modelexplorer(1,featurelist)

