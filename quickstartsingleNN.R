#uncomment and run the next line if you are getting library not found errors
#install.packages("quantmod","FCNN4R","plyr","GA","forecast")

#clean your environment
rm(list = ls())

#While I'm playing with performance... good to chart it on each call


#setwd("/home/keith/unicorninvesting/unicorninvesting")

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")


#downloads the stocks that are default in the featurelist.csv
#this saves the stock data to the data/stockdata directory structure for reference
#pullstocklist()

#loads your list of features from data/features/featurelist.csv
#Currently this just pulls the 6 basic data points of EOD stats for the list of stocks and uses them as features.
#loads your portfoliolist from data/exchangedata/portfolio.csv
#just a list of stock assets... These will also be added to the featurelist if not already in there.
portfolionickname <<- 'forex1'
outputdirectory <<- paste("data/results/runs/", portfolionickname, sep = "")
featurelist <<- loadfeaturelist(outputdirectory)
featurelist <<- head(featurelist,40)
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

for (i in 1:20){
performance = modelexplorer(1,featurelist)
  

#check the plots directory for your charts.

  runid <<- gsub(" ", "-", gsub(":", "-", Sys.time())) #generates an identifer to trace through the stack in the format of "2016-11-02-12-16-11"
  plotsdirectory = paste(outputdirectory,"/plots",sep = '')
  dir.create(plotsdirectory)
  myfilesavelocation = paste("./", outputdirectory, "/plots/NNrunid-" ,runid, i , "-netperformance.png", sep = '')
  png(filename = myfilesavelocation, width = 900, height = 900)
  plot(NNperformancechart)
  # Create a title with a red, bold/italic font
  maintitle = paste("From $1000 to $", round(performance,2), " with ", length(featurelist), " features!", sep = '')
  title(main=maintitle, col.main="Blue", font.main=4)
  # Label the x and y axes with dark green text
  title(xlab= "          Days 1:365", col.lab=rgb(0,0.5,0))
  #    title(ylab= "Total bankroll if starting with 1000$", col.lab=rgb(0,0.5,0))
  dev.off()
}
