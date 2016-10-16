#uncomment and run the next line if you are getting library not found errors
#install.packages("quantmod","FCNN4R","plyr","GA","forecast")

#clean your environment
rm(list = ls())

if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")

#downloads the 8 stocks that are default in the stocklist.csv
#this saves the stock data to the data/stockdata directory structure for reference
pullstocklist()


# This is the model explorer script.  Modify this function in modelexploration.R 
#to play around with the NN model. i.e. add or remove layers, 
#change learning algorythm etc. all of it is in predictiveanalytics\modelexploration.R
# I would like to put this in a config script at some point, but not worth it currently.
modelexplorer()

