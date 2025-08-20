rm(list = ls())
#launch this script with Rscript


# intention is that all of these scripts be called via command line or via Cron Jobs...
# point is that these are end of day processing type activities, or re-occuring activites
# by just making these system calls instead, we take advantage of the fact that they are self contained
# And the fact that the OS already handles parrallel processing
# To move this into a cloud based environment we'll have to get more sophisticated

# 1. Downloading data
# 2. Runing The GA Model generators for each portfolio
# 3. Running the Daily "Best of" for each portfolio to get the portfolios daily allocation changes


# system("Rscript -e 'source(\"your-script.R\")'", wait=FALSE)
# At the end of your script, you may save your objects with save.image() in order to load them later, and notify of its completion with cat():
#   
#   ...
# save.image("script-output.RData")
# cat("Script completed\n\n")

#https://www.r-bloggers.com/passing-arguments-to-an-r-script-from-command-lines/

# List of all the other scripts that need called each night/day for documentation that can run
# If it can run in parrallel then it's a system call to the script.

if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
#if(!exists("rebuildstocklistfeatures", mode="function")) source("./datasetcreation/Generatefeatureslist.R")
if(!exists("pullstocklist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("modelexplorer", mode="function")) source("./predictiveanalytics/modelexploration.R")
if(!exists("loadportfoliolist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("loadfeaturelist", mode="function")) source("./datagathering/downloadstockdata.R")
if(!exists("generatetrainingmatrix", mode="function")) source("./recomendationsystems/modelperformance.R")
if(!exists("mydebug", mode="function")) source("./datacleaning/debugframework.R")
if(!exists("combinestocksfunction", mode="function")) source("./datasetcreation/Combinestocks.R")
if(!exists("Endofdayprocessing", mode="function")) source("./batchjobs/Actiontime.r")
if(!exists("launchaGAportfolio", mode="function")) source("./recomendationsystems/GA_parameter_explorer.R")

library(parallel)
library(doParallel)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
cl = makeCluster(no_cores, type = "FORK")
stocklist = loadfeaturelist()
#  this downloads the daily data and is very burdensome so only uncomment in prod
#  or if you need to download all stocks in the DB featurelist...
pullstocklist(stocklist)

userids = load_unicorn_useridlist()
portfolios = load_unicorn_portfoliolist()

#create userid/potfolio list

#  userids = list.files("./data/results")
foreach (portfolio=t(portfolios)) %dopar% {
  userid=portfolio[1]
  thisportfolio=portfolio[2]
  convertnetresultsintoaction(userid,thisportfolio)
}

foreach (portfolio=t(portfolios)) %dopar% {
  userid=portfolio[1]
  thisportfolio=portfolio[2]
  outputdirectory <<- paste("data/results/",userid,"/", thisportfolio, sep = "")
  launchaGAportfolio(userid,thisportfolio,outputdirectory)
  }
