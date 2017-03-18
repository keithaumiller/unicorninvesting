library(quantmod)
library(RMySQL)

#You have to create a mysql server instance with a db named unicorn for this line to work.
mydb = dbConnect(MySQL(), user='root', password='password', dbname='unicorninvesting', host='127.0.0.1')


#other useful provided files.
#amex = read.csv('data/exchangedata/amex.csv')  # read csv file
#amexstocks = amex[,1]
#nasdaq = read.csv('data/exchangedata/nasdaq.csv')  # read csv file
#nasdaqstocks = nasdaq[,1]
#nyse = read.csv('data/exchangedata/nyse.csv')  # read csv file
#nysestocks = nyse[,1]


#TODO:  Add logic to update the \^ stocks and strip spaces out of the symbol names

#rm(amex,nasdaq,nyse)
downloadcurrency <- function(x){
  #x = 'ZARTWD'
  symbol_name = x
  #currencylist = paste(substr(currencylist[,],1,3), substr(currencylist[,],4,6),sep = '/')
  usablecurrency = paste(substr(symbol_name,1,3), substr(symbol_name,4,6),sep = '/')
  writeable = getFX(usablecurrency)
  writelocationarray = c('data/stockdata/', symbol_name,'/')
  writelocation = paste(writelocationarray, sep = '/', collapse='')
  thisforex = data.frame(get(writeable))
#  newmatrix = cbind(rownames(thisforex),thisforex)
#  colnames(newmatrix) <- c("Date",colnames(thisforex))
#  thisforex = newmatrix

  #yes, I know that techincally this isn't an "Adjusted" price... but I need it to be that format so it works in the rest of the system and can reuse the stock code
  updatedcolname=paste(symbol_name,".Adjusted", sep = '')
  colnames(thisforex) <- updatedcolname
  dir.create(writelocation, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  writelocationarray = c(writelocationarray, "stockdata.csv")# removing to reduce, sessionlabel)
  writelocation = paste(writelocationarray, sep = '/', collapse='')
  write.csv(thisforex, file=writelocation)
}

downloaddata <- function(x){
    symbol_name = x
      getSymbols(symbol_name)
      symbol = get(symbol_name)
      #sessionlabel = paste(symbol_name, Sys.Date(), sep = '_', collapse = '_')
      adjusted = data.frame(adjustOHLC(symbol, symbol.name=symbol_name))

    writelocationarray = c('data/stockdata/', symbol_name,'/')
    writelocation = paste(writelocationarray, sep = '/', collapse='')
    dir.create(writelocation, showWarnings = FALSE, recursive = TRUE, mode = "0777")
    writelocationarray = c(writelocationarray, "stockdata.csv")# removing to reduce, sessionlabel)
    writelocation = paste(writelocationarray, sep = '/', collapse='')
    write.csv(adjusted, file=writelocation)
}


pullstocklist <- function(x) {

initialstocklist = c(as.vector(x))
currencylist = read.csv('data/exchangedata/FOREX.csv')[,1]
#so we don't try to download the currency converstions
stocklist = setdiff(initialstocklist, currencylist)


count = 0
for (i in (stocklist))
{
  print(paste(count, "of", length(stocklist), sep = " "))
  count = count + 1
#  print(i)
  tryCatch({
    downloaddata(i)
  },
  error = function(e){
    cat('ERROR Downloading\n')
  },
  warning 
  )
#  print("Completing Loop\n")
}


count = 0
for (curencys in currencylist)
{
  print(paste(count, "of", length(currencylist), sep = " "))
  count = count + 1
  print(curencys)
  tryCatch({
    downloadcurrency(curencys)
  },
  error = function(e){
    print(paste('ERROR Downloading currency:', curencys, e, sep = ' '))
  },
  warning 
  )
  #  print("Completing Loop\n")
}

#add a USD to USD FOREX stock, just so we can make sure that we have it as a BASE that everything can be converted to at EOD
#mainly this is just to make sure that it has the current dates etc.
USDtoUSDdirectory = "./data/stockdata/USDUSD/"
if(!dir.exists(USDtoUSDdirectory)){
  dir.create(USDtoUSDdirectory, showWarnings = FALSE, recursive = TRUE, mode = "0777")
}
temp = data.frame(read.csv("data/stockdata/USDEUR/stockdata.csv"))
colnames(temp) <- c("Date", "USDUSD.Adjusted")
rownames(temp) <- temp[,"Date"]
temp = temp["USDUSD.Adjusted"]
temp[,"USDUSD.Adjusted"] = 1
write.csv(temp, file="data/stockdata/USDUSD/stockdata.csv")

}

#loadportfoliolist()
loadportfoliolist <- function(x){
#  print(paste("loadportfoliolist: ", x ))
  filetoread = paste(x, '/portfolio.csv', sep = '/')
  portfoliolist <- read.csv(filetoread,header = FALSE)[,1]
  portfoliolist <- levels(portfoliolist)
  portfoliolist <- gsub(" ", "", portfoliolist)
  portfoliolist <- sort(portfoliolist)
  return(portfoliolist)
}


#loadfeaturelist()
loadfeaturelist <- function(x){
#  print(paste("loadfeaturelist: ", x ))
  filetoread = paste(x, '/featurelist.csv', sep = '/')
  featurelist = read.csv(filetoread,header = FALSE)[,1]
#  print(featurelist)
#  featurelist = readRDS(filetoread)
  featurelist = levels(featurelist)
  portfoliolist = loadportfoliolist(x)
  featurelist = unique(c(portfoliolist,featurelist))
  featurelist = gsub(" ", "", featurelist)
  featurelist = sort(featurelist)
  return(featurelist)
}


#Edit this file to determine what stock list you want to use....
#stocklist = loadfeaturelist()

#convertportfoliolisttodbformat takes something like this and converts it to an object for inserting into the DB
# loadportfoliolist("data/results/runs/Energyportfolio1")
# [1] "AAV"   "CKX"   "CVE"   "ENLC"  "MXC"   "NFX"   "NOG"   "PDS"   "PE"    "SDR"  
# [11] "SDRL"  "TAT"   "TEGP"  "TPLM"  "VNRAP" "YUMA" 

convertportfoliolisttodbformat <- function(userid,portfolio,symbollist){
  formatedportfoliolist=data.frame(userid=userid,portfolio_name=portfolio,symbol=symbollist)
  return(formatedportfoliolist)
}

#takes an object in the correct format and puts it in the table
insert_into_unicorn_portfolios_table <- function(x){
  tablename="unicorn_portfolios"
  dbWriteTable(mydb,tablename,x, field.types = NULL, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
  #need to create some logic here...
  return(0)
}

#pulls a portfolio from the unicorn_portfolios table
pull_from_unicorn_portfolios_table<-function(userid,portfolioname){
  
  res <- dbSendQuery(mydb, paste("SELECT symbol FROM unicorn_portfolios WHERE userid = ", userid, " AND portfolio_name = '", portfolioname,"';", sep = ''))
#    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = as.list(dbFetch(res))
  dbClearResult(res)
  results = sort(results)
  return(results)
}

load_unicorn_best_featurelist <- function(userid,portfolioname){
  res <- dbSendQuery(mydb, paste("SELECT symbol FROM unicorn_best_featurelist WHERE userid = ", userid, " AND portfolio_name = '", portfolioname,"';", sep = ''))
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = as.list(dbFetch(res))
  dbClearResult(res)
  results = sort(results)
  return(results)
}

insert_into_unicorn_best_featurelist <- function(userid,portfolioname,symbollist){
#  symbollist = featurelistforNN2
#  userid = 2
#  portfolioname = portfolionickname

  symbollisttable = convertportfoliolisttodbformat(userid,portfolioname,symbollist)
  #remove the old "Best" featurelist
  res <- dbSendQuery(mydb, paste("DELETE FROM unicorn_best_featurelist WHERE userid = ", userid, " AND portfolio_name = '", portfolioname,"';", sep = ''))
#  results = as.list(dbFetch(res))
  dbClearResult(res)
  
  #insert the new "Best" featurelist
  tablename="unicorn_best_featurelist"
  dbWriteTable(mydb,tablename,symbollisttable, field.types = NULL, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
  #need to create some logic here...
 return(0) 
}