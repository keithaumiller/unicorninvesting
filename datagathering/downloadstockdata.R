library(quantmod)
library(RMySQL)
library(parallel)
library(multicore)
library(foreach)
library(parallel)
library(doParallel)

# Calculate the number of cores
no_cores <- detectCores()

# Initiate cluster
cl = makeCluster(no_cores, type = "FORK")

#You have to create a mysql server instance with a db named unicorn for this line to work.(closes current connectiosn first)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
mydb = dbConnect(MySQL(), user='unicorn', password='n7gtRLHi', dbname='unicorn', host='ec2-54-85-232-216.compute-1.amazonaws.com')


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
#  x = 'ZARTWD'
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
#    print(x)
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
#    print(writelocation)
}


pullstocklist <- function(x) {

initialstocklist = c(as.vector(x))
currencylist = read.csv('data/exchangedata/FOREX.csv')[,1]
currencylist = levels(currencylist)
#so we don't try to download the currency converstions
stocklist = setdiff(initialstocklist, currencylist)



foreach(i=stocklist)%dopar% {
 tryCatch({
   print(i)
   downloaddata(i)
 },
 error = function(e){
  print(paste("ERROR Downloading:", e))
 },
 warning = function(w){
  print(paste("Warning Downloading:", w))
 }
 )
}


foreach(i=currencylist)%dopar% {
  tryCatch({
#    print(i)
    downloadcurrency(i)
  },
  error = function(e){
    print('ERROR Downloading\n')
    print(e)
  },
  warning = function(w){
    print('Warning Downloading\n')
    print(w)
  }
  )
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
loadportfoliolist <- function(userid,portfolio){
#  userid=1
#  portfolio=1
#  print(paste("loadportfoliolist: ", x ))
#  filetoread = paste(x, '/portfolio.csv', sep = '/')
#  portfoliolist <- read.csv(filetoread,header = FALSE)[,1]
#  portfoliolist <- levels(portfoliolist)
#  portfoliolist <- gsub(" ", "", portfoliolist)
#  portfoliolist <- sort(portfoliolist)
  portfoliolist <- load_from_unicorn_portfolios_table(userid,portfolio)
  return(portfoliolist)
}

#loadfeaturelist()
loadfeaturelist <- function(userid=1, portfolioname=1, maxfeaturestouse=0){
#  maxfeaturestouse=10
#  print(paste("loadfeaturelist: ", x ))
#  filetoread = paste(x, '/featurelist.csv', sep = '/')
#  featurelist = read.csv(filetoread,header = FALSE)[,1]
#  print(featurelist)
#  featurelist = readRDS(filetoread)
#  featurelist = levels(featurelist)
  portfoliolist = loadportfoliolist(userid,portfolioname)
  featurelist = dbReadTable(mydb,"unicorn_universalfeaturelist_daily")
  featurelist = sort(featurelist[[2]])
  if(maxfeaturestouse != 0)
  {
    featurelist =c(featurelist[1:maxfeaturestouse],portfoliolist)
  }
#  print(length(featurelist))
#  featurelist = unique(c(portfoliolist,featurelist))
  featurelist = sort(featurelist)
  featurelist = gsub(" ", "", featurelist)
  return(featurelist)
}


#Edit this file to determine what stock list you want to use....
#stocklist = loadfeaturelist()

#convertportfoliolisttodbformat takes something like this and converts it to an object for inserting into the DB
# loadportfoliolist("data/results/runs/Energyportfolio1")
# [1] "AAV"   "CKX"   "CVE"   "ENLC"  "MXC"   "NFX"   "NOG"   "PDS"   "PE"    "SDR"  
# [11] "SDRL"  "TAT"   "TEGP"  "TPLM"  "VNRAP" "YUMA" 

convertportfoliolisttodbformat <- function(userid,portfolio,symbollist){
  formatedportfoliolist=data.frame(userid=userid,portfolioid=portfolio,symbol=symbollist)
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
load_from_unicorn_portfolios_table<-function(userid,portfolioname){
  
  res <- dbSendQuery(mydb, paste("SELECT symbol FROM unicorn_portfolios WHERE userid = ", userid, " AND portfolioid = '", portfolioname,"';", sep = ''))
#    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = as.list(dbFetch(res))
  dbClearResult(res)
  results = sort(results[[1]])
  return(results)
}

load_unicorn_best_featurelist <- function(userid,portfolioname){
  res <- dbSendQuery(mydb, paste("SELECT symbol FROM unicorn_best_featurelist WHERE userid = ", userid, " AND portfolioid = '", portfolioname,"';", sep = ''))
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = as.list(dbFetch(res))
  dbClearResult(res)
  results = sort(results[[1]])
  return(results)
}

insert_into_unicorn_best_featurelist <- function(userid,portfolioname,symbollist){
#  symbollist = featurelistforNN2
#  userid = 2
  portfolio = portfolioname
  formatedportfoliolist=data.frame(userid=userid,portfolioid=portfolio,symbol=symbollist)
  symbollisttable = formatedportfoliolist
  #remove the old "Best" featurelist
  res <- dbSendQuery(mydb, paste("DELETE FROM unicorn_best_featurelist WHERE userid = ", userid, " AND portfolioid = '", portfolioname,"';", sep = ''))
#  results = as.list(dbFetch(res))
  dbClearResult(res)
  
  #insert the new "Best" featurelist
  tablename="unicorn_best_featurelist"
  dbWriteTable(mydb,tablename,symbollisttable, field.types = NULL, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
  #need to create some logic here...
 return(0) 
}


insert_universalfeaturelist_daily <- function(){
#    forimport ='MAKE SURE YOU SET YOUR forimport'

    tablename="unicorn_universalfeaturelist_daily"
    dbWriteTable(mydb,tablename,forimport, field.types = NULL, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
    #need to create some logic here...
    return(0)
}

insert_into_unicorn_allocationhistory <- function(userid,portfolio,allocationtable){
#  library(data.table)
  #    forimport ='MAKE SURE YOU SET YOUR forimport'
  now <- as.POSIXlt(Sys.time())
  now.str <- format(now,'%Y-%m-%d %H:%M:%S')
  
  temp= data.frame()
  symbols = colnames(allocationtable)
  datetime = now.str
  allocations = allocationtable[1,]
  
  temp = data.frame("userid"= userid, "portfolioid"= portfolio, "Symbol"= symbols, "datetime" = now.str, "allocation"=allocations)

  forimport=temp
  tablename="unicorn_allocationhistory"
  dbWriteTable(mydb,tablename,forimport, field.types = TRUE, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
  dbGetQuery(mydb,"SHOW WARNINGS")
  #need to create some logic here...
  return(0)
}

load_unicorn_allocationhistory <- function(userid,portfolio,recorddate=NULL){
  library(reshape)
  #this needs to updated to support "Latest" when no date is provided.
  res <- dbSendQuery(mydb, paste("SELECT * FROM unicorn_allocationhistory WHERE userid = ", userid, " AND portfolioid = '", portfolio,"'ORDER BY datetime DESC;", sep = '')) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  dbClearResult(res)
  
  if(is.na(results[1,1])){
    None = data.frame(date=100,holder=100)
    return(None)
  }
  
  #restructure it into what the callers expect. i.e. a portfolio table
  results = recast(results[,3:5],datetime ~ Symbol)
  
  results = results[order(results$datetime,decreasing = TRUE),]
  
  if (is.null(recorddate)){
    print("Recoddate Is Null")
    return(results)
  }
  else{
    date=substr(recorddate,0,10)
    datesresults = results[substr(results$datetime,0,10) == date,][1,]
    return(datesresults)
  }
  return(results)
}

#details is a matrix with the columns in it
insert_into_unicorn_portfolios_details <- function(userid,portfolio,values){
  now <- as.POSIXlt(Sys.time())
  now.str <- format(now,'%Y-%m-%d %H:%M:%S')
  #    forimport ='MAKE SURE YOU SET YOUR forimport'
  
  details = data.frame(matrix(ncol=4,nrow=1))
  names(details)=c("userid","portfolioid","bestperformance","datetime")
  details[1,] = c(userid,portfolio,values,now.str)
  #  details[1,] = c(1,1,-1000,0)
  #  details[1,'datetime']=now.str
  
#  This function needs to be updated to add the datetime iteself to the records

  forimport=details
  tablename="unicorn_portfolios_details"
  dbWriteTable(mydb,tablename,forimport, field.types = NULL, row.names = FALSE, overwrite = FALSE, append = TRUE)#, ..., allow.keywords = FALSE)
  #need to create some logic here...
  return(0)
}

load_unicorn_portfolios_details <- function(userid,portfolio,recorddate=NULL){
  
  #this needs to updated to support "Latest" when no date is provided.
  res <- dbSendQuery(mydb, paste("SELECT * FROM unicorn_portfolios_details WHERE userid = ", userid, " AND portfolioid = '", portfolio,"'ORDER BY datetime DESC;", sep = '')) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  dbClearResult(res)

  if (is.null(recorddate)){
    return(results[1,])
  }
  else{
    date=substr(recorddate,0,10)
    datesresults = results[substr(results$datetime,0,10) == date,][1,]
    return(datesresults)
  }
  return(results)
}

load_unicorn_useridlist <- function(){
  #this needs to updated to support "Latest" when no date is provided.
  res <- dbSendQuery(mydb, paste("SELECT * FROM unicorn_portfolios ORDER BY userid DESC;", sep = '')) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  results = unique(results$userid)
  dbClearResult(res)
  return(results)
}

load_unicorn_portfoliolist<-function(){
  #this needs to updated to support "Latest" when no date is provided.
  res <- dbSendQuery(mydb, paste("SELECT * FROM unicorn_portfolios ORDER BY userid DESC;", sep = '')) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  results = unique(results[,1:2])
  dbClearResult(res)
  return(results)
}

load_unicorn_usersportfolios<-function(userid){
#  mydb = dbConnect(MySQL(), user='unicorn', password='n7gtRLHi', dbname='unicorn', host='ec2-54-85-232-216.compute-1.amazonaws.com')
  #this needs to updated to support "Latest" when no date is provided.
  res <- dbSendQuery(mydb, paste("SELECT * FROM unicorn_portfolios where userid =", userid, " ORDER BY userid DESC;", sep = '')) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  results = unique(results$portfolioid)
  dbClearResult(res)
  if(length(results)==0){results=0}
  return(results)
}

portfolioisforex <- function(userid,portfolioid){
  #this needs to updated to support "Latest" when no date is provided.
  query = paste("SELECT * FROM unicorn_portfolio_attributes where userid =", userid, " and portfolioid =", portfolioid," ORDER BY userid DESC;", sep = '')
  res <- dbSendQuery(mydb, query) #," AND datetime = '", recorddate 
  #    res <- dbSendQuery(mydb, "SELECT symbol FROM unicorn_portfolios WHERE userid = 1 AND portfolio_name = 'EnergyPortfolio1' ;")
  results = dbFetch(res)
  dbClearResult(res)
  tryCatch({
    if(results[,'isforex'] == 1){return(TRUE)}
    else{return(FALSE)}
  },
  error = function(e){
    return(FALSE)
  },
  warning = function(w){
    print('Warning inforexcheck\n')
  }
  )

}
