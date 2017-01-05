library(quantmod)

#other useful provided files.
#amex = read.csv('data/exchangedata/amex.csv')  # read csv file
#amexstocks = amex[,1]
#nasdaq = read.csv('data/exchangedata/nasdaq.csv')  # read csv file
#nasdaqstocks = nasdaq[,1]
#nyse = read.csv('data/exchangedata/nyse.csv')  # read csv file
#nysestocks = nyse[,1]


#TODO:  Add logic to update the \^ stocks and strip spaces out of the symbol names

#rm(amex,nasdaq,nyse)

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


pullstocklist <- function() {
stocklist = c(as.vector(stocklist))

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
}

#loadportfoliolist()
loadportfoliolist <- function(){
  portfoliolist <- read.csv('data/exchangedata/portfolio.csv')[,1]
  portfoliolist <- levels(portfoliolist)
  portfoliolist <- gsub(" ", "", portfoliolist)
  return(portfoliolist)
}

#loadfeaturelist()
loadfeaturelist <- function(){
  featurelist <- read.csv('data/features/featurelist.csv')[,1]
  featurelist <- levels(featurelist)
  portfoliolist <<- loadportfoliolist()
  featurelist <<- unique(c(portfoliolist,featurelist))
  featurelist <<- gsub(" ", "", featurelist)
  return(featurelist)
}


#Edit this file to determine what stock list you want to use....
stocklist = loadfeaturelist()