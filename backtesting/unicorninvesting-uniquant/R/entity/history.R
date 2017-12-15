library(DBI)
library(lubridate)

source('data/db.R')
source('util/utils.R')

history.get     <- function (symbol, from = lubridate::origin, to = now()) {
		database    <- db.connect()
  	table       <- join(c(db.PREFIX, 'history'))

  	fsymbol     <- join(paste("'", symbol, "'", sep = ''), ", ")
  	statement   <- paste("SELECT * FROM ", table, " WHERE symbol IN (", fsymbol,
  		") AND datetime BETWEEN '", from, "' AND '", to, "'", sep = '')

  	log.info('history', paste('Executing statement:', statement))

  	data        <- dbGetQuery(database, statement)

  	db.disconnect(database)

  	return(data)
}