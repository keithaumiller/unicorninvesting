library(DBI)

source('data/db.R')
source('util/log.R')

holding.STOCK <- 'STOCK'
holding.FOREX <- 'FOREX'

holding.get_table <- function (portfolio, type) {
  database    <- db.connect()
  table       <- join(c(db.PREFIX, 'holding'))

  statement   <- join(c("SELECT * FROM ", table, " WHERE portfolioID = '", portfolio$ID, "'", " AND type = '", type, "'"))

  log.info('holding', paste('Executing statement:', statement))

  holding     <- dbGetQuery(database, statement)

  db.disconnect(database)

  return(holding)
}

holding.get    <- function (portfolio, type, params = NULL) {
  holding      <- holding.get_table(portfolio, type)

  database     <- db.connect()

  table        <- join(c(db.PREFIX, 'holding_', sapply(type, tolower)))
  statement    <- join(c("SELECT * FROM ", table, " WHERE holdingID = '", holding$ID, "'"))

  if ( !is.null(params) ) {
    columns    <- names(params)

    fcolumns   <- join(paste("`", columns, "`", sep = ''), ', ')
    fvalues    <- join(paste("'",  params, "'", sep = ''), ', ')

    statement  <- join(c(statement, " AND (", fcolumns, ") IN ((", fvalues, "))"))
  }

  log.info('holding', paste('Executing statement:', statement))

  tresult      <- dbGetQuery(database, statement)
  tresult$ID   <- NULL
  tresult$type <- NULL

  colnames(tresult)[1] <- "ID"

  holding      <- merge(holding, tresult, key = "ID")

  db.disconnect(database)

  return(holding)
}

holding.add   <- function (portfolio, type, params) {
  values      <- list(
    portfolioID = portfolio$ID,
    type        = type
  )

  log.info('portfolio', paste('Adding a holding of type:', type))

  db.insert('holding', values)

  holding     <- holding.get_table(portfolio, type)

  table       <- join(c('holding_', sapply(holding$type, tolower)))
  values      <- append(list(holdingID = holding$ID), params)

  db.insert(table, values)

  holding     <- holding.get(portfolio, type, params)

  return(holding)
}
