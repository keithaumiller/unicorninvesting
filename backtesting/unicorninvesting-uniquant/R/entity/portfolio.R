library(DBI)

source('data/db.R')
source('util/log.R')

portfolio.get <- function (user, name = NULL) {
  database    <- db.connect()
  table       <- join(c(db.PREFIX, 'portfolio'))

  statement   <- join(c("SELECT * FROM ", table, " WHERE userID = '", user$ID, "'"))

  if ( !is.null(name) ) {
    fname     <- join(paste('"', name, '"', sep = ''), ', ')
    statement <- join(c(statement, " AND name IN (", fname, ")"))
  }

  log.info('portfolio', paste('Executing statement:', statement))

  result      <- dbGetQuery(database, statement)

  db.disconnect(database)

  if ( is.empty(result) ) { result <- NA }

  return(result)
}

portfolio.register    <- function (user, name) {
  log.info('user', paste('Registering Portfolio for User: ', user$username))

  values      <- list(
    userID     = user$ID,
    name       = name
  )

  if ( is.true(db.insert('portfolio', values)) ) {
    portfolio <- portfolio.get(user, name)
  } else {
    portfolio <- NULL
  }

  return(portfolio)
}
