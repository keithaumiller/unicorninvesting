library(DBI)

source('constant.R')
source('util/utils.R')

source('data/db.R')

load.histdata <- function (pairs = NULL) {
  database    <- db.connect()
  table       <- str_c(db.PREFIX, 'history')

  dirpath     <- file.path(path.CACHE, 'histdata')

  if ( is.null(pairs) ) {
    group     <- '[A-Z]{6}'
  } else {
    deduped   <- unique(pairs)
    group     <- join(c('(', join(deduped, '|'), ')'))
  }

  pattern     <- join(c('HISTDATA_COM_ASCII_', group, '_T.*\\.zip'))

  log.info('load.R', paste('Searching for files of type', pattern))

  zips        <- file.path(dirpath, list.files(path = dirpath, pattern = pattern))

  if ( !is.empty(zips) ) {
      for (i in 1:length(zips)) {
        files     <- unzip(zips[i], list = TRUE)
        csv       <- files$Name[1]

        pair      <- sub('DAT_ASCII_', '', sub('_T_[0-9]{6}.csv', '', csv))

        symbol    <- join(c(str_sub(pair, end = 3), str_sub(pair, start = 4)))

        buffer    <- unz(zips[i], csv)
        data      <- read.csv(buffer)

        colnames(data)   <- c('datetime', 'open', 'close', 'volume')

        data['datetime'] <- lapply(data['datetime'], function (x) {
          strptime(x, format = '%Y%m%d %H%M%S00', tz = 'EST')
        })
        data['symbol']   <- symbol

        log.info('load.R', paste('Writing', csv, 'to table', table))

        tryCatch(
          {
            dbWriteTable(database, table, data, row.names = FALSE, overwrite = TRUE,
              field.types = list(
                datetime  = 'datetime',
                symbol    = 'varchar(6)',
                open      = 'decimal(10, 6)',
                high      = 'decimal(10, 6)',
                low       = 'decimal(10, 6)',
                close     = 'decimal(10, 6)',
                volume    = 'bigint(20)'
              )
            )

            log.info('load.R', 'Write Successful')
          },
          error    = function (error) {
            error  <- paste('Unable to write', csv, 'to the database')
            log.danger('db', error)
          },
          finally  = function ( ) {
            db.disconnect(database)
          }
        )
      }
  } else {
    log.warn('db', 'No cached data found.')
  }
}
