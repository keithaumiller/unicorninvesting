source('constant.R')
source('util/utils.R')

source('entity/user.R')
source('entity/portfolio.R')
source('entity/holding.R')
source('entity/order.R')
source('entity/trade.R')

source('data/cache.R')
source('back/test.R')

log.DEBUG   <<- TRUE

username    <- 'achillesrasquinha'
password    <- '12345'
firstname   <- 'Achilles'
lastname    <- 'Rasquinha'
email       <- 'achillesrasquinha@gmail.com'
dob         <- '1995-08-14'
gender      <- gender.MALE

portname    <- 'My Portfolio'

if ( !is.true(user.exists(username)) ) {
  log.info('example', paste('Registering User:', username))

  user      <- user.register(
    username  = username,
    firstname = firstname,
    lastname  = lastname,
    email     = email,
    password  = password,
    dob       = dob,
    gender    = gender.MALE
  )

  if ( !is.na(user) || !is.null(user) ) {
    log.success('example', paste('Successfully registered User:', username))
  } else {
    log.danger('example', paste('Error in registering User:', username))
  }
} else {
  user      <- user.get(username, password)
}

portfolio   <- portfolio.get(user, name = portname)
if ( is.na(portfolio) ) {
  log.info('example', join(c('Portfolio"', portname, '" does not exist.')))

  portfolio <- portfolio.register(user, name = portname)
}

# holding.add(portfolio, type = holding.FOREX, params = list(
#   from  = forex.EUR,
#   to    = forex.USD,
#   units = 1000
# ))

# A holding of EURUSD with 10,000 units.
holding     <- holding.get(portfolio, type = holding.FOREX, params = list(
  from = forex.EUR,
  to   = forex.USD
))

# pairs       <- paste(holding$from, holding$to, sep = '')
# cache.FOREX(pairs)

# holding (symbol, units), seed, strategy
back.test(holding, 10000, function (data) {
  # your strategy here.

  # `data` is of the following format:
  # | ID | datetime | symbol | open | high | low | close | volumne |
  # datetime and OHLCV are also called "lines", in this case - 6 lines.
  # In case of FX, open is synonymous to 'bid' while close is synonymous to 'ask'.
  
  # Accessing values are done as follows:
  # Accessing current market line.
  # tail(data, n = 1)[0,] <-  current market line (or the last trade line).

  # Accessing current, previous market line.
  # tail(data, n = 2)[0,] <- previous market line.
  # tail(data, n = 2)[1,] <-  current market line.

  # # Strategy Logging
  current <- tail(data, n = 1)
  print(paste('[', current$datetime, ']: Ask - ', current$close, sep = ''))
  # # end Logging

  # # Random Strategy
  # random  <- sample(1:100, 1)

  # if ( is.equal(random %% 2, 0) ) {
  #   # type - order.MARKET - does a trade based on the next market price (backtester only).
  #   order <- list(symbol = 'EURUSD', units = 1000, type = order.MARKET, trade = trade.BUY)
  # } else {
  #   order <- list(symbol = 'EURUSD', units = 1000, type = order.MARKET, trade = trade.SELL)
  # }

  # return(order)
  # # end Random Strategy

  # # Double Top Double Bottom Strategy
  data <- tail(data, n = 3)


  # if current close is less than previous close
  if ( data[3,]$close < data[2,]$close ) 
    # if previous close is less than its previous close
    if ( data[2,]$close < data[1,]$close )
    {
      order <- list(symbol = 'EURUSD', units = 10, type = order.MARKET, trade = trade.BUY)

      return(order)
    }
    else
    {
      order <- list(symbol = 'EURUSD', units = 10, type = order.MARKET, trade = trade.SELL)

      return(order)
    }
  # # end Double Top Double Bottom Strategy
})


