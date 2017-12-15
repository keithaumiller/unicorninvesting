library(lubridate)

trade.BUY   <- 'BUY'
trade.SELL  <- 'SELL'

trade.trade <- function (symbol, units, type, rate, at = now()) {
	price     <- units * rate
  order     <- list(symbol = symbol, units = units, time = at, price = price, rate = rate)

  return(order)
}