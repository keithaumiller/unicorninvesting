library(stringr)

source('util/utils.R')

source('entity/history.R')
source('entity/trade.R')

color.sign     <- function (value)
{
    color      <- if ( value > 0 ) color.SUCCESS else if ( value < 0 ) color.DANGER else color.WARNING

    return(color)
}

back.test      <- function (holding, seed, strategy,
    from       = lubridate::origin,
    to         = now(),
    default    = 10,   # default units - to be implemented
    commission = 0.01, # to be implemented
    sleep      = 1) 
{
    if ( is.equal(holding$type, holding.FOREX) ) {
        symbol  <- paste(holding$from, holding$to, sep = '')
    } else if ( is.equal(holding$type, holding.STOCK) ) {
        symbol  <- holding$symbol
    }

    symbol      <- unique(symbol)

    # TODO - Work on multiple symbols/data-feeds.
    data        <- history.get(symbol = symbol, from = from, to = to)
    length      <- nrow(data)

    log.warn('back.test', paste('Seed:', seed))
    asset       <- seed
    prev        <- asset

    for (slice in 1:(length - 3)) {
        frame      <- data[ 1:(slice + 3), ] # Day 1, 2, 3, 4 <- current market price.
        feed       <- data[ 1:(slice + 2), ] # Day 1, 2, 3
        tail       <- tail( feed, n = 1)
        current    <- tail(frame, n = 1)
        evaluation <- strategy(feed)

        if ( !is.null(evaluation) ) {
            order  <- evaluation
            time   <- current$datetime

            # done under the assumption that the order type is a market order.
            if ( is.equal(order$trade, trade.BUY) ) {
                rate  <- tail$close
                log.info('back.test', paste('Order BUY  created  at price:', rate))
                rate  <- current$close # synonymous to ask
                order <- trade.trade(symbol = order$symbol, units = order$units, type = order$type, at = time, rate = rate)
                log.info('back.test', paste('Order BUY  executed at price:', rate))

                asset <- asset - order$price
            } else 
            if ( is.equal(order$trade, trade.SELL) ) {
                rate  <- tail$open
                log.info('back.test', paste('Order SELL created  at price:', rate))
                rate  <- current$open  # synonymous to bid
                order <- trade.trade(symbol = order$symbol, units = order$units, type = order$type, at = time, rate = rate)
                log.info('back.test', paste('Order SELL executed at price:', rate))

                asset <- asset + order$price
            }
        }

        gross     <- ((asset - seed) / seed) * 100 # Overall  PnL
        color     <- color.sign(gross)
        statement <- paste(color, 'Equity: ', asset,  ' (GROSS = ', round(gross, digits = 2), '%)', color.RESET, sep = '')

        gross     <- ((asset - prev) / prev) * 100 # Previous PnL
        color     <- color.sign(gross)
        statement <- paste(statement, color, ' (PC = ', round(gross, digits = 2), '%)', color.RESET, sep = '')
        prev      <- asset

        log.info('back.test', statement)

        log.info('back.test', paste('Sleeping for', sleep, 'seconds.'))
        Sys.sleep(sleep)
    }
}