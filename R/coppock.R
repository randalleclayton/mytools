#
# Coppock Index for a price time series. Mostly used with stocks and ETFs
# using end of month price data. A buy signal is generated when the index
# rises from a negative value. Generates buy signals only, there is no sell
# signal. Works very well with indexes (i.e., S&P 500) and some large cap
# stocks. Less reliable on single stocks, better on indexes or ETFs with
# larger number of stocks.
#
# The weighted moving average of the 11 and 14 month Rate Of Change.
#

coppock <- function(symbol, start_date = '2000-01-01',
                    end_date = Sys.Date()) {

  # For Yahoo quotes ... BRK.B => BRK-B
  # Quandl uses _
  # symbol <- gsub("\\.", "-", symbol)
  # Get prices, adjust to monthly close, compute ROC11 and ROC14
  q <- tq_get(symbol, from = start_date) %>%
    tq_transmute_xy(x = adjusted, mutate_fun = to.period,
                    period = "months") %>%
    tq_mutate(select = adjusted, mutate_fun = ROC, n = 11) %>%
    tq_mutate(select = adjusted, mutate_fun = ROC, n = 14)

  # Coppock Index == WMS(ROC11+ROC14)
  s <- tibble::add_column(q, "sum" = (q[,'ROC'] + q[,'ROC.1'])[,1],
                          .before = 3)
  cop <- round(TTR::WMA(s[,"sum"], n = 10),2)
  s <- tibble::add_column(s, "coppock" = cop, .before = 3)
  s <- dplyr::select(s, date, adjusted, coppock)

  # Need 24 months of closing price data to calculate coppock
  s <- s[complete.cases(s),]
  return(s)
}
