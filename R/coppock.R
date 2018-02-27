#
# Coppock Index is the weighted moving average of the sum of the 11
# and 14 month rate of change of a monthly price series.
#
# Coppock Index provides buy signals only. A buy signal is generated
# when the index rises from a negative value.
#
# Works very well with indexes (i.e., S&P 500) and some large cap equities.
#
# Not useful on small cap stocks.
#

coppock <- function(symbol, start_date = '2000-01-01',
                    end_date = Sys.Date()) {

  # For Yahoo quotes ... BRK.B => BRK-B
  symbol <- gsub("\\.", "-", symbol)

  q <- tq_get(symbol, from = start_date) %>%
    tq_transmute_xy(x = adjusted, mutate_fun = to.period,
                    period = "months") %>%
    tq_mutate(select = adjusted, mutate_fun = ROC, n = 11) %>%
    tq_mutate(select = adjusted, mutate_fun = ROC, n = 14)

  # Coppock Index == WMA(ROC11+ROC14)
  s <- tibble::add_column(q, "sum" = (q[,'ROC'] + q[,'ROC.1'])[,1],
                          .before = 3)
  cop <- round(TTR::WMA(s[,"sum"], n = 10),2)
  s <- tibble::add_column(s, "coppock" = cop, .before = 3)
  s <- dplyr::select(s, date, adjusted, coppock)

  # Need 24 months of closing price data to calculate coppock
  s <- s[complete.cases(s),]
  return(s)
}
