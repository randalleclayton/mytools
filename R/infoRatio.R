
getIRs <- function(dates, symbol, bench) {
  for (date in dates) {
    a <- infoRatio(symbol, bench, date, date + 365)
    print(a[[2]])
  }
}


infoRatio <- function(symbol, benchmark, from, to) {

  Ra <- tq_get(symbol,
               get  = "stock.prices",
               from = from,
               to   = to) %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "monthly",
                 col_rename = "Ra")

  Rb <- tq_get(benchmark, get  = "stock.prices",
               from = from,
               to   = to) %>%
    tq_transmute(select     = adjusted,
                 mutate_fun = periodReturn,
                 period     = "monthly",
                 col_rename = "Rb")

  RaRb <- left_join(Ra,Rb, by = "date")

  perf <- RaRb %>%
    tq_performance(Ra = Ra,
                   Rb = Rb,
                   performance_fun = table.InformationRatio)

  return(list(perf, RaRb))
}
