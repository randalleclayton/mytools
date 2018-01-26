
lhist <- function(symbol, i, quotes) {

  hdr <- list("Symbol", "Date", "Buy")
  d <- as.vector(c(3,6,12,18,24))

  for (x in d) {
    hdr <- append(hdr,paste("+", x, "M", sep = ""))
  }

  # Report quotes for months following the buy signal
  n <- as.vector(c(0,3,6,12,18,24))

  # How many quotes in future do we have?
  nrows <- NROW(quotes)

  l <- list(symbol, quotes[[i,"date"]])

  for (x in n) {
    if ((i + x) <= nrows) {
      l <- append(l,round(quotes[[i + x,2]],2))
    } else {
      l <- append(l, NA)
    }
  }

  df <- data.frame(as.vector(l))
  colnames(df) <- hdr
  return(df)
}


buy <- function(symbol, start_date = "1950-01-01") {

  df <- coppock(symbol, start_date = start_date)
  buys <- data.frame()
  signals <- buy_signal(df)

  for (i in signals) {
    bhist  <- lhist(symbol, i, df)
    buys <- rbind(buys, bhist)
  }

  # if (NROW(buys) > 0) {
  #  buys$IR <- ""
  #  for (i in 1:NROW(buys)) {
  #    buys[i,"IR"] <- round(infoRatio(symbol,
  #                                    '^GSPC',
  #                                    buys[i,2],
  #                                    buys[i,2] + 730)[[1]][[2]],2)
  #   }
  # }

  return(list(buys, df))
}
