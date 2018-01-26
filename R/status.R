
triggers <- function(df) {

  n <- NROW(df)
  if (n == 0) { return(NULL) }

  buy_setup <- buy_trigger <- FALSE

  for (i in 2:n) {
    # Avoid oscillations by requiring a new positive to
    # negative move to setup the next buy trigger.
    if (buy_trigger == TRUE) { buy_setup = FALSE }

    if (buy_setup && (df[[i,3]] > df[[i - 1,3]])) {
      buy_trigger = TRUE
    } else {
      if (df[[i,3]] < 0 && df[[i - 1,3]] >= 0) {
        buy_setup = TRUE
        buy_trigger = FALSE
      }
    }
  }
  return(list(buy_setup, buy_trigger))
}

status <- function(symbols) {

  for (symbol in symbols) {

    q <- buy(symbol)
    if (is.null(q)) { next}

    buys <- q[[1]]
    quotes <- q[[2]]
    trigs <- triggers(quotes)
    buy_setup <- trigs[[1]]
    buy_trigger <- trigs[[2]]

    n <- NROW(buys)
    last <- buys[[n,3]]
    p <- project(buys)
    p <- p * last

    x <- buys
    x <- x[complete.cases(x),]
    if (NROW(x) > 0) {
      buys$IR <- ""
      for (i in 1:NROW(x)) {
        buys[i,"IR"] <- round(infoRatio(symbol, '^GSPC', buys[i,2], buys[i,2] + 730)[[1]][[2]],2)
      }
    }

    print(buys)
    cat("\n\n")
    cat("Projection: ")
    cat(round(p,2))
    cat('\n\n')
    print(tail(quotes,6))

    cat('\n')
    cat(paste("Buy setup:", buy_setup, '\n'))
    cat(paste("Buy trigger:", buy_trigger, '\n'))
  }
}
