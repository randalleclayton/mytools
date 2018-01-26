
# Input -- data frame returned from coppock() command
buy_signal <- function(df) {

  n <- NROW(df)
  if (n == 0) { return(NULL) }

  buy_setup <- buy_trigger <- FALSE
  buys <- c()

  for (i in 2:n) {
    # Avoid oscillations by requiring a new positive to
    # negative move to setup the next buy trigger.
    if (buy_trigger == TRUE) { buy_setup = FALSE }

    if (buy_setup && (df[[i,3]] > df[[i - 1,3]])) {
      buys <- append(buys,i)
      buy_trigger = TRUE
    } else {
      if (df[[i,3]] < 0 && df[[i - 1,3]] >= 0) {
        buy_setup = TRUE
        buy_trigger = FALSE
      }
    }
  }
  return(buys)
}
