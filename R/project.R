
actual <- function(buys) {
  last <- NROW(buys)

  l <- c()
  for (col in 3:8) {
    l <- append(l, buys[last,col])
  }
  return(round(l,2))
}

project <- function(buys) {

  data <- buys[complete.cases(buys),]
  data <- select(data, -Symbol, -Date)
  data <- data/data[,1]

  l <- c()
  for (col in 1:NCOL(data)) {
    l <- append(l, sum(data[,col]) / NROW(data))
  }

  return(round(l,2))
}
