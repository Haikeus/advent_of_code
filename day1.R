library(dplyr)
values <- readr::read_csv("input1.txt",
                          col_names = "values") %>%
  pull(values)


count_increasing_depth_1 <- function(x) {
  y <- x[2:length(x)]
  x <- x[1:(length(x) - 1)]
  return(purrr::reduce(y > x, sum))
}

count_increasing_depth_2 <- function(x) {
  s <- 0
  for (i in 2:length(x)) {
    if (x[i] > x[i - 1]) {
      s <- s + 1
    }
  }
  return(s)
}

compare <- function(x) {
  return(x[2] > x[1])
}

count_increasing_depth_3 <- function(x) {
  sum(zoo::rollapply(x, 2, compare))
}


count_triples_1 <- function(x) {
  m <- c()
  for (i in 1:(length(x) - 2)) {
    m <- c(m, sum(x[i:(i + 2)]))
  }
  
  s <- 0
  for (i in 2:length(m)) {
    if (m[i] > m[i - 1]) {
      s <- s + 1
    }
  }
  return(s)
}


count_triples_2 <- function(x) {
  triples <- zoo::rollapply(x, 3, sum)
  return(sum(zoo::rollapply(triples, 2, compare)))
}

compare_triples <- function(x) {
  return(sum(x[1:3]) < sum(x[2:4]))
}

count_triples_3 <- function(x) {
  sum(zoo::rollapply(
    x,
    4,
    compare_triples,
    partial = FALSE
  ))
}

# n <- 3
# 
# compare_triples <- function(x, n) {
#   return(sum(x[1:n]) < sum(x[2:(n + 1)]))
# }
# 
# sum(zoo::rollapply(
#   values,
#   (n + 1),
#   compare_triples,
#   n = n,
#   partial = FALSE
# ))
