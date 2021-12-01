source("day1.R")

lb <- bench::mark(
  count_increasing_depth_1(values),
  count_increasing_depth_2(values),
  count_increasing_depth_3(values)
)

lb2 <- bench::mark(
  count_triples_1(values),
  count_triples_2(values),
  count_triples_3(values)
)
