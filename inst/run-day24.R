library(adventofcode21)

p1 <- f24a_run_with_maximum_number()
p1 <- p1$raw_data |> paste0(collapse = "") |> as.numeric()
p2 <- f24b_run_with_minimum_number()
p2 <- p2$raw_data |> paste0(collapse = "") |> as.numeric()

stopifnot(p1 == aoc_solutions$day24a)
stopifnot(p2 == aoc_solutions$day24b)
