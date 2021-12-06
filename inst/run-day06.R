library(adventofcode21)
x <- readLines("./inst/input06.txt")

p1 <- f06a_step_n_days(x) |> sum()
p2 <- f06a_step_n_days(x, 256) |> sum()

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
