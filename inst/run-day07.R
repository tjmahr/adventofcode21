library(adventofcode21)
x <- readLines("./inst/input07.txt")

p1 <- f07a_optimize_fuel_linear(x)
p2 <- f07b_optimize_fuel_triangular(x)

stopifnot(p1 == aoc_solutions$day07a)
stopifnot(p2 == aoc_solutions$day07b)
