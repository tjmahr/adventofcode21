library(adventofcode21)
x <- readLines("./inst/input03.txt")

p1 <- f03a_calculate_power(x)
p2 <- f03b_calculate_life_support(x)

stopifnot(p1 == aoc_solutions$day03a)
stopifnot(p2 == aoc_solutions$day03b)
