library(adventofcode21)
x <- readLines("./inst/input09.txt")

p1 <- f09a_find_lava_risk_level(x)
p2 <- f09b_find_basins(x)

stopifnot(p1 == aoc_solutions$day09a)
stopifnot(p2 == aoc_solutions$day09b)
