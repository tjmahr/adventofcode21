library(adventofcode21)
x <- readLines("./inst/input25.txt")

p1 <- f25a_find_stable_grid(x)

stopifnot(p1$n == aoc_solutions$day25a)
