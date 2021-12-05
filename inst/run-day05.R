library(adventofcode21)
x <- readLines("./inst/input05.txt")

p1 <- f05a_count_intersections(x)
p2 <- f05a_count_intersections(x, include_diagonals = TRUE)

stopifnot(p1 == aoc_solutions$day05a)
stopifnot(p2 == aoc_solutions$day05b)
