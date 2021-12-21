library(adventofcode21)
x <- readLines("./inst/input17.txt")

# p1 <- f17a_maximize_height(x)
p0 <- f17b_count_velocities(x)
p1 <- max(p1$highest)
p2 <- nrow(p0)

stopifnot(p1 == aoc_solutions$day17a)
stopifnot(p2 == aoc_solutions$day17b)
