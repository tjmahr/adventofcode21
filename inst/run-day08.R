library(adventofcode21)
x <- readLines("./inst/input08.txt")

p1 <- f08a_count_1478s(x)
p2 <- f08b_solve_and_sum_output(x)

stopifnot(p1 == aoc_solutions$day08a)
stopifnot(p2 == aoc_solutions$day08b)
