library(adventofcode21)
x <- readLines("./inst/input02.txt")

p1 <- f02a_find_submarine_product(x)
p2 <- f02b_find_aimed_submarine_product(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
