library(adventofcode21)
x <- readLines("./inst/input01.txt")

p1 <- f01_count_increases(x)
p2 <- f01_count_shingled_increases(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)
