library(adventofcode21)
x <- readLines("./inst/input18.txt")

p1 <- f18a_snailfish_homework(x)
p2a <- f18b_snailfish_homework2(x)
p2 <- max(p2a)

stopifnot(p1 == aoc_solutions$day18a)
stopifnot(p2 == aoc_solutions$day18b)
