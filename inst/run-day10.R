library(adventofcode21)
x <- readLines("./inst/input10.txt")

p1 <- f10a_score_syntax_errors(x)
p2 <- f10b_score_incomplete_lines(x)

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)
