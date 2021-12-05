library(adventofcode21)
x <- readLines("./inst/input04.txt")

p1 <- f04a_play_bingo(x)
p2 <- f04b_play_until_last_bingo(x)

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
