library(adventofcode21)
x <- readLines("./inst/input11.txt")

p1 <- f11a_run_n_steps(x, 100)
sum(p1)
p2 <- f11b_run_until_all_flash(x)

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
