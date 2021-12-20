library(adventofcode21)
x <- readLines("./inst/input20.txt")

p1 <- f20_enhance_n_times(x, 2)
p2 <- f20_enhance_n_times(x, 50)
# p2 <- f20b(x)

stopifnot(p1 == aoc_solutions$day20a)
stopifnot(p2 == aoc_solutions$day20b)
