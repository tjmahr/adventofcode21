library(adventofcode21)
x <- readLines("./inst/input21.txt")

p1 <- f21a_play_deterministic_game(x)
p2a <- f21b_play_quantum_game(x)
p2 <- max(p2a$ways)
print(max(p2a$ways), digits = 20)

stopifnot(p1 == aoc_solutions$day21a)
stopifnot(p2 == aoc_solutions$day21b)
