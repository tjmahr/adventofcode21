library(adventofcode21)
x <- readLines("./inst/input13.txt")

p1a <- f13a_fold_once(x)
p1 <- p1a |> unique() |> nrow()
p2 <- f13b_fold_all(x)

if (interactive()) {
  plot(p2$x, -p2$y)
}

stopifnot(p1 == aoc_solutions$day13a)
