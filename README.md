# adventofcode21

<!-- badges: start -->
<!-- badges: end -->

These are my solutions to [Advent of Code
2021](http://adventofcode.com/2021), a series of 25 programming puzzles.

## Installation

You can install adventofcode21 from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tjmahr/adventofcode21")
```

But you probably shouldn't do that.

## Package overview

The package infrastructure was automatically generated for each day using 
[my aoc package](https://github.com/tjmahr/aoc) and its `use_day(day, year)` 
function.

The `/R` folder contains the R functions I wrote for each day. I used
some light test-driven development for the puzzles. That is, each puzzle
description provides some example inputs and outputs. Before tackling
the main test input, I write a unit-test in `/tests` that confirms that
my solution can correctly reproduce the examples. The `/inst` directory
contains the code to handle the main test input for each day.

I limited the amount of package dependencies used for these puzzles to
maximize future compatibility and to make sure that it is mostly *my
code* that solves the problems. For example, if a puzzle requires
answering questions about the data in a tree-like structure, it would be
kind of cheating for me to find a library for building and traversing
trees to tackle the problem. It’s *advent of code*, not *advent of
load*.

I have allowed myself to use:

-   base, stats, utils, tools, and so on: the base R packages

In the past I have allowed myself to use magrittr, rlang and stringr, but 
the native pipe `|>` eliminates the need for magrittr and some grit 
eliminates the need for the others.

I’ve put my R source code under a GPL-3 license. It should not apply to
the puzzle descriptions in the code comments at the top of each file. I
did not write those descriptions.

## Coding approaches

Here are the programming tasks and techniques I used for the various
puzzles.

-   00a/b *Write puzzle description like this:* Period separated strategies. Enumerated.

-   23a/b *Solve a tile sliding game:* No code. Solved by hand (see inst/notes-day-22b-moves.R).

-   25a/b *Simulate movement in a grid that wraps around:* Matrix subsetting. Mod1.


By “book-keeping”, I mean basic programming where I keep track of some
changing state like a position in a vector.

By “math”, I mean studying the problem and using math to find a shortcut
that lets me skip some computations.

## Helpful builtin R functions

Here are some functions that have I discovered, rediscovered, or
otherwise appreciate somewhat more from these exercises:

### 2021 list

-   [`which()`](https://rdrr.io/r/base/which.html) but with `arr.ind = TRUE` for easier matrix subsetting
-   [`simplify2Array()`](https://rdrr.io/r/base/lapply.html) for combining a list of vectors into a matrix



### 2020 list

-   [`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)
-   [`intersect()`](https://rdrr.io/r/base/sets.html)
-   [`lengths()`](https://rdrr.io/r/base/lengths.html)
-   [`modifyList()`](https://rdrr.io/r/utils/modifyList.html)
-   [`outer()`](https://rdrr.io/r/base/outer.html)
-   [`prod()`](https://rdrr.io/r/base/prod.html)
-   [`strtoi()`](https://rdrr.io/r/base/strtoi.html)
-   [`chartr()`](https://rdrr.io/r/base/chartr.html)
-   [`Find() and Position()`](https://rdrr.io/r/base/funprog.html)
