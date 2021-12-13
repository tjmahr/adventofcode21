#' Day 13: Transparent Origami
#'
#' [Transparent Origami](https://adventofcode.com/2021/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' You reach another volcanically active part of the cave. It would be nice
#' if you could do some kind of thermal imaging so you could tell ahead of
#' time which caves are too hot to safely enter.
#'
#' Fortunately, the submarine seems to be equipped with a thermal camera!
#' When you activate it, you are greeted with:
#'
#'     Congratulations on your purchase! To activate this infrared thermal imaging
#'     camera system, please enter the code found on page 1 of the manual.
#'
#' Apparently, the Elves have never used this feature. To your surprise,
#' you manage to find the manual; as you go to open it, page 1 falls out.
#' It\'s a large sheet of [transparent
#' paper](https://en.wikipedia.org/wiki/Transparency_(projection))! The
#' transparent paper is marked with random dots and includes instructions
#' on how to fold it up (your puzzle input). For example:
#'
#'     6,10
#'     0,14
#'     9,10
#'     0,3
#'     10,4
#'     4,11
#'     6,0
#'     6,12
#'     4,1
#'     0,13
#'     10,12
#'     3,4
#'     3,0
#'     8,4
#'     1,10
#'     2,14
#'     8,10
#'     9,0
#'
#'     fold along y=7
#'     fold along x=5
#'
#' The first section is a list of dots on the transparent paper. `0,0`
#' represents the top-left coordinate. The first value, `x`, increases to
#' the right. The second value, `y`, increases downward. So, the coordinate
#' `3,0` is to the right of `0,0`, and the coordinate `0,7` is below `0,0`.
#' The coordinates in this example form the following pattern, where `#` is
#' a dot on the paper and `.` is an empty, unmarked position:
#'
#'     ...#..#..#.
#'     ....#......
#'     ...........
#'     #..........
#'     ...#....#.#
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     .#....#.##.
#'     ....#......
#'     ......#...#
#'     #..........
#'     #.#........
#'
#' Then, there is a list of *fold instructions*. Each instruction indicates
#' a line on the transparent paper and wants you to fold the paper *up*
#' (for horizontal `y=...` lines) or *left* (for vertical `x=...` lines).
#' In this example, the first fold instruction is `fold along y=7`, which
#' designates the line formed by all of the positions where `y` is `7`
#' (marked here with `-`):
#'
#'     ...#..#..#.
#'     ....#......
#'     ...........
#'     #..........
#'     ...#....#.#
#'     ...........
#'     ...........
#'     -----------
#'     ...........
#'     ...........
#'     .#....#.##.
#'     ....#......
#'     ......#...#
#'     #..........
#'     #.#........
#'
#' Because this is a horizontal line, fold the bottom half *up*. Some of
#' the dots might end up overlapping after the fold is complete, but dots
#' will never appear exactly on a fold line. The result of doing this fold
#' looks like this:
#'
#'     #.##..#..#.
#'     #...#......
#'     ......#...#
#'     #...#......
#'     .#.#..#.###
#'     ...........
#'     ...........
#'
#' Now, only `17` dots are visible.
#'
#' Notice, for example, the two dots in the bottom left corner before the
#' transparent paper is folded; after the fold is complete, those dots
#' appear in the top left corner (at `0,0` and `0,1`). Because the paper is
#' transparent, the dot just below them in the result (at `0,3`) remains
#' visible, as it can be seen through the transparent paper.
#'
#' Also notice that some dots can end up *overlapping*; in this case, the
#' dots merge together and become a single dot.
#'
#' The second fold instruction is `fold along x=5`, which indicates this
#' line:
#'
#'     #.##.|#..#.
#'     #...#|.....
#'     .....|#...#
#'     #...#|.....
#'     .#.#.|#.###
#'     .....|.....
#'     .....|.....
#'
#' Because this is a vertical line, fold *left*:
#'
#'     #####
#'     #...#
#'     #...#
#'     #...#
#'     #####
#'     .....
#'     .....
#'
#' The instructions made a square!
#'
#' The transparent paper is pretty big, so for now, focus on just
#' completing the first fold. After the first fold in the example above,
#' `17` dots are visible - dots that end up overlapping after the fold is
#' completed count as a single dot.
#'
#' *How many dots are visible after completing just the first fold
#' instruction on your transparent paper?*
#'
#' **Part Two**
#'
#' [Finish
#' folding]{title="How can you fold it that many times? You tell me, I'm not the one folding it."}
#' the transparent paper according to the instructions. The manual says the
#' code is always *eight capital letters*.
#'
#' *What code do you use to activate the infrared thermal imaging camera
#' system?*
#' @param x some data
#' @return For Part One, `f13a(x)` returns the raw result of a single fold. For
#'   Part Two, `f13b_fold_all(x)` returns the raw result after performing all of
#'   the fold steps.
#' @export
#' @examples
#' r <- f13a_fold_once(example_data_13())
#' r
#' r |> unique() |> nrow()
#' f13b_fold_all(example_data_13())
f13a_fold_once <- function(x) {
  # strategy: math
  x <- f13_read_input(x)
  result <- f13_fold(x$points, x$folds[[1]])
  result
}


#' @rdname day13
#' @export
f13b_fold_all <- function(x) {
  # strategy: math, plotting
  x <- f13_read_input(x)
  result <- x$points
  for (fold in x$folds) {
    result <- f13_fold(result, fold)
  }
  result
}

# Do a single fold
f13_fold <- function(points, fold) {
  flip <- function(xs, p) {
    ifelse(xs < p, xs, p - xs + p)
  }
  points[fold$axis] <- flip(points[[fold$axis]], fold$location)

  f13_clean_rows(points)
}


f13_read_input <- function(x) {
  get_index <- function(x, ...) x[...]

  parse_fold_line <- function(x) {
    pieces <- strsplit(x, "=")[[1]]
    list(
      axis = substr(pieces[1], 12, 12),
      # convert to 1-based indexing
      location = as.numeric(pieces[2]) + 1
    )
  }

  section_break <- which(x == "")

  points <- x |>
    get_index(seq(1, section_break - 1)) |>
    strsplit(",") |>
    lapply(as.numeric) |>
    # convert to 1-based indexing
    lapply(function(x) x + 1) |>
    lapply(function(x) data.frame(x = x[1], y = x[2])) |>
    f_reduce(rbind) |>
    f13_clean_rows()

  folds <- x |>
    get_index(seq(section_break + 1, length(x))) |>
    lapply(parse_fold_line)

  list(
    points = points,
    folds = folds
  )
}


# Just to help me visual compare point indices to examples:
# Remove rownames and sort columns
f13_clean_rows <- function(df) {
  r <- order(df[[1]], df[[2]])
  df <- df[r, , drop = FALSE]
  row.names(df) <- NULL
  df
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(
      "6,10",
      "0,14",
      "9,10",
      "0,3",
      "10,4",
      "4,11",
      "6,0",
      "6,12",
      "4,1",
      "0,13",
      "10,12",
      "3,4",
      "3,0",
      "8,4",
      "1,10",
      "2,14",
      "8,10",
      "9,0",
      "",
      "fold along y=7",
      "fold along x=5"
    )
  )
  l[[example]]
}
