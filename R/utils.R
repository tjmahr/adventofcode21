
#' `x %% m` but with a value between (0, m]
#'
#' @param x number to divide
#' @param m number to divide with
#' @return the remainder of `x %% m` but a remainder of 0 is
#'   replaced with `m`
#' @details Inspired by Julia's `mod1()`
#' https://docs.julialang.org/en/v1/base/math/#Base.mod1
mod1 <- function(x, m) {
  r <- x %% m
  ifelse(r == 0, m, r)
}

#' Pipe friendly version of `Reduce()`
#' @param x,f,... arguments for `Reduce()`
#' @return `Reduce(f, x, ...)`
f_reduce <- function(x, f, ...) {
  Reduce(f, x, ...)
}

#' Pipe friendly version of `Map()`
#' @param x,y,f,... arguments for `Map()`
#' @return `Map(f, x, y, ...)`
f_map2 <- function(x, y, f, ...) {
  Map(f, x, y, ...)
}

#' Pipe friendly version of `Filter()`
#' @param x,f arguments for `Filter()`
#' @return `Filter(f, x)`
f_filter <- function(x, f) {
  Filter(f, x)
}

# chr_gsub <- function(x, pattern, replacement, perl = TRUE, ...) {
#   gsub(pattern = pattern, x = x, replacement = replacement, perl, ...)
# }
# chr_sub <- function(x, pattern, replacement, perl = TRUE, ...) {
#   sub(pattern = pattern, x = x, replacement = replacement, perl, ...)
# }


#' Repeatedly apply a function
#' @param .x input data
#' @param .reps number of times to call the function
#' @param .f function to call
#' @param ... arguments passed onto `.f()`
#' @return result of calling `.f(.x, ...)` `.rep` times
#' @source https://www.tjmahr.com/repeatedly-calling-a-function/
f_loop_n <- function(.x, .reps = 1, .f, ...) {
  # A single, finite, non-negative number of repetitions
  stopifnot(
    length(.reps) == 1,
    !is.na(.reps),
    .reps >= 0,
    is.finite(.reps)
  )

  # 0 .reps
  value <- .x

  while (.reps >= 1) {
    value <- .f(value, ...)
    .reps <- .reps - 1
  }

  value
}
