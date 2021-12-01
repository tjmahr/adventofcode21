
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
