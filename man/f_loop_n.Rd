% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{f_loop_n}
\alias{f_loop_n}
\title{Repeatedly apply a function}
\source{
https://www.tjmahr.com/repeatedly-calling-a-function/
}
\usage{
f_loop_n(.x, .reps = 1, .f, ...)
}
\arguments{
\item{.x}{input data}

\item{.reps}{number of times to call the function}

\item{.f}{function to call}

\item{...}{arguments passed onto \code{.f()}}
}
\value{
result of calling \code{.f(.x, ...)} \code{.rep} times
}
\description{
Repeatedly apply a function
}
