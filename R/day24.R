#' Day 24: Arithmetic Logic Unit
#'
#' [Arithmetic Logic Unit](https://adventofcode.com/2021/day/24)
#'
#' @name day24
#' @rdname day24
#' @details
#'
#' **Part One**
#'
#' [Magic smoke](https://en.wikipedia.org/wiki/Magic_smoke) starts leaking
#' from the submarine\'s [arithmetic logic
#' unit](https://en.wikipedia.org/wiki/Arithmetic_logic_unit) (ALU).
#' Without the ability to perform basic arithmetic and logic functions, the
#' submarine can\'t produce cool patterns with its Christmas lights!
#'
#' It also can\'t navigate. Or run the oxygen system.
#'
#' Don\'t worry, though - you *probably* have enough oxygen left to give
#' you enough time to build a new ALU.
#'
#' The ALU is a four-dimensional processing unit: it has integer variables
#' `w`, `x`, `y`, and `z`. These variables all start with the value `0`.
#' The ALU also supports *six instructions*:
#'
#' -   `inp a` - Read an input value and write it to variable `a`.
#' -   `add a b` - Add the value of `a` to the value of `b`, then store the
#'     result in variable `a`.
#' -   `mul a b` - Multiply the value of `a` by the value of `b`, then
#'     store the result in variable `a`.
#' -   `div a b` - Divide the value of `a` by the value of `b`, truncate
#'     the result to an integer, then store the result in variable `a`.
#'     (Here, \"truncate\" means to round the value toward zero.)
#' -   `mod a b` - Divide the value of `a` by the value of `b`, then store
#'     the *remainder* in variable `a`. (This is also called the
#'     [modulo](https://en.wikipedia.org/wiki/Modulo_operation) operation.)
#' -   `eql a b` - If the value of `a` and `b` are equal, then store the
#'     value `1` in variable `a`. Otherwise, store the value `0` in
#'     variable `a`.
#'
#' In all of these instructions, `a` and `b` are placeholders; `a` will
#' always be the variable where the result of the operation is stored (one
#' of `w`, `x`, `y`, or `z`), while `b` can be either a variable or a
#' number. Numbers can be positive or negative, but will always be
#' integers.
#'
#' The ALU has no *jump* instructions; in an ALU program, every instruction
#' is run exactly once in order from top to bottom. The program halts after
#' the last instruction has finished executing.
#'
#' (Program authors should be especially cautious; attempting to execute
#' `div` with `b=0` or attempting to execute `mod` with `a<0` or `b<=0`
#' will cause the program to crash and might even [damage the
#' ALU]{title="Maybe this is what happened to the last one."}. These
#' operations are never intended in any serious ALU program.)
#'
#' For example, here is an ALU program which takes an input number, negates
#' it, and stores it in `x`:
#'
#'     inp x
#'     mul x -1
#'
#' Here is an ALU program which takes two input numbers, then sets `z` to
#' `1` if the second input number is three times larger than the first
#' input number, or sets `z` to `0` otherwise:
#'
#'     inp z
#'     inp x
#'     mul z 3
#'     eql z x
#'
#' Here is an ALU program which takes a non-negative integer as input,
#' converts it into binary, and stores the lowest (1\'s) bit in `z`, the
#' second-lowest (2\'s) bit in `y`, the third-lowest (4\'s) bit in `x`, and
#' the fourth-lowest (8\'s) bit in `w`:
#'
#'     inp w
#'     add z w
#'     mod z 2
#'     div w 2
#'     add y w
#'     mod y 2
#'     div w 2
#'     add x w
#'     mod x 2
#'     div w 2
#'     mod w 2
#'
#' Once you have built a replacement ALU, you can install it in the
#' submarine, which will immediately resume what it was doing when the ALU
#' failed: validating the submarine\'s *model number*. To do this, the ALU
#' will run the MOdel Number Automatic Detector program (MONAD, your puzzle
#' input).
#'
#' Submarine model numbers are always *fourteen-digit numbers* consisting
#' only of digits `1` through `9`. The digit `0` *cannot* appear in a model
#' number.
#'
#' When MONAD checks a hypothetical fourteen-digit model number, it uses
#' fourteen separate `inp` instructions, each expecting a *single digit* of
#' the model number in order of most to least significant. (So, to check
#' the model number `13579246899999`, you would give `1` to the first `inp`
#' instruction, `3` to the second `inp` instruction, `5` to the third `inp`
#' instruction, and so on.) This means that when operating MONAD, each
#' input instruction should only ever be given an integer value of at least
#' `1` and at most `9`.
#'
#' Then, after MONAD has finished running all of its instructions, it will
#' indicate that the model number was *valid* by leaving a `0` in variable
#' `z`. However, if the model number was *invalid*, it will leave some
#' other non-zero value in `z`.
#'
#' MONAD imposes additional, mysterious restrictions on model numbers, and
#' legend says the last copy of the MONAD documentation was eaten by a
#' [tanuki](https://en.wikipedia.org/wiki/Japanese_raccoon_dog). You\'ll
#' need to *figure out what MONAD does* some other way.
#'
#' To enable as many submarine features as possible, find the largest valid
#' fourteen-digit model number that contains no `0` digits. *What is the
#' largest model number accepted by MONAD?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f24a(x)` returns .... For Part Two,
#'   `f24b(x)` returns ....
#' @export
#' @examples
#' f24a(example_data_24())
#' f24b()
f24a <- function(x) {

}


#' @rdname day24
#' @export
f24b <- function(x) {

}


f24_helper <- function(x) {
  # create an arithmetic logic unit
  alu <- function(data) {
    vars <- list(
      w = 0,
      x = 0,
      y = 0,
      z = 0
    )
    data <- data
    next_data <- function() {
      n <- data[1]
      data <<- data[-1]
      n
    }
    show_state <- function() {
      list(vars = vars, data = data)
    }
    inp <- function(var) {
      vars[[var]] <<- next_data()
    }
    resolve_v2 <- function(x) {
      if (is.character(x)) vars[[x]] else x
    }
    add <- function(v1, v2) {
      vars[[v1]] <<- vars[[v1]] + resolve_v2(v2)
    }
    mul <- function(v1, v2) {
      vars[[v1]] <<- vars[[v1]] * resolve_v2(v2)
    }
    div <- function(v1, v2) {
      q <- vars[[v1]] / resolve_v2(v2)
      q <- if (q < 0) ceiling(q) else floor(q)
      vars[[v1]] <<- q
    }
    mod <- function(v1, v2) {
      vars[[v1]] <<- vars[[v1]] %% resolve_v2(v2)
    }
    eql <- function(v1, v2) {
      vars[[v1]] <<- as.numeric(vars[[v1]] == resolve_v2(v2))
    }

    list(
      inp = inp,
      add = add,
      mul = mul,
      div = div,
      mod = mod,
      eql = eql,
      show_state = show_state
    )
  }

  x <- example_data_24()

  str_gsub <- function(string, pattern, replacement) {
    gsub(pattern, replacement, string)
  }

  x <- f24_read_input()
  xs <- x |>
    # quote variable names
    str_gsub(" (w|x|y|z)", " \"\\1\"") |>
    # handle 1-arg functions
    str_gsub("inp (\".\")", "inp(\\1)") |>
    str_gsub("(mod|div|add|mul|eql) (\".\") (.+)", "\\1(\\2, \\3)")



  digits <- 93579246899999 |>
    as.character() |>
    strsplit("") |>
    unlist() |>
    as.numeric()

  digits <- 21112211322991 |>
    as.character() |>
    strsplit("") |>
    unlist() |>
    as.numeric()

  digits <- sample(9, 14, replace = TRUE)

  # first 2
  digits[1] <- 9
  digits[2] <- 9
  digits[3] <- 9
  digits[4] <- digits[3]
  digits[5] <- 5
  digits[6] <- digits[5] + 4
  digits[7] <- 6
  digits[8] <- 9
  digits[9] <- 9
  digits[10] <- 1
  digits[11] <- 9
  w1 <- digits[1]
  w2 <- digits[2]
  w3 <- digits[3]
  w4 <- digits[4]
  w5 <- digits[5]
  w6 <- digits[6]
  w7 <- digits[7]
  w8 <- digits[8]
  w9 <- digits[9]
  w10 <- digits[10]
  w11 <- digits[11]
  w12 <- digits[12]
  w13 <- digits[13]
  w14 <- digits[14]


# writeLines(paste(seq_along(xs), xs))

  a <- alu(digits)

  for (command_i in seq_along(xs)) {
    eval(parse(text = xs[command_i]), envir = a)
    x <- a$show_state()[["vars"]][["x"]]
    y <- a$show_state()[["vars"]][["y"]]
    z <- a$show_state()[["vars"]][["z"]]
    if (command_i == 54) {
      stopifnot(z == (26 * (26 * (10 + w1) + w2 + 5)) + w3 + 12)
    }
    if (command_i == 60) {
      stopifnot(x == w3)
    }
    if (command_i == 59) {
      stopifnot(z == (26 * (10 + w1) + w2 + 5))
    }
    if (command_i == 62) {
      stopifnot(x == as.numeric(w3 != w4))
    }
    if (command_i == 67) {
      stopifnot(y == 1 + 25 * (w3 != w4))
      stopifnot(z == (26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4)))
    }
    if (command_i == 72) {
      stopifnot(y == (w4 + 12) * (w3 != w4))
      z_guess <- ((26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4))) +
        (w4 + 12) * (w3 != w4)
      stopifnot(z == z_guess)
    }
    if (command_i == 80) {
      stopifnot(x == 1)
    }
    if (command_i == 85) {
      z_guess2 <- (((((26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4)))) + ((w4 + 12) * (w3 != w4))) * 26)
      stopifnot(y == 26)
      stopifnot(z == z_guess2)
    }
    if (command_i == 90) {
      # stopifnot(y == 26)
      z_guess3 <- ((((((26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4)))) + ((w4 + 12) * (w3 != w4))) * 26) + w5 + 6)
      stopifnot(z == z_guess3)
    }
    if (command_i == 95) {
      stopifnot(z == ((((26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4)))) + ((w4 + 12) * (w3 != w4))))
    }
    if (command_i == 98) {
      stopifnot(x == (w5 + 4 != w6))
    }
    if (command_i == 108) {
      z6 <- ((((((26 * (10 + w1) + w2 + 5) * (1 + 25 * (w3 != w4)))) + ((w4 + 12) * (w3 != w4))) * ((25 * ((w5 + 4) != w6)) + 1)) + ((w6 + 4) * ((w5 + 4) != w6)))
      stopifnot(y == (w6 + 4) * ((w5 + 4) != w6))
      stopifnot(z == z6)
      # stopifnot(z == z6)
      # message(z)
      # message(26 * w1 + w2 + 265)
    }
    if (command_i == 112) {
      stopifnot(x == w2 + 5)
    }
    if (command_i == 116) {
      stopifnot(x == 1)
    }
    if (command_i == 126) {
      z7 <- ((26 * z6) + (w7 + 15))
      stopifnot(z == z7)
    }
    if (command_i == 139) {
      zg <- ((25 * (w8 != (w7 + 3))) + 1) * z6
      stopifnot(z == zg)
    }
    if (command_i == 144) {
      z8 <- ((((25 * (w8 != (w7 + 3))) + 1) * z6) + (w8 + 3) * (w8 != (w7 + 3)))
      stopifnot(z == z8)
    }
    if (command_i == 162) {
      z9 <- ((z8 * 26) + w9 + 7)
      stopifnot(z == z9)
    }
    if (command_i == 180) {
      z10 <- (26 * z9 + w10 + 11)
      stopifnot(z == z10)
    }
    if (command_i == 198) {
      z11 <- (z9 * (25 * (w11 != w10 + 8) + 1)) + (w11 + 2) * (w11 != w10 + 8)
      stopifnot(z == z11)
    }
  }

  str(a$show_state())

  # g <- expand.grid(w8 = 1:9, w9 = 1:9, w10 = 1:9, w11 = 1:9, w12 = 1:9, w13 = 1:9, w14 = 1:9)
  # g <- head(g, 10000)
  # for (row_i in seq_len(nrow(g))) {
  #   d <- c(digits[1:7], unlist(g[row_i, 1:7]))
  #   a <- alu(d)
  #   for (command_i in seq_along(xs)) {
  #     eval(parse(text = xs[command_i]), envir = a)
  #     z <- a$show_state()[["vars"]][["z"]]
  #   }
  #   if (z == 0) stop(row_i)
  # }



  eval(parse(text = xs[2]), envir = a)
  str(a$show_state())

}


f24_read_input <- function() {
  path <- system.file("input24.txt", package = "adventofcode21")
  readLines(path)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day24
#' @export
example_data_24 <- function(example = 1) {
  l <- list(
    a = c(
      "inp x",
      "mul x -1"
    )
  )
  l[[example]]
}
