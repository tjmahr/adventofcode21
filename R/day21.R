#' Day 21: Dirac Dice
#'
#' [Dirac Dice](https://adventofcode.com/2021/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' There\'s not much to do as you slowly descend to the bottom of the
#' ocean. The submarine computer [challenges you to a nice
#' game]{title="A STRANGE GAME."} of *Dirac Dice*.
#'
#' This game consists of a single
#' [die](https://en.wikipedia.org/wiki/Dice), two
#' [pawns](https://en.wikipedia.org/wiki/Glossary_of_board_games#piece),
#' and a game board with a circular track containing ten spaces marked `1`
#' through `10` clockwise. Each player\'s *starting space* is chosen
#' randomly (your puzzle input). Player 1 goes first.
#'
#' Players take turns moving. On each player\'s turn, the player rolls the
#' die *three times* and adds up the results. Then, the player moves their
#' pawn that many times *forward* around the track (that is, moving
#' clockwise on spaces in order of increasing value, wrapping back around
#' to `1` after `10`). So, if a player is on space `7` and they roll `2`,
#' `2`, and `1`, they would move forward 5 times, to spaces `8`, `9`, `10`,
#' `1`, and finally stopping on `2`.
#'
#' After each player moves, they increase their *score* by the value of the
#' space their pawn stopped on. Players\' scores start at `0`. So, if the
#' first player starts on space `7` and rolls a total of `5`, they would
#' stop on space `2` and add `2` to their score (for a total score of `2`).
#' The game immediately ends as a win for any player whose score reaches
#' *at least `1000`*.
#'
#' Since the first game is a practice game, the submarine opens a
#' compartment labeled *deterministic dice* and a 100-sided die falls out.
#' This die always rolls `1` first, then `2`, then `3`, and so on up to
#' `100`, after which it starts over at `1` again. Play using this die.
#'
#' For example, given these starting positions:
#'
#'     Player 1 starting position: 4
#'     Player 2 starting position: 8
#'
#' This is how the game would go:
#'
#' -   Player 1 rolls `1`+`2`+`3` and moves to space `10` for a total score
#'     of `10`.
#' -   Player 2 rolls `4`+`5`+`6` and moves to space `3` for a total score
#'     of `3`.
#' -   Player 1 rolls `7`+`8`+`9` and moves to space `4` for a total score
#'     of `14`.
#' -   Player 2 rolls `10`+`11`+`12` and moves to space `6` for a total
#'     score of `9`.
#' -   Player 1 rolls `13`+`14`+`15` and moves to space `6` for a total
#'     score of `20`.
#' -   Player 2 rolls `16`+`17`+`18` and moves to space `7` for a total
#'     score of `16`.
#' -   Player 1 rolls `19`+`20`+`21` and moves to space `6` for a total
#'     score of `26`.
#' -   Player 2 rolls `22`+`23`+`24` and moves to space `6` for a total
#'     score of `22`.
#'
#' \...after many turns\...
#'
#' -   Player 2 rolls `82`+`83`+`84` and moves to space `6` for a total
#'     score of `742`.
#' -   Player 1 rolls `85`+`86`+`87` and moves to space `4` for a total
#'     score of `990`.
#' -   Player 2 rolls `88`+`89`+`90` and moves to space `3` for a total
#'     score of `745`.
#' -   Player 1 rolls `91`+`92`+`93` and moves to space `10` for a final
#'     score, `1000`.
#'
#' Since player 1 has at least `1000` points, player 1 wins and the game
#' ends. At this point, the losing player had `745` points and the die had
#' been rolled a total of `993` times; `745 * 993 = 739785`.
#'
#' Play a practice game using the deterministic 100-sided die. The moment
#' either player wins, *what do you get if you multiply the score of the
#' losing player by the number of times the die was rolled during the
#' game?*
#'
#' **Part Two**
#'
#' Now that you\'re warmed up, it\'s time to play the real game.
#'
#' A second compartment opens, this time labeled *Dirac dice*. Out of it
#' falls a single three-sided die.
#'
#' As you experiment with the die, you feel a little strange. An
#' informational brochure in the compartment explains that this is a
#' *quantum die*: when you roll it, the universe *splits into multiple
#' copies*, one copy for each possible outcome of the die. In this case,
#' rolling the die always splits the universe into *three copies*: one
#' where the outcome of the roll was `1`, one where it was `2`, and one
#' where it was `3`.
#'
#' The game is played the same as before, although to prevent things from
#' getting too far out of hand, the game now ends when either player\'s
#' score reaches at least `21`.
#'
#' Using the same starting positions as in the example above, player 1 wins
#' in `444356092776315` universes, while player 2 merely wins in
#' `341960390180808` universes.
#'
#' Using your given starting positions, determine every possible outcome.
#' *Find the player that wins in more universes; in how many universes does
#' that player win?*
#'
#' @param x some data
#' @return For Part One, `f21a_play_deterministic_game(x)` returns the product
#'   of the losing player's score and the total number of rolls. For Part Two,
#'   `f21b_play_quantum_game(x)` returns the total win counts for each player.
#' @export
#' @examples
#' f21a_play_deterministic_game(example_data_21())
f21a_play_deterministic_game <- function(x) {
  # strategy: objects, the mod1() function
  g <- f21_new_game(x)

  while(g$is_active()) g$roll()

  h <- g$get_history()
  tail(h$score, 2)[1] * tail(h$total_rolls, 1)
}


#' @rdname day21
#' @export
f21b_play_quantum_game <- function(x) {
  # strategy: split-apply-combine, working with row counts instead of rows

  # optimization: use do.call(rbind, ...) instead of Reduce(rbind, ...)
  f_do_call <- function(x, f, ...) do.call(f, x, ...)
  rbind_good <- function(...) rbind(..., make.row.names = FALSE)

  # determine the 27 outcomes for this row
  update_row <- function(row) {
    active <- row[["active"]]
    active_position <- sprintf("p%s_position", active)
    active_score <- sprintf("p%s_score", active)
    active_count <- sprintf("p%s_count", active)

    result <- rbind(row, row, row, row, row, row, row)

    result$active <- mod1(active + 1, 2)
    result[[active_position]] <- mod1(result[[active_position]] + c(3:9), 10)
    result[[active_score]] <- result[[active_score]] + result[[active_position]]
    result[[active_count]] <- result[[active_count]] * c(1, 3, 6, 7, 6, 3, 1)

    result$winner <- ifelse(result[[active_score]] >= 21, active, 0)
    result
  }

  update_active_player <- function(data) {
    if (!nrow(data$active)) {
      return(data)
    }

    # for each row, count the paths from the current state
    df_ways <- data$active |>
      split(seq_len(nrow(data[["active"]]))) |>
      lapply(update_row) |>
      f_do_call(rbind_good)

    # discharge winners
    df_winners <- df_ways[df_ways[["winner"]] != 0, ]
    df_ways <- df_ways[df_ways$winner == 0, ]

    if (nrow(df_winners)) {
      df_new_wins <- df_winners |>
        split(~ winner) |>
        lapply(function(x) {
          data.frame(
            winner = unique(x$winner),
            ways = sum(x$p1_count * x$p2_count),
            type = "attained"
          )
        }) |>
        f_reduce(rbind)
      data$wins <- rbind(data$wins, df_new_wins)
    }


    # optimization: discharge rows where next player must win
    next_player <- unique(df_ways$active)
    next_player_score <- sprintf("p%s_score", next_player)
    next_player_count <- sprintf("p%s_count", next_player)

    df_implied_wins <- data.frame()
    if (nrow(df_ways)) {
      df_implied_wins <- df_ways[df_ways[[next_player_score]] > 20, ]
      df_ways <- df_ways[df_ways[[next_player_score]] <= 20, ]
    }

    if (nrow(df_implied_wins)) {
      df_total_implied_wins <- data.frame(
        winner = next_player,
        ways = sum(df_implied_wins[[next_player_count]] * 27),
        type = "implied"
      )
      data$wins <- rbind(data$wins, df_total_implied_wins)
    }

    # optimization?: compact the rows
    if (nrow(df_ways)) {
      if (unique(data[["active"]][["active"]]) == 1) {
        df_ways <- aggregate(
          p1_count ~ active + p1_position + p1_score +
            p2_score + p2_count + p2_position + winner,
          df_ways,
          sum
        )
      } else {
        df_ways <- aggregate(
          p2_count ~ active + p1_position + p1_score +
            p2_score + p1_count + p2_position + winner,
          df_ways,
          sum
        )
      }
    }

    data$active <- df_ways
    data
  }

  p <- f21_parse_input(x)

  setup <- data.frame(
    active = 1,
    p1_position = p[1],
    p1_score = 0,
    p1_count = 1,
    p2_position = p[2],
    p2_score = 0,
    p2_count = 1,
    winner = 0
  )

  data <- list(active = setup, wins = NULL)
  while (nrow(data$active)) {
    data <- data |>
      update_active_player()
  }

  results <- data$wins |>
    split(~ winner) |>
    lapply(function(xs) {
      data.frame(
        winner = unique(xs[["winner"]]),
        ways = sum(xs[["ways"]])
      )
    }) |>
    f_reduce(rbind)
  results
}


f21_new_game <- function(x) {
  p <- f21_parse_input(x)

  # I have never tried using a "this" or "self" in a closure-based object
  # before. This is experimental stuff.
  self <- list()
  players <- list(
    list(position = p[1], score = 0, player = 1),
    list(position = p[2], score = 0, player = 2)
  )

  current <- 1:3
  turn <- 1
  total_rolls <- 0
  active_player <- 1
  active_game <- TRUE

  history <- data.frame(
    turn = numeric(0),
    active = numeric(0),
    roll = numeric(0),
    total_rolls = numeric(0),
    position = numeric(0),
    score = numeric(0),
    active_game = logical(0)
  )

  update_history <- function() {
    current_score <- players[[active_player]][["score"]]
    if (current_score >= 1000 || !active_game) {
      active_game <<- FALSE
    }
    d <- data.frame(
      turn = turn,
      active = active_player,
      roll = sum(current),
      total_rolls = total_rolls,
      position = players[[active_player]][["position"]],
      score = current_score,
      active_game = active_game
    )
    history <<- rbind(history, d)
    self
  }

  self$roll <- function() {
    give <- sum(current)
    total_rolls <<- total_rolls + 3
    players[[active_player]][["position"]] <<-
      (players[[active_player]][["position"]] + give) |>
      mod1(10)

    players[[active_player]][["score"]] <<-
      players[[active_player]][["score"]] +
      players[[active_player]][["position"]]

    update_history()

    turn <<- turn + 1
    current <<- mod1(current + 3, 100)
    active_player <<- mod1(active_player + 1, 2)
    self
  }

  self$roll_n <- function(n) {
    for (i in seq_len(n)) {
      self$roll()
    }
    self
  }

  self$get_history <- function() history
  self$is_active <- function() active_game
  self
}


f21_parse_input <- function(x) {
  x <- gsub("Player . starting position: ", "", x) |> as.numeric()
  x
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day21
#' @export
example_data_21 <- function(example = 1) {
  l <- list(
    a = c(
      "Player 1 starting position: 4",
      "Player 2 starting position: 8"
    )
  )
  l[[example]]
}
