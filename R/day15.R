#' Day 15: Chiton
#'
#' [Chiton](https://adventofcode.com/2021/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You\'ve almost reached the exit of the cave, but the walls are getting
#' closer together. Your submarine can barely still fit, though; the main
#' problem is that the walls of the cave are covered in
#' [chitons](https://en.wikipedia.org/wiki/Chiton), and it would be best
#' not to bump any of them.
#'
#' The cavern is large, but has a very low ceiling, restricting your motion
#' to two dimensions. The shape of the cavern resembles a square; a quick
#' scan of chiton density produces a map of *risk level* throughout the
#' cave (your puzzle input). For example:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' You start in the top left position, your destination is the bottom right
#' position, and you [cannot move
#' diagonally]{title="Can't go diagonal until we can repair the caterpillar unit. Could be the liquid helium or the superconductors."}.
#' The number at each position is its *risk level*; to determine the total
#' risk of an entire path, add up the risk levels of each position you
#' *enter* (that is, don\'t count the risk level of your starting position
#' unless you enter it; leaving it adds no risk to your total).
#'
#' Your goal is to find a path with the *lowest total risk*. In this
#' example, a path with the lowest total risk is highlighted here:
#'
#'     1163751742
#'     1381373672
#'     2136511328
#'     3694931569
#'     7463417111
#'     1319128137
#'     1359912421
#'     3125421639
#'     1293138521
#'     2311944581
#'
#' The total risk of this path is `40` (the starting position is never
#' entered, so its risk is not counted).
#'
#' *What is the lowest total risk of any path from the top left to the
#' bottom right?*
#'
#' **Part Two**
#'
#' Now that you know how to find low-risk paths in the cave, you can try to
#' find your way out.
#'
#' The entire cave is actually *five times larger in both dimensions* than
#' you thought; the area you originally scanned is just one tile in a 5x5
#' tile area that forms the full map. Your original map tile repeats to the
#' right and downward; each time the tile repeats to the right or downward,
#' all of its risk levels *are 1 higher* than the tile immediately up or
#' left of it. However, risk levels above `9` wrap back around to `1`. So,
#' if your original map had some position with a risk level of `8`, then
#' that same position on each of the 25 total tiles would be as follows:
#'
#'     8 9 1 2 3
#'     9 1 2 3 4
#'     1 2 3 4 5
#'     2 3 4 5 6
#'     3 4 5 6 7
#'
#' Each single digit above corresponds to the example position with a value
#' of `8` on the top-left tile. Because the full map is actually five times
#' larger in both dimensions, that position appears a total of 25 times,
#' once in each duplicated tile, with the values shown above.
#'
#' Here is the full five-times-as-large version of the first example above,
#' with the original map in the top left corner highlighted:
#'
#'     11637517422274862853338597396444961841755517295286
#'     13813736722492484783351359589446246169155735727126
#'     21365113283247622439435873354154698446526571955763
#'     36949315694715142671582625378269373648937148475914
#'     74634171118574528222968563933317967414442817852555
#'     13191281372421239248353234135946434524615754563572
#'     13599124212461123532357223464346833457545794456865
#'     31254216394236532741534764385264587549637569865174
#'     12931385212314249632342535174345364628545647573965
#'     23119445813422155692453326671356443778246755488935
#'     22748628533385973964449618417555172952866628316397
#'     24924847833513595894462461691557357271266846838237
#'     32476224394358733541546984465265719557637682166874
#'     47151426715826253782693736489371484759148259586125
#'     85745282229685639333179674144428178525553928963666
#'     24212392483532341359464345246157545635726865674683
#'     24611235323572234643468334575457944568656815567976
#'     42365327415347643852645875496375698651748671976285
#'     23142496323425351743453646285456475739656758684176
#'     34221556924533266713564437782467554889357866599146
#'     33859739644496184175551729528666283163977739427418
#'     35135958944624616915573572712668468382377957949348
#'     43587335415469844652657195576376821668748793277985
#'     58262537826937364893714847591482595861259361697236
#'     96856393331796741444281785255539289636664139174777
#'     35323413594643452461575456357268656746837976785794
#'     35722346434683345754579445686568155679767926678187
#'     53476438526458754963756986517486719762859782187396
#'     34253517434536462854564757396567586841767869795287
#'     45332667135644377824675548893578665991468977611257
#'     44961841755517295286662831639777394274188841538529
#'     46246169155735727126684683823779579493488168151459
#'     54698446526571955763768216687487932779859814388196
#'     69373648937148475914825958612593616972361472718347
#'     17967414442817852555392896366641391747775241285888
#'     46434524615754563572686567468379767857948187896815
#'     46833457545794456865681556797679266781878137789298
#'     64587549637569865174867197628597821873961893298417
#'     45364628545647573965675868417678697952878971816398
#'     56443778246755488935786659914689776112579188722368
#'     55172952866628316397773942741888415385299952649631
#'     57357271266846838237795794934881681514599279262561
#'     65719557637682166874879327798598143881961925499217
#'     71484759148259586125936169723614727183472583829458
#'     28178525553928963666413917477752412858886352396999
#'     57545635726865674683797678579481878968159298917926
#'     57944568656815567976792667818781377892989248891319
#'     75698651748671976285978218739618932984172914319528
#'     56475739656758684176786979528789718163989182927419
#'     67554889357866599146897761125791887223681299833479
#'
#' Equipped with the full map, you can now find a path from the top left
#' corner to the bottom right corner with the lowest total risk:
#'
#'     11637517422274862853338597396444961841755517295286
#'     13813736722492484783351359589446246169155735727126
#'     21365113283247622439435873354154698446526571955763
#'     36949315694715142671582625378269373648937148475914
#'     74634171118574528222968563933317967414442817852555
#'     13191281372421239248353234135946434524615754563572
#'     13599124212461123532357223464346833457545794456865
#'     31254216394236532741534764385264587549637569865174
#'     12931385212314249632342535174345364628545647573965
#'     23119445813422155692453326671356443778246755488935
#'     22748628533385973964449618417555172952866628316397
#'     24924847833513595894462461691557357271266846838237
#'     32476224394358733541546984465265719557637682166874
#'     47151426715826253782693736489371484759148259586125
#'     85745282229685639333179674144428178525553928963666
#'     24212392483532341359464345246157545635726865674683
#'     24611235323572234643468334575457944568656815567976
#'     42365327415347643852645875496375698651748671976285
#'     23142496323425351743453646285456475739656758684176
#'     34221556924533266713564437782467554889357866599146
#'     33859739644496184175551729528666283163977739427418
#'     35135958944624616915573572712668468382377957949348
#'     43587335415469844652657195576376821668748793277985
#'     58262537826937364893714847591482595861259361697236
#'     96856393331796741444281785255539289636664139174777
#'     35323413594643452461575456357268656746837976785794
#'     35722346434683345754579445686568155679767926678187
#'     53476438526458754963756986517486719762859782187396
#'     34253517434536462854564757396567586841767869795287
#'     45332667135644377824675548893578665991468977611257
#'     44961841755517295286662831639777394274188841538529
#'     46246169155735727126684683823779579493488168151459
#'     54698446526571955763768216687487932779859814388196
#'     69373648937148475914825958612593616972361472718347
#'     17967414442817852555392896366641391747775241285888
#'     46434524615754563572686567468379767857948187896815
#'     46833457545794456865681556797679266781878137789298
#'     64587549637569865174867197628597821873961893298417
#'     45364628545647573965675868417678697952878971816398
#'     56443778246755488935786659914689776112579188722368
#'     55172952866628316397773942741888415385299952649631
#'     57357271266846838237795794934881681514599279262561
#'     65719557637682166874879327798598143881961925499217
#'     71484759148259586125936169723614727183472583829458
#'     28178525553928963666413917477752412858886352396999
#'     57545635726865674683797678579481878968159298917926
#'     57944568656815567976792667818781377892989248891319
#'     75698651748671976285978218739618932984172914319528
#'     56475739656758684176786979528789718163989182927419
#'     67554889357866599146897761125791887223681299833479
#'
#' The total risk of this path is `315` (the starting position is still
#' never entered, so its risk is not counted).
#'
#' Using the full map, *what is the lowest total risk of any path from the
#' top left to the bottom right?*
#'
#' @param x some data
#' @return For Part One, `f15a_dijkstra(x)` returns a dataframe of edge costs.
#' @export
#' @examples
#' f15a_dijkstra(example_data_15())
f15a_dijkstra <- function(x, repeat_5 = FALSE) {
  # strategy: dijkstra's algorithm + fake dplyr functions
  set_start <- function(df, i, j) {
    df[df[["row"]] == i & df[["col"]] == j, "cost"] <- 0
    df
  }
  visit_next_node <- function(df) {
    current_node <- df[1, ]
    current_node$visited <- TRUE

    neighbors <- current_node |>
      # find unvisited neighboring nodes
      f15_create_neighbor_df(max(df$row), max(df$col)) |>
      select_cols(row = "neigh_row", col = "neigh_col", "id")

    # filtering rows to avoid merge()
    neighbors <- df[df$id %in% neighbors$id, ] |>
      filter_rows(~ visited == FALSE) |>
      # update their cost if better than current cost
      mutate_rows(
        via_row = ~ ifelse(
          current_node$cost + value < cost,
          current_node$row,
          via_row
        ),
        via_col = ~ ifelse(
          current_node$cost + value < cost,
          current_node$col,
          via_col
        ),
        cost = ~ ifelse(
          current_node$cost + value < cost,
          current_node$cost + value,
          cost
        )
      )

    changed_rows <- rbind(current_node, neighbors)
    df <- df |>
      filter_rows(~ ! id %in% changed_rows$id) |>
      rbind(changed_rows)

    # sort so it is a priority queue
    df <- df[order(df$visited, df$cost), , drop = FALSE]
    df
  }

  has_visited_node <- function(df, i, j) {
    df[df$row == i & df$col == j, "visited", drop = TRUE]
  }

  df <- x |> f15_prepare_input(repeat_5) |> set_start(1, 1)
  row_target <- max(df$row)
  col_target <- max(df$col)

  while(!has_visited_node(df, row_target, col_target)) {
    df <- df |> visit_next_node()
  }

  df
}

f15_create_neighbor_df <- function(df, nrows, ncols) {
  df_minimal <- df[c("row", "col")]
  df_candidates <- df_minimal |>
    split(seq_len(nrow(df))) |>
    lapply(function(df) {
      data.frame(
        source_row = df$row,
        source_col = df$col,
        row = df$row + c(-1, 1,  0, 0),
        col = df$col + c( 0, 0, -1, 1)
      )
    }) |>
    f_reduce(rbind) |>
    filter_rows(~ row <= nrows, ~ col <= ncols, ~ 0 < row, ~ 0 < col)

  df_candidates$id <- df_candidates$row * nrows + df_candidates$col - ncols
  names(df_candidates) <- c("row", "col", "neigh_row", "neigh_col", "id")
  df_candidates
}

f15_prepare_input <- function(x, repeat_5 = FALSE) {
  create_dijkstra_df <- function(i, j, value) {
    data.frame(
      id = NA_integer_,
      row = i,
      col = j,
      value,
      cost = Inf,
      via_row = NA,
      via_col = NA,
      visited = FALSE
    )
  }

  # use a dataframe of indices
  l <- x |>
    strsplit("") |>
    lapply(as.numeric)

  if (repeat_5) {
    bump_by_n <- function(l, n) {
      lapply(l, function(x) mod1(x + n, 9))
    }
    l <- l |>
      lapply(function(li) mod1(c(li, li + 1, li + 2, li + 3, li + 4), 9))

    l <- c(
      l,
      bump_by_n(l, 1),
      bump_by_n(l, 2),
      bump_by_n(l, 3),
      bump_by_n(l, 4)
    )
  }

  df <- l |>
    f_map2(
      seq_along(l),
      function(vs, is) create_dijkstra_df(is, seq_along(vs), vs)
    ) |>
    f_reduce(rbind)

  df$id <- seq_along(df$id)
  df
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = c(
      "1163751742",
      "1381373672",
      "2136511328",
      "3694931569",
      "7463417111",
      "1319128137",
      "1359912421",
      "3125421639",
      "1293138521",
      "2311944581"
    )
  )
  l[[example]]
}
