box::use(R / aoc[fetch_raw_input_data, save_raw_input_data, print_answer])

input <-
  fetch_raw_input_data(2025, 4) |>
  strsplit("\n") |>
  unlist() |>
  strsplit("")

map <-do.call(rbind, input)
m <- dim(map)[[1]] # rows
n <- dim(map)[[2]] # cols

dirs <- list(
  n = c(-1, 0),
  ne = c(-1, 1),
  e = c(0, 1),
  se = c(1, 1),
  s = c(1, 0),
  sw = c(1, -1),
  w = c(0, -1),
  nw = c(-1, -1)
)

count_tp <- function(row, col, map, dirs, m, n) {
  tp <- c()
  for (d in dirs) { # get count for surround points
    if (map[row, col] == "@") {
      p <- d + c(row, col)
      if (p[1] > 0 && p[1] <= m && p[2] > 0 && p[2] <= n) { # okay to skip
        char <- map[p[1], p[2]]
        if (char == "@") {
          tp <- c(tp, char)
        }
      }
    } else {
      return(NULL) # need to flag NULLS for removal later
    }
  }
  return(length(tp))
}

{
  total_tp <- c()
  for (row in 1:n) {
    for (col in 1:m) {
      tp_count <- count_tp(row, col, map, dirs, m, n)
      if (!is.null(tp_count)) {
        total_tp <- c(total_tp, tp_count)
      }
    }
  }
  total_tp[total_tp < 4] |> length() |> print_answer()
}
