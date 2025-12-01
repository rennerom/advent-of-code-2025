box::use(R/aoc [fetch_raw_input_data, print_answer])

input <-
  fetch_raw_input_data(2025, 1) |>
  unlist() |>
  strsplit("\n") |>
  sapply(sub, pattern = "L", replacement = "-", USE.NAMES = FALSE) |>
  sapply(sub, pattern = "R", replacement = "", USE.NAMES = FALSE) |>
  as.integer()

lock <- seq(0, 99)
pos <- 50
zero_counter <- 0

for (step in input) {
  # probably a better way to do this...
  for (tick in seq_len(abs(step))) {
    if (step < 0) {
      pos <- (pos - 1) %% length(lock)
      if (pos == 0) {
        zero_counter <- zero_counter + 1
      }
    } else {
      pos <- (pos + 1) %% length(lock)
      if (pos == 0) {
        zero_counter <- zero_counter + 1
      }
    }
  }
}

zero_counter |> print_answer()

