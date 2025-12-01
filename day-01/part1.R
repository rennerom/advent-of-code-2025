box::use(R/aoc [fetch_raw_input_data])

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
  pos <- (pos + step) %% length(lock)
  if (pos == 0) {
    zero_counter <- zero_counter + 1
  }
}

zero_counter
