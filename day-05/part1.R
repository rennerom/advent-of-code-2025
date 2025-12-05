box::use(R / aoc[fetch_raw_input_data, save_raw_input_data, print_answer])

input <- 
  fetch_raw_input_data(2025, 5) |>
  strsplit("\n\n") |>
  unlist()

ranges <-
  input[1] |>
  strsplit("\n") |>
  unlist()

ids <- 
  input[2] |>
  strsplit("\n") |>
  unlist() |>
  as.numeric()

mins <- sapply(ranges, function(x) as.numeric(sub("-.*", "", x)), USE.NAMES = FALSE)
maxes <- sapply(ranges, function(x) as.numeric(sub(".*-", "", x)), USE.NAMES = FALSE)

is_fresh <- function(id, min, max) {
  if (id >= min && id <= max) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

good <- c()
for (id in ids) {
  for (i in  seq_along(ranges)) {
    if (is_fresh(id, mins[i], maxes[i])) {
      good <- c(good, id)
    }
  }
}

good |> unique() |> length() |> print_answer()
