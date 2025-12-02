box::use(R / aoc[fetch_raw_input_data, save_raw_input_data, print_answer])

input <- fetch_raw_input_data(2025, 2)

data <- 
  input |>
  strsplit("[,\n]") |> 
  unlist()

{
  ids <- c()
  for (i in seq_along(data)) {
    start <- as.numeric(sub("-.*", "", data[i]))
    end <- as.numeric(sub(".*-", "", data[i]))
    seq_vec <- seq(start, end)
    ids <- c(ids, seq_vec)
  }
}

is_valid_id <- function(s) {
  l <- nchar(s)
  if (l %% 2 == 0) {
    first_half <- substr(s, 1, as.integer(l / 2))
    second_half <- substr(s, 1 + as.integer(l / 2), l)
    return(first_half == second_half)
  } else {
    return(FALSE)
  }
}

{
  valid_ids <- c()

  for (i in seq_along(ids)) {
    id <- as.character(ids[i])
    if (is_valid_id(id)) {
      valid_ids <- c(valid_ids, ids[i])
    }
  }

  valid_ids |> sum() |> print_answer()
}
