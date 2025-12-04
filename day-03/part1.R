box::use(R / aoc[fetch_raw_input_data, save_raw_input_data, print_answer])

input <- fetch_raw_input_data(2025, 3)

input <-
  input |> 
  strsplit("\n") |> 
  unlist()

get_value <- function(s) {
  s_numeric <- strsplit(s, "") |> unlist() |>  as.numeric()
  first_chunk <- s_numeric |> head(-1)
  last <- s_numeric|> tail(1)
  first_chunk_max <- max(first_chunk)
  if (last > first_chunk_max) {
    return(as.numeric(paste0(first_chunk_max, last)))
  } else {
    first_highest <- s_numeric |> sort(decreasing = TRUE) |> head(1)
    index <- which(s_numeric == first_highest)[1]
    second_highest <- s_numeric |> tail(-index) |> max()
    return(as.numeric(paste0(first_highest, second_highest)))
  }
}

sapply(
  input,
  get_value,
  USE.NAMES = FALSE
) |> 
  sum() |> 
  print_answer()
