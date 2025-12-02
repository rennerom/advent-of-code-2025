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
  if (l < 2) {
    return(FALSE) # single digits cant repeat
  }
  for (i in 1:as.integer(l / 2)) { # these are the possbile chunk sizes allowed
    if (l %% i == 0) { # and they have to be a multiple of the original id length
      chunk <- substr(s, 1, i)
      multiples <- as.integer(l / i)
      reconstructed_s <- paste0(rep(chunk, multiples), collapse = "")
      if (s == reconstructed_s) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
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
