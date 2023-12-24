# Advent of Code Day 2
# https://adventofcode.com/2023/day/3

library(tidyverse)

data <- readr::read_tsv("/Users/haleyfox/R/aoc/data/data_day_3.txt", col_names = FALSE) %>%
  pull(X1)

# PART 1
# any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum.

# Replace "." with a letter and replace all symbols with a * for consistency
data_adj <- data %>%
  str_replace_all("\\.", "A") %>%
  str_replace_all("[^[:alnum:]]", "*")

true_numbers <- vector("character")
n <- length(data)

for (i in 1:n){

# if there is a number in the row run this code, else print hello
if (str_detect(data_adj[i], "\\d+") == TRUE) {

  # find start and end positions of all numbers
  number_positions <- str_locate_all(data_adj[i], "\\d+")
  all <- do.call(rbind, number_positions)

  # for each number in the selected row of the df
  b <- nrow(all)

  for (loc_row in 1:b){
    start <- all[loc_row, 1]
    end <- all[loc_row, 2]
    # find positions where a symbol needs to be to make it a true number
    start_symbol_positions <- c((start),
                            (start - 1),
                            (start + 1))
    end_symbol_positions <- c((end),
                          (end - 1),
                          (end + 1))
    # for each individ number in a number, scan the positions a symbol would need to be
    rows_to_scan <- c(data_adj[i-1], data_adj[i], data_adj[i+1])
    text_1 <- str_sub(rows_to_scan, start_symbol_positions[1], end_symbol_positions[1])
    text_2 <- str_sub(rows_to_scan, start_symbol_positions[2], end_symbol_positions[2])
    text_3 <- str_sub(rows_to_scan, start_symbol_positions[3], end_symbol_positions[3])
    text_4 <- paste(text_1, text_2, text_3, collapse = "")
    # if there is a symbol in one of those crucial positions, extract the number
    has_symbol <- str_detect(text_4, "\\*")
    true_number <- case_when(has_symbol ~ str_sub(data_adj[i], start = all[loc_row,1], end = all[loc_row,2]),
                TRUE ~ "0")
    # bind all numbers together
    true_numbers <- c(true_number, true_numbers)
  }
}
}

true_numbers <- as.numeric(true_numbers)
sum(true_numbers)
# 556367


# PART 2
# A gear is any * symbol that is adjacent to exactly two part numbers. Its gear
# ratio is the result of multiplying those two numbers together.

numbers <- vector("numeric")
numbers_all <- vector("numeric")
gear_ratio <- vector("numeric")

n <- length(data)
for (i in 1:n){
    # find start and end positions of all stars
    star_positions <- str_locate_all(data[i], "\\*")
    all <- do.call(rbind, star_positions)

    b <- nrow(all)
    for (loc_row in 1:b){
      if (nrow(all) > 0) {
      position <- all[loc_row, 1]
      # find positions where a symbol needs to be to make it a true number
      symbol_positions <- c((position - 1),
                            (position),
                            (position + 1))

      # for each * location, scan the positions a number would need to be
      rows_to_scan <- c(data[i-1], data[i], data[i+1])
      text_1 <- str_sub(rows_to_scan[1], symbol_positions[1], symbol_positions[3]) %>%
        paste(collapse = "")
      text_2 <- str_sub(rows_to_scan[2], symbol_positions[1], symbol_positions[3]) %>%
        paste(collapse = "")
      text_3 <- str_sub(rows_to_scan[3], symbol_positions[1], symbol_positions[3]) %>%
        paste(collapse = "")

      # if two of these text dfs has a number, then this * is next to 2 numbers
      # EDIT HERE - LIKELY MISSING STARS WHERE ONLY ONE ROW HAS DIGITS
      check_numbers_1 <- data.frame(
        digit = c(case_when(str_detect(text_1, "\\d\\D\\d") ~ 2,
                            str_detect(text_1, "\\d") ~ 1,
                            TRUE ~ 0),
                  case_when(str_detect(text_2, "\\d\\D\\d") ~ 2,
                            str_detect(text_2, "\\d") ~ 1,
                            TRUE ~ 0),
                  case_when(str_detect(text_3, "\\d\\D\\d") ~ 2,
                            str_detect(text_3, "\\d") ~ 1,
                            TRUE ~ 0)
        ))

      if (sum(check_numbers_1$digit) == 2) {

      # find positions of numbers in text dfs that touch stars
      num_position_1 <- str_locate_all(c(text_1, text_2, text_3), "\\d")
      numbers <- vector("numeric")

      # for each row in the dataframe
      for (d in 1:3){

        if (nrow(num_position_1[[d]]) > 0) {
        e <- nrow(num_position_1[[d]])

        # for each matching number location in the row (3 potential matching digits each row)
        # pull out the actual position of that number in the original row
        for (c in 1:e){
          extract_num_loc <- case_when(num_position_1[[d]][c] == 1 ~ symbol_positions[1],
                                   num_position_1[[d]][c] == 2 ~ symbol_positions[2],
                                   num_position_1[[d]][c] == 3 ~ symbol_positions[3],
                               TRUE ~ 0) %>%
            as.numeric()
          # extract 2 characters before and 2 characters after the location
          # filter to only where digits are touching another digit
          num_1 <- substr(rows_to_scan[d], (extract_num_loc-2), (extract_num_loc+2)) %>%
            str_extract('[[:digit:]]{2,3}')
          # if num_1 = NA then pull out the single number in the matching position
          if (is.na(num_1)) {
            num_1 <- substr(rows_to_scan[d], (extract_num_loc-1), (extract_num_loc+1)) %>%
              str_extract('[[:digit:]]{1}')
          }
          numbers <- c(num_1, numbers) %>%
            unique() %>%
            as.numeric()
        }
        }
      }
      if (length(numbers) == 1) { # this applies if the * is touching two of the same number (e.g., 950 and 950 are unique numbers)
        numbers <- c(numbers, numbers)
      }
      numbers_all <- c(numbers, numbers_all)
      }
      }
    }
}

# multiply two numbers together that are touching the same star
pairs <- split(numbers_all, cut(seq_along(numbers_all), breaks = length(numbers_all)/2, labels = FALSE))
ratios <- vector("numeric")

for (row in 1:length(pairs)) {
  ratio <- pairs[[row]][[1]] * pairs[[row]][[2]]
  ratios <- c(ratio, ratios)
}

sum(ratios)
# 89471771
