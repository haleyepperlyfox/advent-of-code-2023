# Advent of Code Day 4
# https://adventofcode.com/2023/day/4

library(tidyverse)

data <- readr::read_tsv("/Users/haleyfox/R/aoc/data/data_day_4.txt", col_names = FALSE) %>%
  pull(X1)

# PART 1
points_all <- vector("numeric")
for (i in 1:length(data)) {
extract_numbers <- str_match(data[[i]], "(([0-9]* ){10,20})[\\|](( [0-9]*){20,40})")
winning_nos <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[2]], ' ')))
matching_nos <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[4]], ' ')))
matches <- matching_nos %in% winning_nos
total_matches <- sum(matches)
# if total_matches > 0 then do this, else do nothing because 2^-1 = 0.5
if (total_matches > 0) {
points <- 2^(total_matches-1)
points_all <- sum(points_all, points)
}
}
# 21138

# PART 2
all_rows <- list(rows = vector("numeric"),
              no_matches = vector("numeric"),
              matching_rows = list(vector("numeric")),
              times_present = vector("integer"))

for (i in 1:length(data)) {
extract_numbers <- str_match(data[[i]], "(([0-9]* ){0,20})[\\|](( [0-9]*){0,40})")
winning_nos <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[2]], ' ')))
matching_nos <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[4]], ' ')))
matches <- matching_nos %in% winning_nos
num_matches <- sum(matches)
if (num_matches > 0) {
row_list <- list(rows = i,
                 no_matches = num_matches,
                 matching_rows = list(c(rep((i+1):(i+num_matches)))),
                 times_present = c(1))
} else {
  row_list <- list(rows = i,
                   no_matches = num_matches,
                   matching_rows = c(0),
                   times_present = c(1))
}
keys <- unique(names(row_list))
all_rows <- setNames(mapply(c, all_rows[keys], row_list[keys]), keys)
}

fix_matching_rows <- all_rows$matching_rows[-1]
updated <- list(rows = all_rows$rows, 
                no_matches = all_rows$no_matches, 
                matching_rows = fix_matching_rows,
                times_present = all_rows$times_present)

dt <- data.table::as.data.table(updated)
for (i in 2:nrow(dt)) {
  for (prev_row in 1:(i)) {
    if (dt$rows[[i]] %in% dt$matching_rows[[prev_row]]) {
      dt$times_present[[i]] <- (dt$times_present[[i]] + dt$times_present[[prev_row]])
    }
  }
}
sum(dt$times_present)
# 7185540