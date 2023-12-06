# Advent of Code Day 1 
# https://adventofcode.com/2023/day/1

library(tidyverse)
library(stringi)
data <- readr::read_tsv("/Users/haleyfox/R/aoc/data/data_day_1.txt", col_names = FALSE) %>% 
  pull(X1)

# PART 1
data_num <- str_remove_all(data, "\\D")
first <- stri_extract_first_regex(data_num, "\\d")
last <- stri_extract_last_regex(data_num, "\\d")
combo <- str_c(first, last)
sum(as.numeric(combo))

# PART 2

# Function
string_to_numbers <- function(string) {
  # Extract locations of the matched text
  loc <- str_locate(string, "one|two|three|four|five|six|seven|eight|nine")[1,]
  # Extract the actual matched text 
  text <- str_sub(string, loc[1], loc[2])
  # if pattern = one, replace with 1, if pattern = two replace with 2, etc.
  case_when(text == "one" ~ str_replace(string, text, "1"),
                     text == "two" ~ str_replace(string, text, "2"),
                     text == "three" ~ str_replace(string, text, "3"),
                     text == "four" ~ str_replace(string, text, "4"),
                     text == "five" ~ str_replace(string, text, "5"),
                     text == "six" ~ str_replace(string, text, "6"),
                     text == "seven" ~ str_replace(string, text, "7"),
                     text == "eight" ~ str_replace(string, text, "8"),
                     text == "nine" ~ str_replace(string, text, "9"),
                     TRUE ~ string)
}

converted_1 <- vector("character")
for (i in data){
  converted_single_1 <- string_to_numbers(i)
  converted_single_2 <- string_to_numbers(converted_single_1)
  converted_single_3 <- string_to_numbers(converted_single_2)
  converted_single_4 <- string_to_numbers(converted_single_3)
  converted_single_5 <- string_to_numbers(converted_single_4)
  converted_single_6 <- string_to_numbers(converted_single_5)
  converted_single_7 <- string_to_numbers(converted_single_6)
  converted_single_8 <- string_to_numbers(converted_single_7)
  converted_single_9 <- string_to_numbers(converted_single_8)
  converted_single_10 <- string_to_numbers(converted_single_9)
  converted_1 <- c(converted_single_10, converted_1)
}

data_num <- str_remove_all(converted_1, "\\D")
first <- stri_extract_first_regex(data_num, "\\d")
last <- stri_extract_last_regex(data_num, "\\d")
combo <- str_c(first, last)
sum(as.numeric(combo))
