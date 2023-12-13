# Advent of Code Day 2
# https://adventofcode.com/2023/day/2

library(tidyverse)

data <- readr::read_tsv("/Users/haleyfox/R/aoc/data/data_day_2.txt", col_names = FALSE) %>%
  pull(X1)

# PART 1

df <- as.data.frame(data) %>%
  rename("col" = "data") %>%
  separate_wider_delim(cols = col, delim = ": ", names = c("game", "shows")) %>%
  separate_wider_delim(cols = shows, delim = "; ", names_sep = "_", too_few = "align_start")
df_2 <- pivot_longer(df, cols = 2:ncol(df), names_to = "shows", values_to = "results")
df_2$blue <- str_match(df_2$results, "(\\d*)( blue)")[,2]
df_2$red <- str_match(df_2$results, "(\\d*)( red)")[,2]
df_2$green <- str_match(df_2$results, "(\\d*)( green)")[,2]

# Determine which games would have been possible if the bag had been loaded with
# only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the
# IDs of those games?

impossible_games <- df_2 %>%
  filter(as.numeric(blue) > 14 |
         as.numeric(red) > 12 |
         as.numeric(green) > 13) %>%
  mutate(game_id = str_match(game, "(Game )(\\d*)")[,3])

total_sum <- sum(1:100)
sum_impossible_games <- impossible_games %>%
  mutate(game_id = as.numeric(game_id)) %>%
  distinct(game_id) %>%
  summarise(sum(game_id))
total_sum - sum_impossible_games

# PART 2
# what is the fewest number of cubes of each color that could have been in the bag to make the game possible?
# The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together.
# For each game, find the minimum set of cubes that must have been present. What is the sum of the power of these sets?

# select max number for each column after grouping by game - save as new column
max_num_per_game <- df_2 %>%
  mutate(max_blue = max(as.numeric(blue), na.rm = TRUE),
         max_red = max(as.numeric(red), na.rm = TRUE),
         max_green = max(as.numeric(green), na.rm = TRUE), .by = game) %>%
  mutate(game_id = str_match(game, "(Game )(\\d*)")[,3]) %>%
  mutate(game_id = as.numeric(game_id)) %>%
  select(c(7:10)) %>%
  distinct() %>%
  mutate(power = max_blue*max_red*max_green) %>%
  summarise(sum(power))

