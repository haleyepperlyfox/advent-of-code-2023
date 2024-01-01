# Advent of Code Day 5
# https://adventofcode.com/2023/day/5

library(tidyverse)

data <- readr::read_tsv("/Users/haleyfox/R/aoc/data/data_day_5.txt", col_names = FALSE) %>%
  pull(X1)
data <- c(data, "end")

# PART 1

seed_location <- function(first_map_string, second_map_string) {

  df <- data.frame()
  df_1 <- data.frame()
  all_df <- data.frame()

  # pull out rows that correspond to seed-to-soil map
  start_row <- (which(grepl(first_map_string, data))) + 1
  end_row <- (which(grepl(second_map_string, data))) - 1
  map <- data[(start_row):(end_row)]

  # for each seed
  for (source_number in 1:length(source_vec)) {
    # for each row in map
    for (row in 1:length(map)) {
      range <- as.numeric(str_match(map[row], "([0-9]*) ([0-9]*) ([0-9]*)"))
      source_range <- range[3]:(range[3]+(range[4]-1))
      # if source number is within source_range, calculate equivalent destination number
      if (as.numeric(source_vec[source_number]) %in% source_range) {
        # calculate equivalent destination number
        destination_number <- range[2] + (as.numeric(source_vec[source_number]) - range[3])
        lil_df <- data.frame(source = source_vec[source_number], #change name
                             destination = destination_number) #change name
        df <- rbind(df, lil_df)
      }
    }
    # if source_number is not within any source_range, it equals the same destination type as the source_number
    if (!(as.numeric(source_vec[source_number]) %in% df$source)) { #change name
      lil_df_1 <- data.frame(source = source_vec[source_number],
                             destination = source_vec[source_number])
      df_1 <- rbind(df_1, lil_df_1)
    }
  }
  all_df <- rbind(df, df_1)
  print(all_df)
}

# seed to soil
extract_numbers <- str_match(data[1], "seeds:(( [0-9]*){0,1000})") #NOTE - may need to increase 1000
source_vec <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[2]], ' ')))
seed_to_soil_df <- seed_location(first_map_string = "seed-to-soil map:", second_map_string = "soil-to-fertilizer map:") %>%
  rename(seed = source,
         soil = destination)

# soil-to-fertilizer
source_vec <- seed_to_soil_df$soil
soil_to_fert_df <- seed_location(first_map_string = "soil-to-fertilizer map:", second_map_string = "fertilizer-to-water map:") %>%
  rename(soil = source,
         fertilizer = destination)

# fertilizer-to-water
source_vec <- soil_to_fert_df$fertilizer
fert_to_water_df <- seed_location(first_map_string = "fertilizer-to-water map:", second_map_string = "water-to-light map:") %>%
  rename(fertilizer = source,
         water = destination)

# water-to-light
source_vec <- fert_to_water_df$water
water_to_light_df <- seed_location(first_map_string = "water-to-light map:", second_map_string = "light-to-temperature map:") %>%
  rename(water = source,
         light = destination)

# light-to-temperature
source_vec <- water_to_light_df$light
light_to_temp_df <- seed_location(first_map_string = "light-to-temperature map:", second_map_string = "temperature-to-humidity map:") %>%
  rename(light = source,
         temp = destination)

# temperature-to-humidity
source_vec <- light_to_temp_df$temp
temp_to_humidity_df <- seed_location(first_map_string = "temperature-to-humidity map:", second_map_string = "humidity-to-location map:") %>%
  rename(temp = source,
         humidity = destination)

# humidity-to-location
source_vec <- temp_to_humidity_df$humidity
humidity_to_location_df <- seed_location(first_map_string = "humidity-to-location map:", second_map_string = "end") %>%
  rename(humidity = source,
         location = destination)

## NOTE - MUST ADD "END" AS FINAL STRING IN DF
min(humidity_to_location_df$location)
# 111627841





# PART 2

# update function - still too slow to run through the ~20 billion seeds


seed_location <- function(first_map_string, second_map_string) {

  df <- data.frame()
  df_1 <- data.frame()
  all_df <- data.frame()

  # pull out rows that correspond to seed-to-soil map
  start_row <- (which(grepl(first_map_string, data))) + 1
  end_row <- (which(grepl(second_map_string, data))) - 1
  map <- data[(start_row):(end_row)]

  # for each seed
  for (source_number in 1:length(source_vec)) {
    # for each row in map
    for (row in 1:length(map)) {
      range <- as.numeric(str_match(map[row], "([0-9]*) ([0-9]*) ([0-9]*)"))
      min_source <- as.numeric(range[3])
      max_source <- as.numeric(range[3]+(range[4]-1))
      # if source number is within source_range, calculate equivalent destination number
      if (as.numeric(source_vec[source_number]) >= min_source & as.numeric(source_vec[source_number]) <= max_source) {
        # calculate equivalent destination number
        destination_number <- range[2] + (as.numeric(source_vec[source_number]) - range[3])
        lil_df <- data.frame(source = source_vec[source_number], #change name
                             destination = destination_number) #change name
        df <- rbind(df, lil_df)
      }
    }
    # if source_number is not within any source_range, it equals the same destination type as the source_number
    if (!(as.numeric(source_vec[source_number]) %in% df$source)) { #change name
      lil_df_1 <- data.frame(source = source_vec[source_number],
                             destination = source_vec[source_number])
      df_1 <- rbind(df_1, lil_df_1)
    }
  }
  all_df <- rbind(df, df_1)
  print(all_df)
}

# seed to soil
extract_numbers <- str_match(data[1], "seeds:(( [0-9]*){0,1000})") #NOTE - may need to increase 1000
source_vec_2 <- stringi::stri_remove_empty(unlist(strsplit(extract_numbers[[2]], ' ')))
source_vec_1 <- as.numeric(source_vec_2)
all_seeds_vec <- vector("numeric")
odd_numbers <- c(1,3,5,7,9,11,13,15,17,19)
for (odd_seed in odd_numbers) {
  all_seeds <- source_vec_1[odd_seed]:(source_vec_1[odd_seed] + source_vec_1[odd_seed + 1] -1)
  all_seeds_vec <- c(all_seeds_vec, all_seeds)
}
length(all_seeds_vec)/100000 #19451.69
source_vec <- all_seeds_vec[1:100000]
length(source_vec)
seed_to_soil_df <- seed_location(first_map_string = "seed-to-soil map:", second_map_string = "soil-to-fertilizer map:") %>%
  rename(seed = source,
         soil = destination)

# soil-to-fertilizer
source_vec <- seed_to_soil_df$soil
soil_to_fert_df <- seed_location(first_map_string = "soil-to-fertilizer map:", second_map_string = "fertilizer-to-water map:") %>%
  rename(soil = source,
         fertilizer = destination)

# fertilizer-to-water
source_vec <- soil_to_fert_df$fertilizer
fert_to_water_df <- seed_location(first_map_string = "fertilizer-to-water map:", second_map_string = "water-to-light map:") %>%
  rename(fertilizer = source,
         water = destination)

# water-to-light
source_vec <- fert_to_water_df$water
water_to_light_df <- seed_location(first_map_string = "water-to-light map:", second_map_string = "light-to-temperature map:") %>%
  rename(water = source,
         light = destination)

# light-to-temperature
source_vec <- water_to_light_df$light
light_to_temp_df <- seed_location(first_map_string = "light-to-temperature map:", second_map_string = "temperature-to-humidity map:") %>%
  rename(light = source,
         temp = destination)

# temperature-to-humidity
source_vec <- light_to_temp_df$temp
temp_to_humidity_df <- seed_location(first_map_string = "temperature-to-humidity map:", second_map_string = "humidity-to-location map:") %>%
  rename(temp = source,
         humidity = destination)

# humidity-to-location
source_vec <- temp_to_humidity_df$humidity
humidity_to_location_df <- seed_location(first_map_string = "humidity-to-location map:", second_map_string = "end") %>%
  rename(humidity = source,
         location = destination)

min(humidity_to_location_df$location)



