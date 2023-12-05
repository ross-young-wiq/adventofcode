
# INTRO -------------------------------------------------------------------

# Title:  05 If You Give A Seed A Fertilizer
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# parameters
advent_day <- "05"

# load data
puzzle <- read_lines(file = paste0("data/2023", advent_day, ".txt"))

# puzzle <- c(
#   "seeds: 79 14 55 13",
#   "",
#   "seed-to-soil map:",
#   "50 98 2",
#   "52 50 48",
#   "",
#   "soil-to-fertilizer map:",
#   "0 15 37",
#   "37 52 2",
#   "39 0 15",
#   "",
#   "fertilizer-to-water map:",
#   "49 53 8",
#   "0 11 42",
#   "42 0 7",
#   "57 7 4",
#   "",
#   "water-to-light map:",
#   "88 18 7",
#   "18 25 70",
#   "",
#   "light-to-temperature map:",
#   "45 77 23",
#   "81 45 19",
#   "68 64 13",
#   "",
#   "temperature-to-humidity map:",
#   "0 69 1",
#   "1 0 69",
#   "",
#   "humidity-to-location map:",
#   "60 56 37",
#   "56 93 4"
# )



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # get first element (seeds)
  head(1) %>% 
  
  # extract seed numbers
  str_extract_all(pattern = "\\d+", simplify = TRUE) %>% 
  
  # convert to numeric
  as.double() %>% 
  
  # save vector as variable
  force() -> v_seeds


# start with puzzle input
puzzle %>% 
  
  # remove first row
  tail(-1) %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # create new column with mapping relationship
  .[, c("map_from", "map_to") := tstrsplit(x = input, "-to-")] %>% 
  
  # clean-up
  .[, map_from := fifelse(is.na(map_to), NA_character_, map_from)] %>% 
  .[, map_to   := str_remove_all(string = map_to, pattern = " map:")] %>% 
  
  # fill values down
  tidyr::fill(map_from, map_to, .direction = "down") %>% 
  
  # remove rows without number data
  .[str_detect(string = input, pattern = "\\d")] %>% 
  
  # split input column into three components
  .[, c("dest_start", "source_start", "range_len") := tstrsplit(x = input, " ")] %>% 
  
  # convert to double
  .[, c("dest_start", "source_start", "range_len") := lapply(.SD, as.double), .SDcols = c("dest_start", "source_start", "range_len")] %>% 

  # create dest_end and source_end
  .[, `:=`(dest_end = dest_start + range_len - 1, source_end = source_start + range_len - 1)] %>% 
  
  # save as data.table
  force() -> dt
 
# create vector with order of mapped pairings 
v_map_order <- c(
  "seed",
  "soil",
  "fertilizer",
  "water",
  "light",
  "temperature",
  "humidity",
  "location"
)

# create short version of data.table
dtx <- dt[, .(map_from, source_start, source_end, dest_start, dest_end)]


# PART ONE ----------------------------------------------------------------

x <- c()
i <- 1

my_fun <- function(nbr_from0, map_from0, dt0 = dtx) {
  x0 <- dt0[map_from == map_from0 & source_start <= nbr_from0 & source_end >= nbr_from0]
  if (nrow(x0) == 0) {nbr_from0} else {x0[, dest_start + (nbr_from0 - source_start)]}
}

# brute force approach with a for loop
map_dbl(
  .x = v_seeds,
  .f = ~ .x %>% 
    my_fun(v_map_order[1]) %>% 
    my_fun(v_map_order[2]) %>% 
    my_fun(v_map_order[3]) %>% 
    my_fun(v_map_order[4]) %>% 
    my_fun(v_map_order[5]) %>% 
    my_fun(v_map_order[6]) %>% 
    my_fun(v_map_order[7]) %>% 
    my_fun(v_map_order[8])
) %>% 
  
  # lowest number
  min()
  
