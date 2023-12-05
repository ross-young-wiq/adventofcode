
# INTRO -------------------------------------------------------------------

# Title:  02 Cube Conundrum
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202302.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # extract game id number as integer
  .[, game_id := as.integer(str_extract(string = input, pattern = "\\d+"))] %>% 
  
  # extract list of instructions (e.g. 1 green)
  .[, input := str_extract_all(string = input, pattern = "\\d+ \\w+")] %>% 
  
  # unnest list into a new long-column
  .[, .(input = unlist(input)), by = .(game_id)] %>% 
  
  # split count and colour into separate columns
  .[, c("count", "colour") := tstrsplit(input, " ")] %>% 
  
  # convert count to integer
  .[, count := as.integer(count)] %>% 
  
  # save variable as data.table
  force() -> dt_puzzle



# PART ONE ----------------------------------------------------------------

# start with puzzle data.table
dt_puzzle %>% 
  
  # left join with max_count lookup table
  merge.data.table(
    y = data.table(colour = c("red", "green", "blue"), max_count = c(12, 13, 14)), 
    by = c("colour"),
    all.x = TRUE,
    all.y = FALSE
  ) %>% 
  
  # sum cases where count exceeds the colour max count, within each game_id
  .[, .(N = sum(count > max_count, na.rm = TRUE)), by = .(game_id)] %>% 
  
  # sum the game_id values where count does not exceed colour max count
  .[N == 0, sum(game_id)]



# PART TWO ----------------------------------------------------------------

# start with puzzle input
dt_puzzle %>% 
  
  # get max count within each game_id and colour
  .[, .(max_count = max(count)), by = .(game_id, colour)] %>% 
  
  # get the product of each max_count value within each game_id
  .[, .(prod_count = prod(max_count)), by = .(game_id)] %>% 
  
  # sum the prod_count values
  .[, sum(prod_count)]
