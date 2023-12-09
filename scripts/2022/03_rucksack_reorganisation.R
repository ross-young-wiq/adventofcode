
# INTRO -------------------------------------------------------------------

# Title:  03 Rucksack Reorganisation
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202203.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # split into separate digits
  str_split(pattern = "") %>% 
  
  # save as list
  force() -> lst_puzzle



# PART ONE ----------------------------------------------------------------

# start with puzzle list
lst_puzzle %>%
  
  # take intersection of first half vs second half
  map(.f = ~ intersect(.x[1:length(.x)/2], .x[(length(.x)/2+1):length(.x)])) %>% 
  
  # convert to priority
  map_dbl(.f = match, c(letters, LETTERS)) %>% 
  
  # add
  sum()



# PART TWO  ---------------------------------------------------------------

# start with puzzle list
lst_puzzle %>%
  
  # convert to data.table
  data.table(input = . ) %>% 
  
  # separate into badge groups
  .[, badge_id := ceiling(.I/3)] %>% 
  
  # get the intersected value within each badge group
  .[, .(badge_item = reduce(input, intersect)), by = .(badge_id)] %>% 
  
  # get badge_item as a vector
  .[, badge_item] %>% 
  
  # convert to priority
  map_dbl(.f = match, c(letters, LETTERS)) %>% 
  
  # add
  sum()
  
  