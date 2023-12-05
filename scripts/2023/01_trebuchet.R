
# INTRO -------------------------------------------------------------------

# Title:  01 Trebuchet?!
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202301.txt"))



# DATA PREP ---------------------------------------------------------------

# numbers lookup table
dt <- data.table(
  chr = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
  dbl = 1:9
)



# PART ONE ----------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # remove non-numerical
  str_remove_all(pattern = "[^\\d]") %>% 
  
  # extract first and last digits from string
  str_sub_all(start = c(1, -1), end = c(1, -1)) %>% 
  
  # combine digits and convert string to numeric
  map_dbl(.f = ~ as.numeric(paste0(.x, collapse = ""))) %>% 
  
  # add numbers
  sum()



# PART TWO ----------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # run loop over each string
  map(
    .f = ~ 
      
      # find location of all patterns (start and stop position) for both text (e.g. "one") and digit (e.g. "1")
      str_locate_all(string = .x, pattern = unlist(dt)) %>% 
        
      # include reference to pattern for the locations
      set_names(nm = unlist(dt)) %>% 
      
      # convert locations to a data.table
      map(as.data.table) %>% 
      
      # bind rows
      rbindlist(idcol = "pat") %>% 
      
      # create new column which converts character patterns to digit (e.g. "one" to "1")
      .[, pat_dgt := fcoalesce(as.character(match(pat, dt$chr)), pat)] %>% 
      
      # sort by start position to order the appearance of each pattern
      .[order(start)] %>% 
      
      # take first and last pattern match, returning the digit reference
      .[c(1, .N), pat_dgt]
    
  ) %>% 
  
  # combine digits and convert string to numeric
  map_dbl(.f = ~ as.numeric(paste0(.x, collapse = ""))) %>% 
  
  # add numbers
  sum()

