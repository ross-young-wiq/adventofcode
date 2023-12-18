
# INTRO -------------------------------------------------------------------

# Title:  15 Lens Library
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202315.txt"))



# DATA PREP ---------------------------------------------------------------

# split puzzle input by comma, and then convert to vector
v_puz <- str_split(string = puzzle, pattern = ",", simplify = TRUE) %>% as.vector()



# PART ONE ----------------------------------------------------------------

# start with puzzle vector
v_puz %>% 
  
  # convert each string from utf8 to integer
  map(.f = base::utf8ToInt) %>% 
  
  # run loop for each string
  map_dbl(
    
    # for each pair of elements, add together, multiply by 17, modulo 256, 
    # and then repeat with result and next element.
    .f = function(x) {
      purrr::reduce(
        .x = x,
        .f = function(y, z) {
          ((y + z) * 17) %% 256
        },
        .init = 0
      )
    }
  ) %>% 
  
  # sum result for each string
  sum()



# PART TWO ----------------------------------------------------------------


