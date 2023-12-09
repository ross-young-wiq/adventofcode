
# INTRO -------------------------------------------------------------------

# Title:  09 Mirage Maintenance
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202309.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # split into separate numbers
  map(.f = str_split, pattern = " ", simplify = TRUE) %>% 
  
  # convert to numeric
  map(.f = as.double) %>% 
  
  # save list as variable
  force() -> lst_puz



# PART ONE ----------------------------------------------------------------

# run loop across each vector of numbers
map_dbl(
  .x = lst_puz,
  .f = function(x) {
    
    # initialise parameters
    i <- 0
    x0 <- x
    
    # repeat until we reach all zeroes
    repeat {
      if (all(unique(x0) == 0)) {break} # breakpoint
      i <- i + tail(x0, 1)              # increment tally by the last value
      x0 <- diff(x0)                    # otherwise take the difference between terms and start again
    }
    
    # return tally value
    return(i)
    
  }
) %>% 
  
  # sum the tally values
  sum()



# PART TWO  ---------------------------------------------------------------

# run loop across each vector of numbers
map(
  .x = lst_puz,
  .f = function(x) {
    
    # initialise parameters
    v <- c()
    x0 <- x
    i <- 1
    
    # repeat until we reach all zeroes
    repeat {
      v[i] <- head(x0, 1)                # save first element from each cycle
      if (all(unique(x0) == 0)) {break}  # breakpoint
      x0 <- diff(x0)                     # take the difference between terms and start again
      i <- i + 1                         # increment counter by 1
    }
    
    # return vector with the first element from each cycle
    return(v)
    
  }
) %>% 
  
  # NOTE: an interesting observation:
  # .  if v is a vector containing c(x1, x2, x3, x4, x5)
  # .  the result we want is: (x5 - (x4 - (x3 - (x2 - x1)))
  # .  this simplifies to x5 - x4 + x3 - x2 + x1 (ALTERNATING SIGN).
  # .  this means we can just change the sign of every 2nd term
  
  # do this by multiplying the vector by a repeating sequence of +1 and -1 (same length as the vector)
  map(.f = ~ .x * rep_len(c(1, -1), length.out = length(.x))) %>% 
  
  # take the sum within each vector
  map_dbl(.f = sum) %>% 
  
  # sum the result
  sum()
