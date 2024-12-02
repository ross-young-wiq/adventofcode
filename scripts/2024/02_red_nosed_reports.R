
# INTRO -------------------------------------------------------------------

# Title:  02 Red-Nosed Reports
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202402.txt"))



# DATA PREP ---------------------------------------------------------------

# split levels from each individual line (report)
str_split(string = puzzle, pattern = " ") %>% 
  
  # convert from text to numeric
  map(.f = as.numeric) %>% 
  
  # save list as variable
  force() -> x



# PART ONE ----------------------------------------------------------------

# loop through list and take difference between levels
map(.x = x, .f = ~ diff(.x)) %>% 
  
  # for each report, test safety criteria
  map_dbl(.f = ~ (all(.x > 0) | all(.x < 0)) & all(abs(.x) >= 1) & all(abs(.x) <= 3)) %>% 
  
  # sum reports that are deemed safe
  sum()



# PART TWO  ---------------------------------------------------------------

# loop through list
map_lgl(
  .x = x,
  .f = function(z) {
    
    # write a for loop starting with 0 (standard case) and with a problem dampner applied (removing each value one-at-a-time)
    # then apply standard checks for safety criteria as normal
    for (i in 0:length(z)) {
      
      z0 <- if(i == 0) {z} else {z[-i]}
      z1 <- diff(z0)
      z2 <- (all(z1 > 0) | all(z1 < 0)) & all(abs(z1) >= 1) & all(abs(z1) <= 3)
      
      # if deemed safe, then return TRUE for report
      if (z2 == TRUE) {return(TRUE)}
      
    }
    
    # if no cases are safe, then return FALSE for report
    return(FALSE)
    
  }
) %>% 
  
  # sum reports that are deemed safe
  sum()

