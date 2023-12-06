
# INTRO -------------------------------------------------------------------

# Title:  06 Wait For It 
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202306.txt"))



# PREP DATA ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # keep only numeric data and convert to matrix
  str_extract_all(pattern = "\\d+", simplify = TRUE) %>% 
  
  # save as input data
  force() -> mat_data


# create function to 
# (1) calculate equation with time: x * s * (t - x), where:
# . t is the time limit 'time';
# . s is the speed limit (default set to 1)
# . x is a number between 1 and t (can ignore 0 because no movement)
# (2) determine number of cases that exceed current record distance 'dist'
my_fun <- function(time, dist, speed = 1) {
  
  map_dbl(.x = seq(time), .f = ~ .x * speed * (time - .x)) %>% 
    
    {sum(. > dist)}
  
}



# PART ONE ----------------------------------------------------------------

# run function
map2_dbl(
  .x = as.numeric(mat_data[1,]), 
  .y = as.numeric(mat_data[2,]), 
  .f = my_fun
) %>% 
  
  # multiply result
  prod()



# PART TWO ----------------------------------------------------------------

# run function
map2_dbl(
  .x = as.numeric(paste0(mat_data[1,], collapse = "")),
  .y = as.numeric(paste0(mat_data[2,], collapse = "")),
  .f = my_fun
) %>% 
  
  # multiply result
  prod()


