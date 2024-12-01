
# INTRO -------------------------------------------------------------------

# Title:  00 Historian Hysteria
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202401.txt"))



# DATA PREP ---------------------------------------------------------------

# split input into two pieces
str_split(string = puzzle, pattern = "   ", n = 2, simplify = TRUE) %>%
  
  # convert matrix to data.table
  as.data.table() %>%
  
  # convert datatype to numeric
  .[, lapply(.SD, as.numeric)] %>%
  
  # save data.table as variable
  force() -> dt



# PART ONE ----------------------------------------------------------------

# sort each column independently, take sum of absolute difference
sum(abs(sort(dt[, V1]) - sort(dt[, V2])))



# PART TWO  ---------------------------------------------------------------

# multiply value in first column by frequency in second column, then take sum
sum(map_dbl(.x = dt[, V1], .f = ~ .x * sum(.x == dt[, V2])))



