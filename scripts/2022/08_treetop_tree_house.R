
# INTRO -------------------------------------------------------------------

# Title:  08 Treetop Tree House
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202208.txt"))



# DATA PREP ---------------------------------------------------------------

# split string into separate digits and convert to matrix
mat_puz <- str_split(string = puzzle, pattern = "", simplify = TRUE)

# get rows and columns for the matrix
nrowx <- nrow(mat_puz)
ncolx <- ncol(mat_puz)

# convert from character values to numeric
mat_puz <- matrix(as.double(mat_puz), nrow = nrowx, ncol = ncolx, byrow = FALSE)



# PART ONE ----------------------------------------------------------------

# combine lists
c(
  list(
    # EAST
    map(.x = 1:nrowx, .f = ~ mat_puz[.x,] > cummax(c(-1, mat_puz[.x, 1:(ncolx-1)]))),
    # WEST
    map(.x = 1:nrowx, .f = ~ rev(rev(mat_puz[.x,]) > cummax(c(-1, rev(mat_puz[.x, 2:ncolx])))))
  ) %>% 
    map(.f = flatten_lgl) %>% 
    map(.f = ~ matrix(data = .x, nrow = nrowx, ncol = ncolx, byrow = TRUE)),
  
  list(
    # SOUTH
    map(.x = 1:ncolx, .f = ~ mat_puz[,.x] > cummax(c(-1, mat_puz[1:(nrowx-1), .x]))),
    # NORTH
    map(.x = 1:ncolx, .f = ~ rev(rev(mat_puz[,.x]) > cummax(c(-1, rev(mat_puz[2:nrowx, .x])))))
  ) %>% 
    map(.f = flatten_lgl) %>% 
    map(.f = ~ matrix(data = .x, nrow = nrowx, ncol = ncolx, byrow = FALSE))
) %>% 
  
  # take four matrix results and add together
  reduce(.f = `+`) %>% 
  
  # convert to vector
  as.vector() %>% 
  
  # identify values greater than 0
  {. > 0} %>%
  
  # add values
  sum()



# PART TWO ----------------------------------------------------------------



