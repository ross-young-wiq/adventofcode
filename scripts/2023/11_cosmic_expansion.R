
# INTRO -------------------------------------------------------------------

# Title:  11 Cosmic Expansion
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202311.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # split into separate digits
  str_split(pattern = "", simplify = TRUE) %>% 
  
  # save matrix as variable
  force() -> mat_puz


# get row / col references where empty
empty_row <- which(apply(mat_puz, 1, function(x) {all(x == ".")}))
empty_col <- which(apply(mat_puz, 2, function(x) {all(x == ".")}))


# start with puzzle matrix
mat_puz %>% 
  
  # identify which cells contain planet ("#")
  {. == "#"} %>% 
  
  # convert to vector (column by column)
  which() %>% 
  
  # save as data.table
  data.table(cell = .) %>% 
  
  # get x and y coordinates (then give each row a unique ID 'z')
  .[
    , 
    `:=`(
      x = (cell - 1) %%  nrow(mat_puz) + 1,
      y = (cell - 1) %/% nrow(mat_puz) + 1,
      z = .I
    )
  ] %>% 
  
  # remove cell column
  .[, cell := NULL] %>% 
  
  # save data.table as variable
  force() -> dt


# create function to calculate path
my_fun <- function(e_size, e_row = empty_row, e_col = empty_col, dt0 = dt) {

  # rename columns
  dt0[, .(x1 = x, y1 = y, z1 = z)] %>% 
    
    # non equi join
    .[dt0[, .(x2 = x, y2 = y, z2 = z)], on = .(z1 > z2)] %>% 
    
    # ensure no coordinates missing
    .[!is.na(x1) & !is.na(x2)] %>% 
    
    # get number of times the path crosses an empty row and empty col
    .[
      ,
      `:=`(
        n_empty_row = map2_dbl(.x = x1, .y = x2, .f = ~ sum(pmin(.x, .y) <= e_row & pmax(.x, .y) >= e_row)),
        n_empty_col = map2_dbl(.x = y1, .y = y2, .f = ~ sum(pmin(.x, .y) <= e_col & pmax(.x, .y) >= e_col))
      )
    ] %>%
    
    # get length of path 
    # note: we need to takeaway the count of empty rows and columns because they are already counted once as part of the path
    .[, len_path := abs(x2 - x1) + abs(y2 - y1) + (e_size - 1) * (n_empty_row + n_empty_col)] %>% 
    
    # return sum of path lengths
    .[, sum(len_path)]
  
}


  
# PART ONE ----------------------------------------------------------------

my_fun(e_size = 2)


# PART TWO  ---------------------------------------------------------------

my_fun(e_size = 1E6)


