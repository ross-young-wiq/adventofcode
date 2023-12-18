
# INTRO -------------------------------------------------------------------

# Title:  14 Parabolic Reflector Dish
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202314.txt"))



# DATA PREP ---------------------------------------------------------------

# create matrix from puzzle input
mat_puz <- str_split(string = puzzle, pattern = "", simplify = TRUE)

# get dimensions of puzzle matrix
dim_mat <- dim(mat_puz)

# get vector
v_puz    <- as.vector(mat_puz)

# get column and row IDs within vector
col_id  <- ceiling(seq_along(v_puz) / dim_mat[1])
row_id  <- rep(seq(dim_mat[1]), dim_mat[2])

# identify hash (with various transpositions) 
# TODO: this is a bit convoluted, but works later. need to simplify
v_hash     <- cumsum(v_puz == "#")
v_hash_t   <- mat_puz %>% t() %>% as.vector() %>% {cumsum(. == "#")}
v_hash_tt  <- v_hash_t %>% matrix(nrow = dim_mat[1]) %>% t() %>% as.vector()
v_hash_ttt <- v_hash   %>% matrix(nrow = dim_mat[1]) %>% t() %>% as.vector()



# PART ONE ----------------------------------------------------------------

# start with puzzle vector
v_puz %>% 
  
  # convert to factors (to be able to sort in order required)
  factor(levels = c("#", "O", ".")) %>%
  
  # sort values
  {.[order(col_id, v_hash, rank(.))]} %>% 
  
  # count number of "O" values and multiply by load value per row
  {(. == "O") * rev(row_id)} %>% 
  
  # sum values to calculate total load
  sum()



# PART TWO ----------------------------------------------------------------

# create function to complete a full cycle (north, west, south, east)
my_fun <- function(v, ...) {
  
  v %>% 
    factor(levels = c("#", "O", ".")) %>% 
    {.[order(col_id, v_hash, rank(.))]} %>% 
    {.[order(row_id, v_hash_tt, rank(.))]} %>% 
    
    factor(levels = c("#", ".", "O")) %>% 
    {.[order(row_id, v_hash_ttt, rank(.))]} %>% 
    {.[order(row_id, v_hash_tt, rank(.))]} %>%
    
    matrix(nrow = dim_mat[1], byrow = TRUE) %>% as.vector()
  
}


# run 250 cycles (storing result from each cycle)
accumulate(
  .x = 1:250,
  .f = my_fun,
  .init = v_puz
) %>% 
  
  # convert each result into a single text string
  map_chr(.f = paste0, collapse = "") %>% 
  
  # save result 
  force() -> x


# identify first repeat
first_repeat <- head(which(duplicated(x)), 1)

# get position of which cycle this value first appeared
start_of_loop  <- head(which(x == x[first_repeat]), 1)
length_of_loop <- unique(diff(which(x == x[first_repeat])))

# cycle starts at position 102 (but the first cycle is the initial matrix, which doesn't count)
# loop repeats after 9 cycles

# find where the final cycle appears in the loop
final_cycle_loop <- (1000000000 - (start_of_loop - 1))

# get string for last cycle
last_cycle <- x[start_of_loop + (final_cycle_loop %% length_of_loop)]

# start with value for last cycle
last_cycle %>% 
  
  # split into separate strings and convert to vector
  str_split(pattern = "", simplify = TRUE) %>% as.vector() %>% 
  
  # count number of "O" values and multiply by load value per row
  {(. == "O") * rev(row_id)} %>% 
  
  # sum values to calculate total load
  sum()


