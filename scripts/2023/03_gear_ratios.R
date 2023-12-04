
# INTRO -------------------------------------------------------------------

# Title:  02 Cube Conundrum
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# parameters
advent_day <- "03"

# load data
puzzle <- read_lines(file = paste0("data/2023", advent_day, ".txt"))



# CLEANING ----------------------------------------------------------------

# start with puzzle input
puzzle %>%
  
  # run loop over input to locate (1) numbers and (2) symbols (excluding .)
  map2(
    .x = list(.),
    .y = c("\\d+", "[^\\d\\.]"),
    .f = ~ .x %>% 
      str_locate_all(pattern = .y) %>%   # find start and stop values for each pattern match
      set_names(seq_along(.x)) %>%       # allocate row id to each list result
      map(as.data.table) %>%             # convert each list element to a data.table
      rbindlist(idcol = "row_id") %>%    # bind rows, allocating row_id as a new column
      .[, row_id := as.integer(row_id)]  # convert row_id to integer
  ) %>% 
  
  # assign names to each list element (1) numbesr and (2) symbols
  set_names("nbr", "sym") %>% 
  
  # save variable as data.table
  force() -> lst_input

# start with number data
lst_input$nbr %>% 
  
  # add row_length (confirming that all are 140 characters long)
  .[, row_length := unique(map_int(puzzle, str_length))] %>%
  
  # create adjusted columns
  .[
    , 
    `:=`(
      adj_start = pmax(1, start - 1), 
      adj_end   = pmin(row_length, end + 1),
      row_start = pmax(1, row_id - 1),
      row_end   = pmin(length(puzzle), row_id + 1)
    )
  ] %>% 
  
  # remove columns not required
  .[, c("row_id", "start", "end", "row_length") := NULL] %>% 
  
  # extract numerical value from grid
  .[, nbr := as.integer(unlist(str_extract_all(puzzle, "\\d+")))] %>% 
  
  # save variable as data.table
  force() -> dt_nbr


# create a cross-join with numerical data location and symbol location
merge.data.table(
  x = dt_nbr[, join_key := 1],
  y = lst_input$sym[, join_key := 1],
  by = c("join_key"),
  allow.cartesian = TRUE
) %>% 
  
  # add column to flag potential matches
  .[, filter_flag := (start >= adj_start) & (start <= adj_end) & (row_id >= row_start) & (row_id <= row_end)] %>% 
  
  # save variable as data.table
  force() -> dt_result



# PART ONE ----------------------------------------------------------------

# start with data.table
dt_result %>% 
  
  # count rows that meet the filter for each numerical value
  .[, .(N = sum(filter_flag, na.rm = TRUE)), by = .(nbr)] %>% 
  
  # remove numbers with no symbol matches
  .[N > 0] %>% 
  
  # combine the number with the count of matches, then sum
  .[, sum(nbr * N)]
  


# PART TWO ----------------------------------------------------------------

# start with data.table
dt_result %>% 
  
  # keep rows where filter flag is TRUE
  .[filter_flag == TRUE] %>% 
  
  # sum count of rows and the product of the number
  .[, .(n_count = .N, prod_nbr = prod(nbr)), by = .(row_id, start, end)] %>% 
  
  # filter to where the row count is exactly two
  .[n_count == 2] %>% 
  
  # get sum of prod_nbr
  .[, sum(prod_nbr)]