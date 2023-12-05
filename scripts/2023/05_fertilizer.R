
# INTRO -------------------------------------------------------------------

# Title:  05 If You Give A Seed A Fertilizer
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202305.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle input
puzzle %>% 
  
  # get first row (contains info on seeds)
  head(1) %>% 
  
  # extract seed numbers
  str_extract_all(pattern = "\\d+", simplify = TRUE) %>% 
  
  # convert to numeric
  as.double() %>% 
  
  # save vector as variable
  force() -> v_seeds


# start with puzzle input
puzzle %>% 
  
  # remove first row (already handled seeds)
  tail(-1) %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # create new column with mapping relationship
  .[, c("cat_from", "cat_to") := tstrsplit(x = input, "-to-")] %>% 
  
  # clean-up columns
  .[, cat_from := fifelse(is.na(cat_to), NA_character_, cat_from)] %>%
  .[, cat_to := NULL] %>% 
  
  # fill values down
  tidyr::fill(cat_from, .direction = "down") %>% 
  
  # remove rows without number data
  .[str_detect(string = input, pattern = "\\d")] %>% 
  
  # split input column into three components
  .[, c("dest_start", "source_start", "range_len") := tstrsplit(x = input, " ")] %>% 
  
  # convert to double
  .[, c("dest_start", "source_start", "range_len") := lapply(.SD, as.double), .SDcols = c("dest_start", "source_start", "range_len")] %>% 

  # create dest_end and source_end
  .[, `:=`(dest_end = dest_start + range_len - 1, source_end = source_start + range_len - 1)] %>% 
  
  # sort data
  .[order(cat_from, source_start)] %>% 
  
  # retain required columns
  .[, .(cat_from, source_start, source_end, dest_start, dest_end)] %>% 
  
  # save as data.table
  force() -> dt
 

# create vector with order of mapped pairings 
v_cat_order <- c("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity")

# global min
v_global_min <- dt[, min(source_start)]
v_global_max <- dt[, max(source_end)]


# include rows at the start and end (and in-between) to ensure global min and max are covered
# bind rows
rbindlist(
  l = list(
    
    # (1) row that goes before
    map(
      .x = v_cat_order,
      .f = function(z) {
        
        dt0 <- dt[cat_from == z][1]
        
        dt0_min <- dt0$source_start
        
        if (dt0_min > v_global_min) {
          data.table(
            cat_from     = z,
            source_start = v_global_min,
            source_end   = dt0_min - 1,
            dest_start   = v_global_min,
            dest_end     = dt0_min - 1
          )
        } else {dt0[FALSE]} # return empty data.table if not required
        
      }
    ) %>% 
      
      rbindlist(),
    
    # (2) rows that goes after
    map(
      .x = v_cat_order,
      .f = function(z) {
        
        dt0 <- dt[cat_from == z][.N]
        
        dt0_max <- dt0$source_end
        
        if (dt0_max < v_global_max) {
          data.table(
            cat_from     = z,
            source_start = dt0_max + 1,
            source_end   = v_global_max,
            dest_start   = dt0_max + 1,
            dest_end     = v_global_max
          )
        } else {dt0[FALSE]} # return empty data.table if not required
        
      }
    ) %>% 
      
      rbindlist(),
    
    # (3) rows that go in-between
    dt %>% 
      
      # create copy (to avoid overwrite)
      data.table::copy() %>% 
      
      # identify gaps between rows
      .[, lag_x := lead(source_start) - source_end, by = .(cat_from)] %>% 
      
      # create new columns for loading
      .[, `:=`(newstart = source_end + 1, newend = lead(source_start) - 1)] %>% 
      
      # remove rows that do not have gaps
      .[!is.na(lag_x)] %>% 
      .[lag_x > 1] %>% 
      
      # keep required columns
      .[, .(cat_from, source_start = newstart, source_end = newend, dest_start = newstart, dest_end = newend)],
    
    # (4) original data.table
    dt
    
  )
) %>% 
  
  # reorder
  .[order(cat_from, source_start)] %>% 
  
  # save as new data.table
  force() -> dtx



# PART ONE ----------------------------------------------------------------

# create function to extract require destination value based on inputs
my_fun <- function(nbr_from0, cat_from0, dt0 = dtx) {
  x0 <- dt0[cat_from == cat_from0 & source_start <= nbr_from0 & source_end >= nbr_from0]
  if (nrow(x0) == 0) {nbr_from0} else {x0[, dest_start + (nbr_from0 - source_start)]}
}

# brute force approach with a for loop
map_dbl(
  .x = v_seeds,
  .f = function(x) {
    
    for (i in v_cat_order) {x <- my_fun(nbr_from = x, cat_from = i)}
    return(x)
    
  }
) %>% 
  
  # lowest number
  min()



# PART TWO ----------------------------------------------------------------

# unable to proceed with 'part 1' solution --> too many loops for brute force;
# instead return a table with start and stop values

# get odd number sequence
v_seq <- seq(from = 1, to = length(v_seeds) - 1, by = 2)

# create data.table to store seed information
data.table(
  seed_start = v_seeds[v_seq],
  seed_len   = v_seeds[v_seq + 1]
) %>% 
  
  # add column for seed end
  .[, seed_end := seed_start + seed_len] %>% 
  
  # remove length column
  .[, seed_len := NULL] %>% 
  
  # save data.table as variable
  force() -> dt_seed


my_fun2 <- function(nbr_from0, nbr_to0, cat_from0, dt0 = dtx) {
  
  # start with data.table
  dt0 %>% 
    
    # filter to the matching rows
    .[
      cat_from == cat_from0 & 
        (
          (source_start <= nbr_from0 & source_end >= nbr_from0) | 
          (source_start <= nbr_to0   & source_end >= nbr_to0)
        )
    ] %>% 
    
    # find start and end numbers that line up
    .[
      ,
      `:=`(
        seed_source_start = fifelse(source_start >= nbr_from0, source_start, nbr_from0),
        seed_source_end   = fifelse(source_end   <= nbr_to0,   source_end,   nbr_to0)
      )
    ] %>% 
    
    # get the amount of digits required to scale up
    .[
      , 
      `:=`(
        seed_start = dest_start + (seed_source_start - source_start),
        seed_end   = dest_end   + (seed_source_end   - source_end)
      )
    ] %>% 
    
    .[, .(seed_start, seed_end)] %>% 
    
    .[]
}

# brute force approach, but one loop per seed start and end
map_dbl(
  .x = 1:nrow(dt_seed),
  .f = function(x) {
    
    x0 <- dt_seed[x]
    
    for (i in v_cat_order) {
      x0 <- map2_df(.x = x0$seed_start, .y = x0$seed_end, .f = ~ my_fun2(nbr_from0 = .x, nbr_to0 = .y, cat_from0 = i))
    }
    return(x0[, min(seed_start)])
    
  } 
) %>% 
  
  # lowest number
  min()
