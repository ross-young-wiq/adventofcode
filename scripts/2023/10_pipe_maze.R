
# INTRO -------------------------------------------------------------------

# Title:  10 Pipe Maze
# Author: Ross Young



# SETUP -------------------------------------------------------------------

# load packages
library(tidyverse)
library(data.table)

# load data
puzzle <- read_lines(file = paste0("data/202310.txt"))



# DATA PREP ---------------------------------------------------------------

# start with puzzle
puzzle %>% 
  
  # split into separate components
  str_split(pattern = "") %>% 
  
  # convert to vector
  unlist() %>% 
  
  # save vector as variable
  force() -> v_puz


# get number of rows and columns from the input set
len_row <- length(puzzle)
len_col <- str_length(puzzle[1])


# start with the puzzle vector
v_puz %>% 
  
  # convert to matrix
  matrix(data = ., nrow = len_row, byrow = TRUE) %>% 
  
  # save matrix as variable
  force() -> mat_puz


# create lookup table for directions
data.table(
  move = c("L", "R", "D", "U"),
  x    = c(  0,   0,  +1,  -1),
  y    = c( -1,  +1,   0,   0)
) %>% 
  
  # save data.table as variable
  force() -> dt_dir

# get starting coordinates of SNAKE
start_x <- which(v_puz == "S") %/% len_col + 1
start_y <- which(v_puz == "S") %%  len_col

# set parameters
i <- 0
m <- c()
movement <- "L" # since the snake can go two ways, we explicitly set the starting direction



# PART ONE ----------------------------------------------------------------

# run loop (this takes around 20 seconds to run)
repeat {
  
  # replace starting coordinates with movement direction
  start_x <- start_x + dt_dir[move == movement, x]
  start_y <- start_y + dt_dir[move == movement, y]
  
  # get new symbol
  new_sym <- mat_puz[start_x, start_y]
  
  # determine next movement based on current direction and new symbol
  movement <- if (new_sym %chin% c("|", "-")) {movement} else
  if (new_sym == "L" & movement == "D") {"R"} else
  if (new_sym == "L" & movement == "L") {"U"} else
  if (new_sym == "7" & movement == "R") {"D"} else
  if (new_sym == "7" & movement == "U") {"L"} else
  if (new_sym == "F" & movement == "U") {"R"} else
  if (new_sym == "F" & movement == "L") {"D"} else
  if (new_sym == "J" & movement == "R") {"U"} else
  if (new_sym == "J" & movement == "D") {"L"} else
  if (new_sym == ".") {"error"} else {"end"}
  
  # save coordinates to vector (TODO: fix the 'growing the vector' approach - inefficient)
  m <- c(m, start_x, start_y)
  
  # increment by 1
  i <- i + 1
  
  # if end of cycle, then break infinite loop
  if (movement == "end") {break}
  
}

# print result
print(i / 2)



# PART TWO ----------------------------------------------------------------

# create matrix with pipe values
pipe_pts <- matrix(data = m, ncol = 2, byrow = TRUE)

# start with puzzle vector
v_puz %>% 
  
  # convert to data.table
  data.table(input = .) %>% 
  
  # add cell id
  .[, cell_id := .I] %>% 
  
  # add row id
  .[, row_id := ceiling(cell_id / len_col)] %>% 
  
  # add column id
  .[, col_id := rowid(row_id)] %>% 
  
  # left join to identify pipe points
  merge.data.table(
    y = data.table(
          row_id = pipe_pts[,1], 
          col_id = pipe_pts[,2]
        ) %>% 
      .[, pipe_id := 1],
    by = c("row_id", "col_id"),
    all.x = TRUE,
    all.y = FALSE
  ) %>% 
  
  # replace missing values with 0
  .[, pipe_id := fcoalesce(pipe_id, 0)] %>% 
  
  # NOTE: an interesting observation:
  #   . count the number of times you 'cross the path' between the left edge of grid and a specific point.
  #   . if even, then you are outside
  #   . if odd, then you are inside
  
  #   . to be inside the polygon, it's easier to think about your point being slightly above or slightly below the centre of the cell.
  #   . if you're slightly above the centre of the cell, then moving left you will pass through "|" an "L" and "J" (also "S" because replace with "J")
  #   . ignore "F" and "7" because your line will pass ABOVE these paths (because your line is slightly above the centre of the cell).
  .[, sum_crossings := cumsum(pipe_id == 1 & input %chin% c("|", "L", "J", "S")), by = .(row_id)] %>% 
  
  # remove rows with pipes
  .[pipe_id == 0] %>% 
  
  # keep rows where the number of 'paths crossed' is odd
  .[sum_crossings %% 2 == 1] %>%
  
  # count rows
  nrow()

