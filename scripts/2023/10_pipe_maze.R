
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
tribble(
  ~move  , ~x, ~y,
  "left" ,  0, -1,
  "right",  0, +1,
  "down" , +1,  0,
  "up"   , -1,  0
) %>% 
  
  # convert to data.table
  as.data.table() %>% 
  
  # save data.table as variable
  force() -> dt_lkup


# get starting coordinates of SNAKE
start_x <- which(v_puz == "S") %/% len_col + 1
start_y <- which(v_puz == "S") %%  len_col


# set parameters
i <- 0
m <- c()
movement <- "left"
# movement <- "down"



# PART ONE ----------------------------------------------------------------

# run loop
repeat {
  
  # replace starting coordinates with movement direction
  start_x <- start_x + dt_lkup[move == movement, x]
  start_y <- start_y + dt_lkup[move == movement, y]
  
  # get new symbol
  xx <- mat_puz[start_x, start_y]
  
  # determine next movement based on current direction and new symbol
  movement <- if (xx %chin% c("|", "-")) {movement} else
        if (xx == "L" & movement == "down") {"right"} else
          if (xx == "L" & movement == "left") {"up"} else
            if (xx == "7" & movement == "right") {"down"} else
              if (xx == "7" & movement == "up") {"left"} else
                if (xx == "F" & movement == "up") {"right"} else
                  if (xx == "F" & movement == "left") {"down"} else
                    if (xx == "J" & movement == "right") {"up"} else
                      if (xx == "J" & movement == "down") {"left"} else
                        if (xx == ".") {"error"} else {"end"}
  
  # save coordinates to vector
  m <- c(m, start_x, start_y)
  
  # if end of cycle, then break infinite loop
  if (movement == "end") {break}
  
  # otherwise increment by 1
  i <- i + 1
  
}

# print result
print((i + 1) / 2)



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
  #   . count number of barriers encountered to the left (if even, then outside, if odd, then inside)
  .[
    , 
    `:=`(
      sum_pipe = cumsum(pipe_id == 1 & input == "|"),
      sum_LJ   = cumsum(pipe_id == 1 & input %chin% c("L", "J", "S")),
      sum_F7   = cumsum(pipe_id == 1 & input %chin% c("F", "7"))
    ), 
    by = .(row_id)
  ] %>% 
  
  # remove pipes
  .[pipe_id == 0] %>% 
  
  # count number of barriers encountered
  .[((sum_pipe + sum_LJ) %% 2 == 1) | ((sum_pipe + sum_F7) %% 2 == 1)] %>% 
  
  # count rows
  nrow()


