
# CAUTION: Takes really long to run!

# Read in data
# https://adventofcode.com/2020/day/11/input
day11 <- read.table("./data/input_day11.txt")
day11 <- data.frame(str_split_fixed(day11$V1, "", max(nchar(day11$V1))))

#### PART 1 ####
# Function that gives out the positions around a given position.
# Takes into account that it might be a position on the border of the
# dataframe that is not surrounded by 8 positions.
# Position is given as c(row, col)
get_neighbours <- function(position, df) {
  possible_rows <- c(position[1] - 1, position[1], position[1] + 1)
  possible_cols <- c(position[2] - 1, position[2], position[2] + 1)
  
  all_neighbours <- expand.grid(row = possible_rows, col = possible_cols)
  
  # exclude current position
  all_neighbours <- all_neighbours[-which(all_neighbours$row == position[1] & all_neighbours$col == position[2]), ]
  
  # exclude positions outside of the df
  are_too_small <- all_neighbours$row == 0 | all_neighbours$col == 0
  if (any(are_too_small)) {
    all_neighbours <- all_neighbours[-which(are_too_small), ] 
  }
  
  are_too_large <- all_neighbours$row > nrow(df) | all_neighbours$col > ncol(df)
  if (any(are_too_large)) {
    all_neighbours <- all_neighbours[-which(are_too_large), ] 
  }
  
  return(all_neighbours)
}

# Count number of occupied seats
count_occupied <- function(neighbours, df) {
  seats <- df[as.matrix(neighbours)]
  occupied <- sum(seats == "#")
  
  return(occupied)
}

# Go through dataframe, determine the status for each position and its neighbours
# and change the seat status if necessary.
one_iteration <- function(df) {
  new_df <- df
  
  for (i_row in 1:nrow(df)) {
    for (i_col in 1:ncol(df)) {
      temp_pos <- df[matrix(c(i_row, i_col), nrow = 1)]
      neighbours <- get_neighbours(c(i_row, i_col), df)
      seat_count <- count_occupied(neighbours, df)
      
      # If seat is empty and there are no occupied seats, occupy seat
      if (temp_pos == "L" & seat_count == 0) new_df[matrix(c(i_row, i_col), nrow = 1)] <- "#"
      # If a seat is occupied and at least 4 neighbours are occupied, vacate seat
      if (temp_pos == "#" & seat_count >= 4) new_df[matrix(c(i_row, i_col), nrow = 1)] <- "L"
    }
  }
  
  return(new_df)
}

# As long as the previous state and the new state are not equal, continue
# to iterate.
previous_state <- day11
new_state <- one_iteration(day11)
iteration_count <- 1

while (!all(all.equal(previous_state, new_state) == TRUE)) {
  previous_state <- new_state
  new_state <- one_iteration(new_state)
  iteration_count <- iteration_count + 1
  print(paste("completed iteration", iteration_count))
}

# Count occupied seats
sum(new_state == "#")
# 2346

#### PART 2 ####
# I need to exchange the neighbours function with a function that returns all seats people
# can see.

# Travel in each direction until you hit the first seat.
# Trajectories:
# left c(-1, 0)
# right c(1, 0)
# up c(0, -1)
# down c(0, 1)
# up left c(-1, -1)
# up right c(1, -1)
# down left c(-1, 1)
# down right c(1, 1)
get_visible_neighbours <- function(position, df) {
  visible_neighbours <- vector(mode = "character")
  
  trajectories <- expand.grid(row = c(0, 1, -1),
                              col = c(0, 1, -1))
  # Exclude no motion
  trajectories <- trajectories[-1, ]
  
  # Go through each trajectory and identify the first seat (!= .)
  for (i_trajectory in 1:nrow(trajectories)) {
    seat_unidentified <- TRUE
    
    temp_pos <- position
    temp_trajectoy <- trajectories[i_trajectory, ]
    
    while (seat_unidentified) {
      temp_pos <- temp_pos + temp_trajectoy
      
      # If position is of the dataframe, go to next trajectory  
      if (temp_pos[1] > nrow(df) | temp_pos[1] <= 0) break
      if (temp_pos[2] > ncol(df) | temp_pos[2] <= 0) break
      
      # If position is not a seat, go to next trajectory
      if (df[as.matrix(temp_pos, nrow = 1)] != ".") {
        visible_neighbours <- c(visible_neighbours, df[as.matrix(temp_pos, nrow = 1)])
        break
      }
    }
  }
  
  return(visible_neighbours)
}

# Updated iteration function
one_iteration2 <- function(df) {
  new_df <- df
  
  for (i_row in 1:nrow(df)) {
    for (i_col in 1:ncol(df)) {
      temp_pos <- df[matrix(c(i_row, i_col), nrow = 1)]
      neighbours <- get_visible_neighbours(c(i_row, i_col), df)
      seat_count <- sum(neighbours == "#")
      
      # If seat is empty and there are no occupied seats, occupy seat
      if (temp_pos == "L" & seat_count == 0) new_df[matrix(c(i_row, i_col), nrow = 1)] <- "#"
      # If a seat is occupied and at least 4 neighbours are occupied, vacate seat
      if (temp_pos == "#" & seat_count >= 5) new_df[matrix(c(i_row, i_col), nrow = 1)] <- "L"
    }
  }
  
  return(new_df)
}

# As long as the previous state and the new state are not equal, continue
# to iterate.
previous_state <- day11
new_state <- one_iteration2(day11)
iteration_count <- 1

while (!all(all.equal(previous_state, new_state) == TRUE)) {
  previous_state <- new_state
  new_state <- one_iteration2(new_state)
  iteration_count <- iteration_count + 1
  print(paste("completed iteration", iteration_count))
}

# Count occupied seats
sum(new_state == "#")
# 2111
