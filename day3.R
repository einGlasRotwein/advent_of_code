
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/3/input
day3 <- read.table("./data/input_day3.txt", comment.char = "")

#### PART 1 ####
# The # are trees, the . are free spaces.
# If I ride down the forest with my sledge, following the slope 3 to the right, 1 down
# (I start in the top left corner and the rows repeat when I reach the right end of the "map") -
# how many trees do I hit?

# Separate into columns for convenience
# Found on:
# https://stackoverflow.com/questions/52662277/separate-string-to-columns-by-each-character
day3 <- data.frame(str_split_fixed(day3$V1, "", max(nchar(day3$V1))))

move <- c(1, 3) # down, right
temp_pos <- c(1, 1) # row, col

trees_hit <- 0

while (temp_pos[1] != nrow(day3)) {
  temp_pos <- temp_pos + move
  
  # If end of map (last column) is reached, start from the beginning
  temp_pos[2] <- temp_pos[2] %% 31
  if (temp_pos[2] == 0) temp_pos[2] <- 31
  
  temp_loc <- day3[temp_pos[1], temp_pos[2]]
  if (temp_loc == "#") trees_hit <- trees_hit + 1
}

# Number of trees we hit
trees_hit
# 289

#### PART 2 ####
# How many trees do I hit with the following slopes:

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.

# Put the stuff above into a function
hit_those_trees <- function(map, start, slope) {
  temp_pos <- start
  
  trees_hit <- 0
  
  while (temp_pos[1] != nrow(day3)) {
    temp_pos <- temp_pos + slope
    
    # If end of map (last column) is reached, start from the beginning
    temp_pos[2] <- temp_pos[2] %% 31
    if (temp_pos[2] == 0) temp_pos[2] <- 31
    
    temp_loc <- map[temp_pos[1], temp_pos[2]]
    if (temp_loc == "#") trees_hit <- trees_hit + 1
  }
  
  # Number of trees we hit
  return(trees_hit)
}

# Try slopes
# Right 1, down 1.
(slope1 <- hit_those_trees(day3, c(1, 1), c(1, 1)))

# Right 3, down 1. (This is the slope you already checked.)
(slope2 <- hit_those_trees(day3, c(1, 1), c(1, 3)))

# Right 5, down 1.
(slope3 <- hit_those_trees(day3, c(1, 1), c(1, 5)))

# Right 7, down 1.
(slope4 <- hit_those_trees(day3, c(1, 1), c(1, 7)))

# Right 1, down 2.
(slope5 <- hit_those_trees(day3, c(1, 1), c(2, 1)))

# Multiply all:
slope1 * slope2 * slope3 * slope4 * slope5
# 5522401584
