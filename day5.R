
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/5/input
day5 <- readLines("./data/input_day5.txt")

# A number of boarding passes, where letters indicate the seat number. See 
# https://adventofcode.com/2020/day/5 for the logic behind it.

#### PART 1 ####
# GOAL: What is the highest seat ID?
# So we'll write some functions that identify the seat, given the code
nrows <- 0:127
ncols <- 0:7

# Validate example
code <- "FBFBBFFRLR"

# Identify row
get_row <- function(seat_code, nrows) {
  split_code <- unlist(str_split(seat_code, ""))
  
  # Iterate through the first 7 letters to find the row
  for (i_char in 1:7) {
    # Split in half (ntile), and keep the lower half. Else, keep the upper half
    if (split_code[i_char] == "F") {
      nrows <- nrows[ntile(nrows, 2) == 1]
    } else {
      nrows <- nrows[ntile(nrows, 2) == 2]
    }
  } 
  
  return(nrows)
}

seat_row <- (get_row(code, nrows))
# 44

# Identify seat
get_col <- function(seat_code, ncols) {
  split_code <- unlist(str_split(seat_code, ""))
  # get the last 3 letters
  split_code <- split_code[(length(split_code) - 2):length(split_code)]
  
  # Iterate through the first 7 letters to find the row
  for (i_char in seq_along(split_code)) {
    
    # Split in half (ntile), and keep the lower half. Else, keep the upper half
    if (split_code[i_char] == "L") {
      ncols <- ncols[ntile(ncols, 2) == 1]
    } else {
      ncols <- ncols[ntile(ncols, 2) == 2]
    }
  } 
  
  return(ncols)
}

(seat_col <- get_col(code, ncols))
# 5

calculate_seat_id <- function(row, col) {
  id <- row * 8 + col
  return(id)
}

calculate_seat_id(seat_row, seat_col)
# 357

# All in one function
get_seat_id <- function(code, ncols, nrows) {
  seat_row <- get_row(code, nrows)
  seat_col <- get_col(code, ncols)
  id <- calculate_seat_id(seat_row, seat_col)
  
  return(id)
}

get_seat_id(code, ncols, nrows)
# 357

# Use this function to get the seat IDs for all codes
seat_ids <- vector(length = length(day5))

for (i_code in seq_along(day5)) {
  seat_ids[i_code] <- get_seat_id(day5[i_code], ncols, nrows)
}

# Highest seat id
max(seat_ids)
# 955

#### PART 2 ####
# My seat is the missing seat. Which seats are missing?

# Calculate the full set of possible ids:
possible_ids <- vector(mode = "numeric")

for (i_row in nrows) {
  for (j_col in ncols) {
    possible_ids <- c(possible_ids, calculate_seat_id(i_row, j_col))
  }
}

# Missing ids
leftover_ids <- possible_ids[!possible_ids %in% seat_ids]

# Problem: Seats in the very front/back are missing as well. They don't exist.
# We leave these out.
# "the seats with IDs +1 and -1 from yours will be in your list."

# I can see which one it is with the plain eye, but I want to have a function that identifies the
# entry where the increase between seat ids is != 1
diff_idx <- diff(leftover_ids)

# The second element were diff_idx != 1 should identify the seat id
leftover_ids[diff_idx != 1][2]
# 569
