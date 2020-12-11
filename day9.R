
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/9/input
day9 <- readLines("./data/input_day9.txt")
day9 <- as.numeric(day9)

#### PART 1 ####
# Takes a vector and calculates all possible sums of the elements (drawing without replacement)
all_sums <- function(x, k) {
  all_combs <- combn(x, k)
  sums <- unique(colSums(all_combs))
  return(sums)
}

range <- 25

for (i_pos in seq_along(day9)) {
  i_window <- i_pos:(i_pos + (range - 1))
  compare <- i_pos + range
  
  sums <- all_sums(day9[i_window], 2)
  
  if (day9[compare] %in% sums) {
    next
  } else {
    stop(paste(day9[compare], "is not the sum of the", range, "previous numbers."))
  }
}

# 31161678 is not the sum of the 25 previous numbers.

#### PART 2 ####
# Go through all the numbers and add them one by one. As soon as the sum is above 31161678,
# move to the next position and repeat the process.
for (i_pos in seq_along(day9)) {
  temp_start <- i_pos
  temp_sum <- day9[i_pos]
  temp_stop <- i_pos
  
  while(temp_sum < 31161678) {
    temp_stop <- temp_stop + 1
    temp_sum <- sum(day9[temp_start:temp_stop])
    temp_min <- min(day9[temp_start:temp_stop])
    temp_max <- max(day9[temp_start:temp_stop])
    
    if (temp_sum == 31161678) {
      stop(paste0("The smallest number is ", temp_min, ", the largest number is ", temp_max))
    }
  }
  
}

# The smallest number is 1212280, the largest number is 4241588
1212280 + 4241588
# 5453868

