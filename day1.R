
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/1/input
day1 <- readLines("./data/input_day1.txt", strin)
day1 <- as.numeric(day1)

#### PART 1 ####
# GOAL: Find the 2 values that sum up to 2020
# Combine all values (without replacement, i.e. number 1 is not combined with number 1)
all_combs <- combn(day1, 2)

# Which of the two combinations is 2020?
idx <- which(colSums(all_combs) == 2020)

# Verify that the sum is 2020
sum(all_combs[, idx])

# Multiply the result
prod(all_combs[, idx])
# 63616

#### PART 2 ####
all_combs <- combn(day1, 3)
idx <- which(colSums(all_combs) == 2020)
sum(all_combs[, idx])
prod(all_combs[, idx])
# 67877784
