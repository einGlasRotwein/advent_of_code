
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/2/input
day2 <- read.table("./data/input_day2.txt")
names(day2) <- c("limits", "letter", "password")

#### PART 1 ####
# GOAL: Identify the passwords that are valid, i.e. where a given letter occurs at least a min, but
# no more than a max number of times.

# Separate first column into min/max occurrence
day2 <- day2 %>% 
  separate(limits, into = c("min", "max"), sep = "-")

# Clean up letter
day2 <- day2 %>% 
  mutate(letter = str_remove(letter, ":"))

# Count number of occurrences of the target letter
day2 <- day2 %>% 
  mutate(occurrence = str_count(password, letter))

# Filter out the passwords where the target letter occurs within the min and max
valid_pws <- day2 %>% 
  rowwise() %>% 
  filter(occurrence %in% min:max)

# Number of valid passwords
nrow(valid_pws)
# 524

#### PART 2 ####
# Goal: min and max are actually the positions where the target letter might appear.
# Valid passwords are where the target letter appears on one (and only one) of the target
# positions.
day2 <- day2 %>% 
  rename(pos1 = min,
         pos2 = max)

# Positions of the target letter
day2$valid <- NA

for (i in 1:nrow(day2)) {
  temp_positions <- which(strsplit(day2$password[i], "")[[1]] == day2$letter[i])
  
  if (xor(day2$pos1[i] %in% temp_positions,
          day2$pos2[i] %in% temp_positions)) {
    day2$valid[i] <- TRUE
  } else {
    day2$valid[i] <- FALSE
  }
}

# How many are valid?
sum(day2$valid)
