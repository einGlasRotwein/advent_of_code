
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/1/input
day1 <- read.table("./data/input_day1.txt")
day1 <- day1$V1

#### PART 1 ####
# GOAL
# Find the 2 values that sum up to 2020

# Are there any values that are too large anyways?
min(day1)
max(day1)

# Since the minimum is 13, we can exclude anything that is larger than 2007
sum(day1 > 2007) # ... which is only 3 entries, but whatever

day1 <- day1[day1 <= 2007]

# The majority of values is larger than 1500
qplot(day1, binwidth = 50)

# To sum up to 2020, the last digits need to sum up to 10. For each entry, I can reduce the set of
# possible matches by limiting the search set to the correct last digit.
# For this: Split number into digits.
# (Yes, this is not very elegant ant completely unnecessary, but I wanted to try this.)

day1 <- data.frame(raw = day1)

day1 <- day1 %>% 
  mutate(raw_char = as.character(raw)) %>% 
  mutate(raw_char = case_when(str_length(raw_char) == 4 ~ raw_char,
                              str_length(raw_char) == 3 ~ paste0("0", raw_char),
                              str_length(raw_char) == 2 ~ paste0("00", raw_char),
                              str_length(raw_char) == 1 ~ paste0("000", raw_char))) %>% 
  separate(raw_char, into = paste0("digit", 1:4), sep = 1:4, fill = "left")

# Furthermore, each number can only be paired with a number that is smaller than/equal 2020 - number

for (i in 1:nrow(day1)) {
  temp_num <- day1$raw[i]
  temp_last_digit <- day1$digit4[i]
  temp_valid_last_digit <- (10 - as.numeric(temp_last_digit)) %% 10
  
  temp_possible_matches <- day1 %>% 
    filter(raw <= 2020 - temp_num,
           digit4 == temp_valid_last_digit)
  
  if (nrow(temp_possible_matches > 0)) {
    temp_possible_matches <- temp_possible_matches$raw
    
    for (j in seq_along(temp_possible_matches)) {
      if (temp_num + temp_possible_matches[j] == 2020) {
        num1 <- temp_num
        num2 <- temp_possible_matches[j]
        stop(paste(temp_num, "+", temp_possible_matches[j], "=", temp_num + temp_possible_matches[j]))
      }
    }
  }
}

# Multiply the result
num1 * num2
# 63616

#### PART 2 ####
# Same, but this time, we need to find 3 numbers that sum up to 2020.
# So this time, I need to restore the whole data set.
rm(list = ls())
day1 <- read.table("./data/input_day1.txt")
day1 <- day1$V1

# This time, we pick one number, select a second one where 2020 - second number <= first number
# Add those. And repeat.
for (i in seq_along(day1)) {
  num1 <- day1[i]
  
  temp_pool_num2 <- day1[day1 <= (2020 - num1)]
  
  if (length(temp_pool_num2) > 0) {
    for (j in seq_along(temp_pool_num2)) {
      temp_sum <- num1 + temp_pool_num2[j]
      temp_pool_num3 <- temp_pool_num2[temp_pool_num2 <= (2020 - temp_sum)]
      
      if (length(temp_pool_num3) > 0) {
        for (k in seq_along(temp_pool_num3)) {
          num2 <- temp_pool_num2[j]
          temp_sum2 <- temp_sum + temp_pool_num3[k]
          
          if (temp_sum2 == 2020) {
            num3 <- temp_pool_num3[k]
            stop(paste(num1, "+", num2, "+", num3, "=", num1 + num2 + num3))
          }
        }
      }
    }
  }
}

# Multiply
num1 * num2 * num3
# 67877784
