
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/6/input
day6 <- readLines("./data/input_day6.txt")

# Find the number of unique questions answered with yes (as represented by letters) for each
# group. See https://adventofcode.com/2020/day/6 for the logic behind it.

#### PART 1 ####
# GOAL: How many unique letters per group?

# Again, the data format is a bit tricky for me, so I'll use a similar approach as in day 4.
# Original input as a column in a data frame
day6 <- data.frame(content = day6)

# Index of blank locations
day6 <- day6 %>% 
  mutate(blanks = content == "")

# Number blanks
day6$group_no <- NA
day6$group_no[day6$blanks] <- 1:sum(day6$blanks)

# Fill group ID (backwards)
day6 <- day6 %>% 
  fill(group_no, .direction = "up")

# Create one string per group
day6 <- day6 %>% 
  group_by(group_no) %>% 
  summarise(whole_content = paste(content, collapse = "")) %>% 
  mutate(whole_content = trimws(whole_content))

# Idea found on:
# https://stackoverflow.com/questions/31814548/function-that-extracts-each-unique-character-in-a-string
unique_chars <- function(x) unique(strsplit(x, "")[[1]])

# Reduce strings to unique characters
day6 <- day6 %>% 
  rowwise() %>% 
  mutate(unique_chars = paste0(unique_chars(whole_content), collapse = ""))

# Count unique characters
day6 <- day6 %>% 
  mutate(str_count = str_count(unique_chars))

# Sum:
sum(day6$str_count)

#### PART 2 ####
# Goal: Identify the questions to which EVERYONE within a group answered yes.

# The data format I had before doesn't work this time.

# So we'll start over:
day6 <- readLines("./data/input_day6.txt")

# day6 <- c("abc", "", "a", "b", "c", "", "ab", "ac", "", "a", "a", "a", "a", "", "b")

# Original input as a column in a data frame
day6 <- data.frame(content = day6)

# Index of blank locations
day6 <- day6 %>% 
  mutate(blanks = content == "")

# Number blanks
day6$group_no <- NA
day6$group_no[day6$blanks] <- 1:sum(day6$blanks)

# Fill group ID (backwards)
day6 <- day6 %>% 
  fill(group_no, .direction = "up")

# Eliminate blanks
day6 <- day6 %>% 
  filter(!blanks) %>% 
  select(-blanks)

# Give last group a number
day6$group_no[is.na(day6$group_no)] <- max(day6$group_no, na.rm = TRUE) + 1

# Number people
day6 <- day6 %>% 
  group_by(group_no) %>% 
  mutate(person = seq_along(group_no))

# Store the answers everyone has in common
everyone_answered <- list()

# Loop through groups and compare what all members have in common
for (i_group in 1:max(day6$group_no)) {
  # Use the first person's answers as comparison
  temp_letters <- day6$content[day6$group_no == i_group & day6$person == 1]
  temp_letters <- strsplit(temp_letters, "")[[1]]
  
  # If there is only one person
  if (length(day6$content[day6$group_no == i_group & day6$person == 2]) == 0) {
    # store the answers everyone has (i.e. the only person on the group)
    everyone_answered[[i_group]] <- temp_letters
    # ... and proceed to next group
    next
  }
  
  # ... otherwise, go through the other persons and find what everyone has in common
  for (j_person in 2:max(day6$person[day6$group_no == i_group])) {
    next_letters <- day6$content[day6$group_no == i_group & day6$person == j_person]
    next_letters <- strsplit(next_letters, "")[[1]]
    
    # The overlap between the current person's letters (temp letters) and the next person's
    # letters becomes the new temp_letters. So in the next round, temp_letters represents the
    # overlap between the first two people and can be compared with the next person.
    
    if (length(temp_letters) == 0) temp_letters <- ""
    
    temp_letters <- temp_letters[temp_letters %in% next_letters]
  }
  
  everyone_answered[[i_group]] <- temp_letters
}

# Sum of lengths of every list entry
lapply(everyone_answered, length) %>% 
  unlist() %>% 
  sum()
# 3394
