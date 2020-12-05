
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/4/input
day4 <- readLines("./data/input_day4.txt")

#### PART 1 ####
# Goal: Check passports for required fields.

# Problem: Each row contains a different amount of information (e.g. only height, or hair colour AND eye colour ...)
# There's probably a better way of reading this in. But here's what I do:

# Original input as a column in a data frame
day4 <- data.frame(content = day4)

# Index of blank locations
day4 <- day4 %>% 
  mutate(blanks = content == "")

# Number blanks
day4$passport_no <- NA
day4$passport_no[day4$blanks] <- 1:sum(day4$blanks)

# Fill passport ID (backwards)
day4 <- day4 %>% 
  fill(passport_no, .direction = "up")

# Create one string per passport
day4 <- day4 %>% 
  group_by(passport_no) %>% 
  summarise(whole_content = paste(content, collapse = " ")) %>% 
  mutate(whole_content = trimws(whole_content))

# Which passports have the required fields?
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)

# Cid is optional, so we leave that out.
required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

# Which passport strings contain all the required fields?
day4 <- day4 %>% 
  mutate(passport_valid = 
           str_detect(whole_content, required_fields[1]) &
           str_detect(whole_content, required_fields[2]) &
           str_detect(whole_content, required_fields[3]) &
           str_detect(whole_content, required_fields[4]) &
           str_detect(whole_content, required_fields[5]) &
           str_detect(whole_content, required_fields[6]) &
           str_detect(whole_content, required_fields[7]))

# How many are valid?
sum(day4$passport_valid)
# 230

#### PART 2 ####
# Further validation, this time:
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# Only use passports that have all fields:
day4 <- day4 %>% 
  filter(passport_valid)

# Empty list that will later contain one named vector for every row in day4
passport_list <- list()

for (i_row in 1:nrow(day4)) {
  # Each feature as one element in a vector
  temp_passport <- unlist(strsplit(day4$whole_content[i_row], " "))
  # Everything before the : is the name, everything afterwards is the value
  temp_names <- str_extract_all(temp_passport, ".*?:", simplify = TRUE)[ , 1]
  temp_names <- gsub(":", "", temp_names)
  temp_values <- str_extract_all(temp_passport, ":.*?$", simplify = TRUE)[ , 1]
  temp_values <- gsub(":", "", temp_values)
  temp_passport <- temp_values
  names(temp_passport) <- temp_names
  
  passport_list[[i_row]] <- temp_passport
}

# We want this in a data frame. So first, we extract the required fields from each list.
# This basically leaves us with a list of the same vectors as before, but this time, they are
# ordered (required fields)
passport_list <- lapply(passport_list, function(x) {
  x[required_fields]
})

# As dataframe
passport_df <- as.data.frame(do.call(rbind, passport_list))

# Now - validation :-)
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
passport_df <- passport_df %>% 
  mutate(byr = as.numeric(byr)) %>%
  filter(byr %in% 1920:2002)

# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
passport_df <- passport_df %>% 
  mutate(iyr = as.numeric(iyr)) %>%
  filter(iyr %in% 2010:2020)

# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
passport_df <- passport_df %>% 
  mutate(eyr = as.numeric(eyr)) %>%
  filter(eyr %in% 2020:2030)

# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
#   If in, the number must be at least 59 and at most 76.

# Split height into value and unit (cm or in)
passport_df <- passport_df %>% 
  mutate(hgt_unit = str_extract(hgt, "[a-z]+")) %>% 
  # filter out the ones which don't have a unit
  filter(!is.na(hgt_unit)) %>% 
  # height as numeric
  mutate(hgt = as.numeric(str_extract(hgt, "[0-9]+")))

# Filter out values within the wrong range
passport_df <- passport_df %>% 
  filter((hgt_unit == "cm" & hgt %in% 150:193) |
         (hgt_unit == "in" & hgt %in% 59:76))

# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.

# Filter out all hair colours that don't start with a #
passport_df <- passport_df %>% 
  filter(grepl("^#", hcl))

# All hair colours are 6 (7 including the #) characters long
all(str_length(passport_df$hcl) == 7)

# Do the strings  only consist of #, a-f and 0-9?
all(grepl('^[a-f|0-9|#]+$', passport_df$hcl) == TRUE)
# All good!

# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
valid_ecl <- c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

passport_df <- passport_df %>% 
  filter(ecl %in% valid_ecl)

# pid (Passport ID) - a nine-digit number, including leading zeroes.
passport_df <- passport_df %>% 
  filter(str_length(pid) == 9)

# Valid passports:
nrow(passport_df)
# 156
