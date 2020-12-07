
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/7/input
day7 <- readLines("./data/input_day7.txt")

# It's a lot of bags stacked in each other. 
# See https://adventofcode.com/2020/day/7 for the logic behind it.

#### PART 1 ####
# GOAL: How many bags can contain at least one shiny gold bag?

# There's probably better ways of doing this, but I don't know how.

# Start with the bag lists (everything before "contains" in the string)
bag_names <- gsub("(contain).*","\\1", day7)
bag_names <- gsub(" bags contain", "", bag_names)

# Bag content
content <- gsub(".*(contain)","\\1", day7)
content <- gsub("contain ", "", content)
content <- gsub(" bags", "", content)
content <- gsub(" bag", "", content)
content <- gsub(".", "", content, fixed = TRUE)

content <- lapply(content, function(x){
  trimws(unlist(strsplit(x, split = ",")))
})

# This is how we find each bag directly containing a shiny gold bag
shiny_idx <- lapply(content, function(x) {
  any(grepl("shiny gold", x, fixed = TRUE))
}) %>% unlist()

bag_names[shiny_idx]

# ... and now we would need to find all bags that contain THESE bags.
# This can probably be done with recursive stuff, but my head doesn't want to work right now,
# so I'll do a while loop.

# Function that finds bags containing a certain bag (or certain bags)
find_bag <- function(bag_names, search_space, search_term) {
  shiny_idx <- lapply(search_space, function(x) {
    any(grepl(search_term, x))
  }) %>% unlist()
  
  bags_found <- bag_names[shiny_idx]
  
  return(bags_found)
}

bags_found <- "dummy"
search_term <- "shiny gold"
target_bags <- vector(mode = "character")

while (length(bags_found) != 0) {
  bags_found <- find_bag(bag_names, content, search_term)
  target_bags <- c(target_bags, bags_found)
  
  search_term <- paste0("(", bags_found, ")")
  search_term <- paste0(search_term, collapse = "|")
  
  bags_found <- find_bag(bag_names, content, search_term)
}

# Number of bags found is the number of UNIQUE target bags!
length(unique(target_bags))
# 151

#### PART 2 ####
# How many bags does a shiny bag contain?

# Function counting the bags in a given bag
count_bags <- function(bag) {
  count_df <- data.frame(counts = vector(mode = "numeric"),
                         idx = vector(mode = "numeric"),
                         bag_name = vector(mode = "character"))
  
  for (i_bag in seq_along(bag)) {
    temp_content <- unlist(content[bag_names == bag[i_bag]])
    temp_counts <- str_extract_all(temp_content, "[0-9]", simplify = TRUE)
    if (length(temp_counts) == 0) temp_counts <- 0
    temp_counts <- as.numeric(temp_counts)
    
    temp_df <- data.frame(counts = temp_counts,
                          idx = i_bag,
                          bag_name = bag[i_bag])
    
    count_df <- rbind.data.frame(count_df, temp_df)
  }
  
  return(count_df)
}

bag_content <- function(bag) {
  names_df <- data.frame(names = vector(mode = "character"),
                         bag_name = vector(mode = "character"))
  
  for (i_bag in seq_along(bag)) {
    temp_content <- unlist(content[bag_names == bag[i_bag]])
    temp_names <- gsub("[0-9]", "", temp_content)
    temp_names <- trimws(temp_names)
    
    temp_df <- data.frame(names = temp_names,
                          bag_name = bag[i_bag])
    
    names_df <- rbind.data.frame(names_df, temp_df)
  }
  
  return(names_df)
}

search_term <- "shiny gold"
bag_counter <- 0
multiplier <- 1

while(length(search_term) != 0) {
  temp_counts <- count_bags(search_term)
  
  multiplier <- multiplier[temp_counts$idx]
  
  # Each bag needs to be multiplied with the number of times it appears
  temp_counts$counts <- temp_counts$counts * multiplier
  bag_counter <- bag_counter + sum(temp_counts$counts)

  search_term <- bag_content(search_term)$names
  search_term <- search_term[search_term != "no other"]
  
  temp_counts <- temp_counts[temp_counts$counts != 0, ]
  
  multiplier <- temp_counts$counts
}

bag_counter
# 41559
