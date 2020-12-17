
library(tidyverse)
library(binaryLogic)
# Read in data
# https://adventofcode.com/2020/day/13/input

#### PART 1 ####
day14 <- readLines("./data/input_day14.txt")

# CLEAN UP
memory_idx <- str_extract(day14, "\\[([A-Za-z0-9_]+)\\]")
memory_idx <- as.numeric(gsub("\\[|\\]", "", memory_idx))
max_memory <- max(memory_idx, na.rm = TRUE) # How large does memory have to be?

command_types <- str_extract(day14, "[a-z]+")

memory_values <- str_extract(day14, "[0-9]+$")
memory_values[command_types == "mask"] <- NA

masks <- sub("mask = ", "", day14, fixed = TRUE)
masks[command_types != "mask"] <- NA

day14_df <- data.frame(id = 1:length(day14),
                       command_types = command_types,
                       memory_idx = memory_idx,
                       memory_values = memory_values,
                       masks = masks)

# Hacky way to repeat mask
day14_df <- day14_df %>% 
  mutate(is_mask = !is.na(masks))

# Number blanks
day14_df$group_no <- NA
day14_df$group_no[day14_df$is_mask] <- 1:sum(day14_df$is_mask)

day14_df <- day14_df %>% 
  fill(group_no)

mask_lookup <- masks[!is.na(masks)]

day14_df$masks <- mask_lookup[day14_df$group_no]

day14_df <- day14_df %>% 
  filter(!is.na(memory_idx)) %>% 
  select(memory_idx, memory_values, masks) %>% 
  mutate(memory_values = as.numeric(memory_values),
         memory_idx = as.numeric(memory_idx))

day14_df$new_memory_value <- NA

for (i in 1:nrow(day14_df)) {
  temp_mask <- unlist(strsplit(day14_df$masks[i], ""))
  temp_mask <- as.numeric(ifelse(temp_mask != "X", temp_mask, NA))
  
  temp_value <- as.binary(day14_df$memory_values[i], n = 36)
  
  through_mask <- ifelse(is.na(temp_mask), temp_value, temp_mask)
  
  day14_df$new_memory_value[i] <- as.numeric(as.binary(through_mask, logic = TRUE))
}

day14_df$id <- 1:nrow(day14_df)

# Only pick the last occurence of every memory_idx (because this is the last that gets overwritten).
memory <- day14_df %>% 
  arrange(desc(memory_idx), desc(id)) %>%
  group_by(memory_idx) %>% 
  summarise(memory_idx = memory_idx[1],
            new_memory_value = new_memory_value[1])

# Sum in memory
options(scipen = 999)
sum(memory$new_memory_value)
# 7477696999511

#### PART 2 ####
part2 <- data.frame(memory_address = vector(mode = "numeric"),
                    memory_value = vector(mode = "numeric"))

for (i in 1:nrow(day14_df)) {
  mask <- unlist(strsplit(day14_df$masks[i], ""))
  memory_idx <- as.character(as.binary(day14_df$memory_idx[i], n = 36))
  memory_value <- day14_df$memory_value[i]
  
  combination <- ifelse(mask %in% c(1, "X"), mask, memory_idx)
  
  # X are floating, so:
  masklist <- list()
  
  for(i in 1:sum(combination == "X")) {
    masklist[[i]] <- c(0, 1)
  }
  
  floating <- expand.grid(masklist)
  
  masklist <- list()
  
  for (i in 1:nrow(floating)) {
    temp_mask <- combination
    temp_mask[combination == "X"] <- as.character(floating[i, ])
    masklist[[i]] <- temp_mask
  }
  
  # Convert all the possible memory locations to binary
  memory_locations <- lapply(
    masklist, function(x) as.numeric(as.binary(as.numeric(x), logic = TRUE))
  ) %>% 
    unlist()
  
  temp_df <- data.frame(memory_address = memory_locations,
                        memory_value = memory_value)
  
  part2 <- rbind.data.frame(part2, temp_df)
  
}

# Again, find the last value for every memory address
part2$id <- 1:nrow(part2)

# Only pick the last occurence of every memory_idx (because this is the last that gets overwritten).
memory <- part2 %>% 
  arrange(desc(memory_address), desc(id)) %>%
  group_by(memory_address) %>% 
  summarise(memory_address = memory_address[1],
            memory_value = memory_value[1])

# Sum in memory
options(scipen = 999)
sum(memory$memory_value)
# 3687727854171
