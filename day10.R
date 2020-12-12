
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/10/input
day10 <- readLines("./data/input_day10.txt")
day10 <- as.numeric(day10)

#### PART 1 ####
# If I want to use all adapters, I probably just have to sort them in ascending order
# and record the differences between all elements.
# I must not forget that I have my built-in adapter (always 3 higher than the highest adapter).
# And I DID forget to add the charging outlet. So I'll add it now.
adapter_chain <- sort(day10)
adapter_chain <- c(adapter_chain, max(adapter_chain) + 3) # add built-in adapter
adapter_chain <- c(0, adapter_chain) # add charging outlet

# Table of differences between the elements
(diffs <- table(diff(adapter_chain)))

prod(diffs) # multiply (we only have 1 and 3 jolt differences)
# 2059

#### PART 2 ####
# What are ALL valid ways to arrange the adapters?

# Approach: The way I identified in example 1 is the maximal solution.
# There is also a minimal solution, where I delete all the "unnecessary" adapters.
# I.e. when the difference between the 1st and the 3rd adapter is no more than 3, the
# 2nd adapter can go. The number of adapters which are missing in this configuration are
# "optional" adapters. I need to determine how many "adapter present/absent" combinations
# I can create with these remaining adapters.
maximal_solution <- adapter_chain

# Any numbers with a difference of 3 need to stay for the minimal solution.
# So these ...
idx <- which(diff(maximal_solution) == 3)

# ... and these indices
idx <- c(idx, which(diff(maximal_solution) == 3) + 1)
idx <- sort(idx)
idx <- unique(idx)

# These numbers need to be present. However, now we have some numbers that are further away
# from each other than 3 steps. (Plus the built-in adapter and the charging outlet).
fixed_adapters <- c(0, maximal_solution[idx], max(adapter_chain) + 3)

# The number of combinations is now determined by the number of free spots between the fixed
# adapters and the number of adapters that fit into these gaps.
# The maximum distance between adapters is 4. In these cases, I need to set at least one
# adapter. In all other cases, I MAY set an adapter - but I don't need to.
max(diff(fixed_adapters))

# E.g. when there is a gap of one between the fixed adapters, there can be no other adapter.
# When there is a gap of 2, there may or may not be an adapter (if I have one that I can put there).
# When there is a gap of 3, there may or may not be one or two adapters (I have any I can put there).
# When there is a gap of 4, I need to put at least one adapter there.

# Remaining adapters:
free_adapters <- adapter_chain[!adapter_chain %in% fixed_adapters]

# Count the number of possible combinations
total_combs <- vector(mode = "numeric")

# Go through the gaps > 1
for (i_gap in seq_along(diff(fixed_adapters))) {
  temp_gap <- diff(fixed_adapters)[i_gap]
  if (temp_gap == 1) next
  
  # Tell me the fixed adapters it starts/ends with.
  temp_gap_start <- fixed_adapters[i_gap]
  temp_gap_end <- fixed_adapters[i_gap + 1]
  
  # Identify all adapters that can be put into the gap
  temp_set <- free_adapters[free_adapters %in% temp_gap_start:temp_gap_end]
  if (length(temp_set) == 0) next
  
  # All combinations of present/absent for that set:
  n_combs <- 2 ^ length(temp_set)
  
  # If the gap has the length of 4, one combination (all free adapters absent) is not
  # valid.
  if (temp_gap == 4) n_combs <- n_combs - 1
  
  total_combs <- c(total_combs, n_combs)
}

# There are 7 possibilities to fill the first gap, 7 to fill the second gap, 4 to fill the
# third gap ... and so on.
head(total_combs)

# The total number of combinations should be the product of these numbers.
prod(total_combs)
# 86812553324672
