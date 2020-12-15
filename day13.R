
library(tidyverse)

julis_theme <- 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "top")

# Read in data
# https://adventofcode.com/2020/day/13/input

#### PART 1 ####
earliest_dep <- as.numeric(readLines("./data/input_day13.txt")[1])
busses <- readLines("./data/input_day13.txt")[2]
busses <- unlist(strsplit(busses, ","))
busses <- as.numeric(busses[busses != "x"])

# How far the busses are away at the earliest departure:
(busses - earliest_dep) %% busses

# The earliest bus is 7 minutes away ...
away <- min((busses - earliest_dep) %% busses)

# ... and it is bus number:
bus_id <- busses[which(((busses - earliest_dep) %% busses) == away)]

away * bus_id # multiply

#### PART 2 ####
busses <- readLines("./data/input_day13.txt")[2]

busses <- unlist(strsplit(busses, ","))

# Get the index of the busses -1 (because in this task, they count from 0)
bus_idx <- which(busses != "x")
# Bus 29 needs to depart at the timestamp, bus 41 needs to depart 19 minutes after the timestamp,
# bus 661 needs to depart 29 minutes after the timestamp ...
bus_idx <- bus_idx - 1
busses <- as.numeric(busses[busses != "x"])

# Sort busses
bus_idx <- bus_idx[sort(busses, index.return = TRUE)$ix]
busses <- sort(busses)

# Some busses should be further away than their interval, i.e. bus 13 should be 42 minutes
# away at the timestamp we are looking for. However, if bus 13 is 42 minutes away, that also
# means that bus 13 is 3 minutes away.
# For easier computation, we break down bus_idx based on the bus interval
bus_idx <- bus_idx %% busses

# I don't know how to solve this mathematically. Instead, I start with a timestamp
# where bus 13 is 3 minutes away. Initially, we jump ahead in time in steps of the first bus,
# making sure that we only generate the timepoints where, in this case, bus 13 is 3 minutes away.
timestamp <- 13 - 3

# How far is each bus away at this timestamp?
next_t <- busses - timestamp
(next_t <- (next_t %% busses))

n_timestamps <- 1000 # how many timestamps to generate
jumps <- 13 # jump in steps of ...

for (i_bus in 2:length(busses)) {
  timetable <- list()
  
  counter <- 1
  temp_stamp <- timestamp
  
  while ((counter -1) <= n_timestamps) {
    timetable[[counter]] <- (busses - temp_stamp) %% busses
    temp_stamp <- temp_stamp + jumps
    counter <- counter + 1
  }
  
  # Bind together list of timestamps
  timetable <- do.call(rbind.data.frame, timetable)
  names(timetable) <- paste0("bus_", 1:length(busses))
  
  timetable$t <- seq(timestamp, timestamp + jumps * n_timestamps, jumps)
  
  # Filter those lines where the next bus is the right amount of minutes away
  matches <- timetable[timetable[ , i_bus] == bus_idx[i_bus], ]
  
  # The first occurrence is our new timestamp. The difference between the first two occurrences
  # is the new jump.
  first_occurence <- matches$t[1]
  second_occurence <- matches$t[2]
  jumps <- second_occurence - first_occurence
  timestamp <- first_occurence
}

# Only print the first 100 steps
timetable[1:100, ] %>% 
  pivot_longer(bus_1:bus_9, names_to = "bus", values_to = "min_away") %>% 
  mutate(bus = sub("_", " ", bus)) %>% 
  ggplot(aes(x = t, y = min_away, colour = bus)) +
  geom_line() +
  labs(y = "minutes away") +
  julis_theme

# Confirm that all busses are the specified amount of minutes away:
(busses - first_occurence) %% busses

bus_idx

# Solution:
options(scipen = 999)
first_occurence
