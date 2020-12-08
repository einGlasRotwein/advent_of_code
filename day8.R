
library(tidyverse)

# Read in data
# https://adventofcode.com/2020/day/8/input
day8 <- readLines("./data/input_day8.txt")

# Execute a tiny little program that is stuck in an infinite loop :-)

#### PART 1 ####
# Goal: Determine which value the accumulator (a variable that is increasing as the
# programme progresses) has right before the programme runs into the loop.

# First: Write a little function that executes the programme.
# Takes a position and returns a position and the accelerator status.
# Also takes the "programme" and the current accelerator status
execute_program <- function(position, programme, accelerator) {
  temp_content <- programme[position]
  temp_command <- str_extract(temp_content, "[a-z]+")
  temp_number <- str_extract(temp_content, "[0-9+-]+") # extract number with sign
  temp_number <- as.numeric(temp_number)
  
  if (temp_command == "jmp") {
    new_position <- position + temp_number
  } else {
    new_position <- position + 1
  }
  
  if (temp_command == "acc") accelerator <- accelerator + temp_number
  
  return(c(new_position, accelerator))
}

# Run through programm and keep track of the positions visited. As soon as a position is
# visited again, return the current accelerator value.
places_visited <- vector(mode = "numeric")

recursive_execution <- function(position, programme, accelerator, places_visited) {
  next_values <- execute_program(position, programme, accelerator)
  places_visited <- c(places_visited, position) # add position to places visited
  position <- next_values[1] # update position
  
  if (length(places_visited) != length(unique(places_visited))) {
    stop(paste("Stopped at accelerator =", accelerator))
  } else {
    accelerator <- next_values[2] # update accelerator
    return(recursive_execution(position, programme, accelerator, places_visited))
  }
}

recursive_execution(1, day8, 0, places_visited)
# Stopped at accelerator = 1675

#### PART 2 ###
# Goal: By changing one nop to a jmp or the other way around, the program will execute normally.
# What is the value of the accumulator AFTER the programm terminates (i.e. AFTER it ran the last
# line).

# First of all: How many jump or nop instructions do I have?
commands <- str_extract(day8, "[a-z]+")
table(commands)
# acc jmp nop 
# 326 224  55 

# I think I have to go through all of them and just try.
# Locations that can possibly be changed:
possible_changes <- (1:length(day8))[commands %in% c("jmp", "nop")]

# However, we throw out the locations which are a nop and where it wouldn't make sense to
# change them to a jump. For example, changing a nop 0 to a jump would lead to an infinite loop.
numbers <- as.numeric(str_extract(day8, "[0-9+-]+"))

# However, there are no nops that are 0 :-(
sum(commands == "nop" & numbers == 0)

# How about nops where changing them to a jump would throw us off the programme,
# because the position would be <1 or >605.
pos_nops <- (1:length(day8))[commands == "nop"] # positions of nops
nop_numbers <- numbers[commands == "nop"] # corresponding values

# We don't want the nops where the position + the value is <1 or >605
# ... which doesn't happen anyways.
sum((pos_nops + nop_numbers) < 1)
sum((pos_nops + nop_numbers) > 605)

# So we loop through our positions and try to execute the programme. We stop it when it reaches
# a location again, like before.
# However, there are a few changes:
# a) We use the accelerator AFTER the previous line has been executed, to be able to report the
#    latest accelerator state (after the programme terminates).
# b) We stop the execution when the last line (605) has been visited.
# c) We change the previous stop to a return, so we can run the programme repeatedly.
recursive_execution2 <- function(position, programme, accelerator, places_visited) {
  next_values <- execute_program(position, programme, accelerator)
  places_visited <- c(places_visited, position) # add position to places visited
  position <- next_values[1] # update position
  accelerator <- next_values[2] # update accelerator
  
  if (position == 605) stop(paste("SUCCESSFUL RUN! ACCELERATOR = "), accelerator)
  
  if (length(places_visited) != length(unique(places_visited))) {
    return(paste("Fail. Stopped at accelerator =", accelerator))
  } else {
    return(recursive_execution2(position, programme, accelerator, places_visited))
  }
}

for(i_change in seq_along(possible_changes)) {
  temp_change_position <- possible_changes[i_change]
  old_command <- str_extract(day8[temp_change_position], "[a-z]+")
  new_command <- ifelse(old_command == "nop", "jmp", "nop")
  temp_number <- as.numeric(str_extract(day8[temp_change_position], "[0-9+-]+"))
  new_line <- paste(new_command, temp_number)
  
  temp_programme <- day8
  temp_programme[temp_change_position] <- new_line
  
  places_visited <- vector(mode = "numeric")
  recursive_execution2(1, temp_programme, 0, places_visited)
}
# SUCCESSFUL RUN! ACCELERATOR = 1532 
