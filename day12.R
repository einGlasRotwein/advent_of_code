
# Read in data
# https://adventofcode.com/2020/day/12/input
day12 <- readLines("./data/input_day12.txt")

#### PART 1 ####
# Position is a vector with two elements c(vertical, horizontal),
# where horizontal is W = -1; E = 1 and vertical is S = -1; N = 1
# Facing is a character (N, E, S, W)
# Returns the position and where the ship is facing.
move_ship <- function(command, position, facing) {
  letter <- str_extract(command, "[A-Z]+")
  value <- as.numeric(str_extract(command, "[0-9]+"))
  
  if (letter == "N") {
    position[1] <- position[1] + value
  } else if (letter == "S") {
    position[1] <- position[1] - value
  } else if (letter == "E") {
    position[2] <- position[2] + value
  } else if (letter == "W") {
    position[2] <- position[2] - value
    
  } else if (letter == "F") {
    direction <- ifelse(facing %in% c("N", "S"), 1, 2)
    sign <- ifelse(facing %in% c("N", "E"), 1, -1)
    position[direction] <- position[direction] + sign * value
  } else if (letter == "R") {
    compass <- c("N", "E", "S", "W")
    position_compass <- which(compass == facing)
    move <- value/90
    idx <- (position_compass + move) %% 4
    if (idx == 0) idx <- 4
    facing <- compass[idx]
    
  } else if (letter == "L") {
    compass <- c("N", "E", "S", "W")
    position_compass <- which(compass == facing)
    move <- value/90
    idx <- (position_compass - move) %% 4
    if (idx == 0) idx <- 4
    facing <- compass[idx]
  }
  
  return(list(position, facing))
}

move_ship_repeatedly <- function(commands, position, facing) {
  for (i in seq_along(commands)) {
    temp_command <- commands[i]
    new_values <- move_ship(temp_command, position, facing)
    position <- new_values[[1]]
    facing <- new_values[[2]]
  }
  
  return(list(position, facing))
}

# Move ship along the puzzle input commands
final_position <- move_ship_repeatedly(day12, c(0, 0), "E")

sum(abs(final_position[[1]]))
# 1221

#### PART 1 ####
# Waypoint follows the same logic as position
move_ship <- function(command, position, waypoint) {
  letter <- str_extract(command, "[A-Z]+")
  value <- as.numeric(str_extract(command, "[0-9]+"))
  
  if (letter == "N") {
    waypoint[1] <- waypoint[1] + value
  } else if (letter == "S") {
    waypoint[1] <- waypoint[1] - value
  } else if (letter == "E") {
    waypoint[2] <- waypoint[2] + value
  } else if (letter == "W") {
    waypoint[2] <- waypoint[2] - value
    
  } else if (letter == "R") {
    
    if (value == 270) {
      waypoint[1] <- waypoint[1] * -1
      waypoint <- waypoint[c(2, 1)]
    } else if (value == 180) {
      waypoint <- waypoint * -1
    } else if (value == 90) {
      waypoint[2] <- waypoint[2] * -1
      waypoint <- waypoint[c(2, 1)]
    }
    
  } else if (letter == "L") {
    
    if (value == 270) {
      waypoint[2] <- waypoint[2] * -1
      waypoint <- waypoint[c(2, 1)]
    } else if (value == 180) {
      waypoint <- waypoint * -1
    } else if (value == 90) {
      waypoint[1] <- waypoint[1] * -1
      waypoint <- waypoint[c(2, 1)]
    }
    
  } else if (letter == "F") {
    move <- waypoint * value
    position <- position + move
  }
  
  return(list(position, waypoint))
}

move_ship_repeatedly <- function(commands, position, waypoint) {
  for (i in seq_along(commands)) {
    temp_command <- commands[i]
    new_values <- move_ship(temp_command, position, waypoint)
    position <- new_values[[1]]
    waypoint <- new_values[[2]]
  }
  
  return(list(position, waypoint))
}

final_position <- move_ship_repeatedly(day12, c(0, 0), c(1, 10))

sum(abs(final_position[[1]]))
# 59435
