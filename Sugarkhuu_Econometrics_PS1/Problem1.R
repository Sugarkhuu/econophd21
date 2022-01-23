# This file contains a function for Monty hall problem and an example with 100000 draws for each strategy


monty_hall <- function(ifSwitch = FALSE) {

  door_list = c(1,2,3)        # all doors as list
  car_id    = sample(1:3,1)   # id of the door with car, Question 1 
  first_id  = sample(1:3,1)   # id of the door picked first, Question 2
  
  # Question 3
  # noCarNotPickedDoors - Doors with goat that were not picked first
  noCarNotPickedDoors = setdiff(door_list, c(first_id,car_id)) # if first_id = car_id, then 2 doors, if not 1 door
  
  goatOpen_id = if (length(noCarNotPickedDoors) == 1) noCarNotPickedDoors else sample(noCarNotPickedDoors,1)
    
  # Strategies - Question 4 and 5
  if (ifSwitch == FALSE){
  
      second_id = first_id # No switch strategy, Question 4  
  
  } else {
  
      second_id = setdiff(door_list, c(first_id,goatOpen_id)) # Switch strategy, Question 5  
    
  }
  
  if (car_id == second_id) {
      result = TRUE
  } else {
      result = FALSE
  }
  
  return(result)

}



# Testing the strategies, Question 6 - Demonstration
set.seed(1234) #setting the seed
times = 100000


# Stay

ifSwitch = FALSE
winTotal = 0

for (i in 1:times) {
  winThis  = if (monty_hall(ifSwitch)) 1 else 0
  winTotal = winTotal + winThis
}

winFracStay = winTotal/times


# Switch

ifSwitch = TRUE
winTotal = 0

for (i in 1:times) {
  winThis  = if (monty_hall(ifSwitch)) 1 else 0
  winTotal = winTotal + winThis
}

winFracSwitch = winTotal/times


# Print outcome
sprintf('Winning fraction of the Stay Strategy is: %.4f', winFracStay)
sprintf('Winning fraction of the Switch Strategy is: %.4f', winFracSwitch)