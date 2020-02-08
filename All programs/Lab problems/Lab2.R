#number of times contestans should stay
n_stay <- 0    

#number of times contestans should switch
n_switch <- 0

#number of times the randomly choose player is right
n_player <- 0

#play for 100 times
for ( i in 1:100) {
  
  #creates a vector named door that contains the numbers 1, 2, and 3
  door <- c(1,2,3) 
  
  #randomly select one of the doors to have the car behind it
  cardoor <- sample(door,1) 
  
  #randomly select the contestants choice of door
  choice <- sample(door,1) 
  
  #create a new vector that holds the values corresponding to goats
  goatdoors <- setdiff(door, cardoor) 
  
  #identify the options we have for the reveal
  reveal_options <- setdiff(goatdoors, choice)
  
  #in the situation where there are two goats to choose from,
  #we select one randomly and assign it to the variable reveal
  if (choice == cardoor) { 
    reveal <- sample(reveal_options,1)  }  
  
  #in the situation there will is only a single element in reveal_options,
  #we assign it to reveal
  else {
    reveal <- reveal_options 
  }
  
  #create a new vector which identifies the two remaining unrevealed doors
  remaining_doors <-setdiff(door, reveal)
  
  #create a new variable recording the final choice of door if the contestant switches
  newchoice <- setdiff(remaining_doors, choice)   
  
  #creates a vector named choices that contains the two choices
  choices <- c(choice, newchoice)
  
  #randomly select one of the choices
  playerchoice <- sample(choices, 1)
  
  #if the old choice is right, n_stay + 1
  if (choice == cardoor) {
    n_stay <- n_stay + 1
    
    #if the old choice is right and the player stays, n_player + 1
    if (playerchoice == choice) {
      n_player <- n_player + 1
    }
  }
  
  #if the new choice is right, n_switch + 1
  if (newchoice == cardoor) {
    n_switch <- n_switch + 1    
    
    #if the new choice is right and the player switchs, n_player + 1
    if (playerchoice == newchoice) {
      n_player <- n_player + 1
    }
  }
}

#print the result for staying
print(n_stay/100)

#print the result for switching
print(n_switch/100)

#print the results for the player
cat("The probability for the randomly choose player to win is", n_player/100)
