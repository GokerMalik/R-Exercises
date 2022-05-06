#Check if the required packages were installed and attached
packsAsked <- c()

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

#configure the RNG to ensure the interoperability with the course material
set.seed(1986, sample.kind = "Rounding")

# Course 3 
#1.3 Addition Rule and Monty Hall
#4- The Monty Hall Problem

B <- 1000

stick <- replicate(B, {
  
  #define three doors
  doors <- as.character(1:3)
  #randomly distribute objects behind the doors
  prize <- sample(c("car", "goat", "goat"))
  #find the door with the same index with the car
  prize_door <- doors[prize == "car"]
  #randomly pick a door
  my_pick <- sample(doors, 1)
  #show a random door
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  #decie whether choose what's shown or change
  stick <- my_pick
  #check if you could win the price
  stick == prize_door

})

mean(stick)