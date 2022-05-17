#Check if the required packages were installed and attached
packsAsked <- c("gtools")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

#configure the RNG to ensure the interoperability with the course material
set.seed(1, sample.kind = "Rounding")

# Course 3 
#1.2 Combinations and Permutations
#2- The Birthday Problem

#define a classroom of 50 people with birthdays
n <- 50
bdays <- sample(1:365, n, replace = TRUE)

#check if any was duplicated
any(duplicated(bdays))

#run a monte-carlo experiment to check the probability of two people having the same birthday
B <- 10000
results <- replicate (B, {
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})

mean(results)