#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

#configure the RNG to ensure the interoperability with the course material

# Course 3 
#2.1 Continious Probability
#1- Continuous Probability

#get a list of male heights
x <- heights %>% filter(sex=="Male") %>% pull(height)

#define the function to calculate the continuous probability
ConProb <- function(a) mean(x <= a)

#find the probability of picking a student taller than 70 inches
1 - ConProb(70)

# Course 3 
#2.1 Continious Probability
#2- Theoretical Distribution

#find the probability that a student is taller than 70.5 inches. Assume the data-set a normal distribution.
1- pnorm(70.5, mean(x), sd(x))

# Course 3 
#2.1 Continious Probability
#3- Probability Density

