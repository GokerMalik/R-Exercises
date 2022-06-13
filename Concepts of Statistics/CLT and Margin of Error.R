#Check if the required packages were installed and attached
packsAsked <- c("tidyverse", "dslabs")

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

# Course 4 
#2 The Central Limit Theorem in Practice
#2- Margin of Error

ds_theme_set()
take_poll(25)

#the poll gives us 12 Blue beads and 13 red beads
#proportion of blue beads equals to 12/25 = 0.48

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/2000)

se
pnorm(0.01/se) - pnorm(-0.01/se)



#the line below actually calculates the probability of being 2SE away from the average.t
pnorm(2) - pnorm(-2)
