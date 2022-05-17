#Check if the required packages were installed and attached
packsAsked <- c("dplyr", "dslabs")

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
#2.1 Continious Probability
#5- Monte Carlo Simulations

####Simulate a normal distribution with random values and replicate real heights data#####
x <- heights %>% filter(sex == "Male") %>% .$height
n <- length(x)
avg <- mean(x)
sd <- sd(x)
simulated_data <- rnorm(n, avg, sd)

##find the max value of 10000 random normal distribution with 800 elements.
ntimes <- 10000

tallest <- replicate(ntimes, {
  
  simulation <- rnorm(800, avg, sd)
  max <- max(simulation)
  
})

#check what is the probability that we are going to have a
#seven footer as the tallest person when the populaion is 800
mean(tallest >= 7*12)