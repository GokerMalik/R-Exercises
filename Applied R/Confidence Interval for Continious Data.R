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

# Course 4 
#6: Election Forecasting
#7- Assessment


# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000


# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  
  sam <- sample(x, N, replace = TRUE)
  
  se_hat <- sd(sam) / sqrt(N)
  
  interval <- c(mean(sam) - qnorm(0.975)*se_hat, mean(sam) + qnorm(0.975)*se_hat)
  between(mu, interval[1], interval[2])
  
  
})


# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.

mean(res)