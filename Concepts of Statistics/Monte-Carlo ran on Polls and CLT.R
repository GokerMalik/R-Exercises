#Check if the required packages were installed and attached
packsAsked <- c("tidyverse", "gridExtra")

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
#3- A Monte Carlo Simulation for the CLT

N <- 1000
p <- 0.45

#the code below is to simulate one poll. Although it returns the
#proportion of X in a sample, we actually expect it to be P
X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p, p))
X_bar <- mean(X)

#the theory tells us that we can calculate the standard error of X_bar as follows:
se_Xbar = sqrt(X_bar*(1-X_bar)/N)


#the code below is to put this into practice through a monte-carlo simulation.

B <- 10000
X_hat <- replicate (B, {
  
  X <- sample(c(0,1), size = N, replace = TRUE, prob=c(1-p, p))
  mean(X)
  
})

#now we have our data comes from the monte-carlo simulation. We can compare the
#observation with the theory

#theoric average
X_bar

#observed average
mean(X_hat)

#theoric standard error
se_Xbar

#observed standard error
sd(X_hat)


#check how accurate the data is by running this monte-carlo
p1 <- data.frame(X_hat = X_hat) %>%
  ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(X_hat = X_hat) %>%
  ggplot(aes(sample = X_hat)) +
  stat_qq(dparams = list(mean = mean(X_hat), sd = sd(X_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)