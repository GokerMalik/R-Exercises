#Check if the required packages were installed and attached
packsAsked <- c("dplyr", "ggplot2")

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
#3.1 Continuous Probability
#2- Sampling models

####ROULETTE WHEEL#####

#define the roulette wheel
color <- rep(c("Black", "Red", "Green"), c(18,18,2))

#define 1000 draws
n <- 10000
x1 <-sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)

#or
x2 <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))
S2 <- sum(x2)

#Make an experiment of having 1000 (thousand) people playing roulette for
#10 thousand times
people <- 10000
games <- 1000
play_roluette <- replicate(people, {
  
  x3 <- sample(c(-1,1), games, replace = TRUE, prob=c(9/19, 10/19))
  S3 <- sum(x3)
  S3
  
})

#find the average and the standard deviation of the play_roulette
mean(play_roluette)
sd(play_roluette)

#How close is the distribution of the money gained or lost by the casino to the normal distribution?
#Create a theoretical normal distribution with 100 elements with the max and min of the play_roluette
sequen <- seq(min(play_roluette), max(play_roluette), length = 100)
normal_density <- data.frame(th_games = sequen, th_profit = dnorm(sequen, mean(play_roluette), sd(play_roluette)))

#plot histogram for the density of observed values together with
#theoretical profit
data.frame(observed_profit=play_roluette) %>%
  ggplot(aes(observed_profit, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(th_games,th_profit), color = "blue")