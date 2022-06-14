#Check if the required packages were installed and attached
packsAsked <- c("dplyr")

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
#4: Statistical Models
#1- Poll Aggregators

# Victory of Barack Obama

# Generate the results for 12 polls taken
#mimic sample sizes from the actual polls.
d <- 0.39
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

find_intervals <- function(N){
  
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat)-1
  
  
}

confidence_intervals <- sapply(Ns, find_intervals)

polls <- data.frame(poll = 1: ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_Size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# Nate Silver's approach

# Nate silver realised that we could combine the poll data to obtain a bigger poll.
# Although the raw data is not publicly available, we can use math.

#here, Mr Silver's approach combines the samples from 12 polls.

d_hat <- polls %>% summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% .$avg

p_hat <- (1+d_hat)/2

moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))