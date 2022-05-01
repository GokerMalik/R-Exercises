#Check if the required packages were installed and attached
packsAsked <- c("dslabs")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}


#start coding here

# Course 2 
#1.2 Introduction to Distributions
#5- Normal Distributions

#get the male heights from the data.frame
ind <- heights$sex == "Male"
mh <- heights$height[ind] #mh = male heights

#calculate the average and the standard deviaton
average <- mean(mh)
standardDeviation <- sd(mh)
#built-in SD function uses "lenght(vector)-1". For short number of values, rather use
#sqrt(sum((x-average)^2)/length(vector))

#7- Normal Distributions

#Cumulative Distribution between intervals with real data
mean(mh <= 68.5) - mean(mh <= 67.5)
mean(mh <= 69.5) - mean(mh <= 68.5)
mean(mh <= 70.5) - mean(mh <= 69.5)
#Cumulative Distribution between intervals with normal distribution
pnorm(68.5, mean(mh), sd(mh)) - pnorm(67.5, mean(mh), sd(mh))
pnorm(69.5, mean(mh), sd(mh)) - pnorm(68.5, mean(mh), sd(mh))
pnorm(70.5, mean(mh), sd(mh)) - pnorm(69.5, mean(mh), sd(mh))
#they return almost the same value. The ideal conditions are when there is only one integer
#betwwen the intervals.
