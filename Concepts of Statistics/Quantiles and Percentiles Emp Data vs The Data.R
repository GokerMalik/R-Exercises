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
#7- Normal Distributions

#get the male heights from the data.frame
ind <- heights$sex == "Male"
mh <- heights$height[ind] #mh = male heights

#Cumulative Distribution between intervals with real data
mean(mh <= 68.5) - mean(mh <= 67.5)
mean(mh <= 69.5) - mean(mh <= 68.5)
mean(mh <= 70.5) - mean(mh <= 69.5)
#Cumulative Distribution between intervals with normal distribution
pnorm(68.5, mean(mh), sd(mh)) - pnorm(67.5, mean(mh), sd(mh))
pnorm(69.5, mean(mh), sd(mh)) - pnorm(68.5, mean(mh), sd(mh))
pnorm(70.5, mean(mh), sd(mh)) - pnorm(69.5, mean(mh), sd(mh))
#they return almost the same value. The ideal conditions are when there is only one integer
#between the intervals.

# Course 2 
#1.3 Introduction to Distributions
#1- Definition of quantiles

#How to find the value overlaps with the 50th quantile of a custom data-set
#Result of the quantile function is a percentile
percentiles <- quantile(mh, c(0.5, 0.7))

#quantile returns a vector. To access to the value, we should use the label.
percentiles[names(percentiles) == "50%"]
percentiles[names(percentiles) == "70%"]

#how to find the value overlaps with the 50th quantile of a normal data-set
#Result of percentile is a quantile
normalPercentiles <- qnorm(c(0.5,0.7), mean(mh), sd(mh))
normalPercentiles