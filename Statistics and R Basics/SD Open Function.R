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

#calculate the average and the standard deviation
average <- mean(mh)
standardDeviation <- sd(mh)
#built-in SD function uses "length(vector)-1". For short number of values, rather use
#sqrt(sum((x-average)^2)/length(vector))