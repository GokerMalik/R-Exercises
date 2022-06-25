#Check if the required packages were installed and attached
packsAsked <- c("dplyr", "dslabs", "ggplot2")

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
#3- Poll Data and Pollster Bias

# Victory of Trump

#filter the pollls to include only the federal data and which is remarked, at least as B+ or non-graded.
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

#calculate the spread
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

#estimate the spread
d_hat <- polls %>% summarize(d_hat = sum (spread * samplesize)/sum(samplesize)) %>%
  .$d_hat

#estimate the probability of Clinton wins
p_hat <- (d_hat + 1)/2

#estimate the marigin of error
moe <- qnorm(0.95)*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
(2*p_hat) - 1
moe

# However, if we check the data, it doesn't seem to be randomly distributed
polls %>% ggplot(aes(spread)) + geom_histogram(color = 'black', binwidth = .01)


#some of the pollsters were regularly polling before the week of the elections.
#filter the data to show them

polls %>% group_by(pollster)%>%
  filter(n()>=6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#the theory to use multiple polls to obtain a larger data was working correctly when the
#result of the polls were closer. However, there are differences across the polls
#which can not be explained by the theory.


# Course 4 
#4: Statistical Models
#4- Data-Driven Models

#take only the latest polls from wach pollster.
#polls <- polls_us_election_2016

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

#check the histogram
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

#CLT can explain how the data could be processed. To make the data
#normally distributes, we can simply use the average of the spreads as
#the spread is a random variable.

sd(one_poll_per_pollster$spread)

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(0.975)*se, end = avg + qnorm(0.975)*se)
round(results*100,1)