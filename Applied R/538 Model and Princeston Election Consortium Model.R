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

data("polls_us_election_2016")
head(results_us_election_2016)


##################################################
##### Top 5 states ranked by electoral votes #####
##################################################

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#######################################################################
##### Computing the average and standard deviation for each state #####
#######################################################################

results <- polls_us_election_2016 %>%
  #Exclude popular vote
  filter(state != "U.S." &
           #exclude congressional districts
           !grepl("CD", state) &
           #exclude early polls
           enddate >= "2016-10-31" &
           #exclude lowly graded
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  #calculate spread
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  #calculate the average and the standard deviation
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))



# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
# na.rm removes the NA data from the calculation.
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))



#######################################################################
##### Calculating the posterior mean and posterior standard error #####
#######################################################################

###Assuming that the sampling varies in the way to neuteralise each-other
mu <- 0
tau <- 0.02


results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))



########################################################
##### Monte-Carlo simulation of the election night #####
########################################################

#Princeston Election Consortium

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     # This is where to start calculating the results
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     #award Clinton if she won
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%
    #calculate the total awards for clinton
    summarize(clinton = sum(clinton)) %>%
    # 7 votes for Rhode Island and DC
    .$clinton + 7
})

# Check the percentage that Clinton could get more than 269 votes
mean(clinton_EV > 269)

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)


########################################################
##### Simulation that the general bias is included #####
########################################################

#FiveThirtyEight

mu <- 0
tau <- 0.02
#for the state level, we are going to assume that the general bias is larger. We are gonna use 0.03.
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  # Here, this time we include the general bias
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7
})

mean(clinton_EV_2 > 269)