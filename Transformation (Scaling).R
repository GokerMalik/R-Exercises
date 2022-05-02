#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr", "tidyverse")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#3.2 Using the Gapminder Dataset
#3- Transformations

#add the dollars_per_day value
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

#define yars
years <- c(pastYear = 1970, thisYear = 2010)

#define the plot but scale the values
p1 <- gapminder %>% filter(year == years["pastYear"] & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

#or alternatively, define the plot but scale the axis  
p2 <- gapminder %>% filter(year == years["pastYear"] & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

#note that the plot looks exactly the same, but the axis!!
p1
