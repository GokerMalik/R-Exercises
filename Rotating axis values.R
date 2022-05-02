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
#4- Stratify and Boxplot

#add the dollars_per_day value
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

#define years
years <- c(pastYear = 1970, thisYear = 2010)

p <- gapminder %>% filter(year == years["pastYear"] & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p