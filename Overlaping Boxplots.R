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
#5- Comparing Distributions

#add the dollars_per_day value
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

#define years
years <- c(pastYear = 1970, thisYear = 2010)

#define western regions
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")

#find the countries with available data for two of the years and take the intersection
countries1 <- gapminder %>% filter(year == years["pastYear"] & !is.na(gdp)) %>% .$country
countries2 <- gapminder %>% filter(year == years["thisYear"] & !is.na(gdp)) %>% .$country
countryList <- intersect(countries1, countries2)

p <- gapminder %>% filter(year%in%years & country %in% countryList) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) +
  scale_fill_discrete(name = "Years")
  
p