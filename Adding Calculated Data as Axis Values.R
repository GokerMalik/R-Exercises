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
#6- Comparing Distributions

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

#creatre smooth density
p <- gapminder %>% filter(year %in% years & country %in% countryList) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  #as there aren't many countries in West compared to developing countries,
  #it's normal that majority is around the same value. In developing world,
  #There are many countries that adds their ratio for "dollars per day" to 1.
  #This make it look like there are more countries in West than there are in developing world.
  #this is why we use the line below and rely on the number, rather than ratio
  ggplot(aes(dollars_per_day, y=..count.., fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw=0.75) +
  facet_grid(year~.)

p