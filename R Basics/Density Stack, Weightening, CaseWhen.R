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
#6- Density Plots

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

#re-organise the data and define the groups in more detail
gapminder <- gapminder %>%
  mutate (group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others")) %>%
#pipe the new gapminder dataframe and re-order the groups in desired order
  mutate(group = factor(group, levels = c("Others","Latin America","East Asia","Sub-Saharan Africa","West"))) %>%
#create weight value for the countries by their population differently for each year
  group_by(year) %>%
  mutate(weight = population/sum(population)*2) %>%
  ungroup()


#greare the plot
p <- gapminder %>%
  filter(year %in% years & country %in% countryList) %>%
  #we adjust the weight at the line below
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

p