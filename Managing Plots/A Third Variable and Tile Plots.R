#Check if the required packages were installed and attached
packsAsked <- c("dslabs","dplyr")

insDemandIndex <- !(packsAsked %in% rownames(installed.packages()))
sapply(packsAsked[insDemandIndex], install.packages)

attDemandIndex <- !(packsAsked %in% .packages())
packsToAtt <- packsAsked[attDemandIndex]

for (i in packsToAtt){
  library(i, character.only = TRUE)
}

#start coding here

# Course 2 
#4.3 Data Visualization Principles, Part 2
#3- Case Study: Vaccines

#calculate the rate per 10.000 per year
data1 <- us_contagious_diseases %>%
  #we exclude Hawaii and Alaska as they became states at around 1950s
  filter(!state %in% c("Hawaii", "Alaska") & disease == "Measles", !weeks_reporting == 0) %>%
  #if the cases are not reported some weeks, the yearly rate would be lower
  #the code below is to adjust this
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

#plot the rates for california. Indicate the year 1963 when the vaccine
#was introduced
p1 <- data1 %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

p2 <- data1 %>%
  filter(!is.na(rate)) %>%
  ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle("Measles") +
  ylab("") +
  xlab("")


# compute the US average rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
p3 <- data1 %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  #plot all the states
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  #plot the US average
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  #locate the label
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  #indicate 1963
  geom_vline(xintercept = 1963, col = "blue")

p3