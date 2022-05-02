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
#2.2 Introduction to Distributions
#5- Other Examples

#filter the male heights
d <- heights %>% filter(sex == "Male")

#define a smoth density plot
p1 <- d %>%
  ggplot(aes(x=height)) +
  geom_density(fill = "blue")

#define the histogram
p2 <- d %>%
  ggplot(aes(x=height)) + 
  xlab("Male heights in inches") +
  ggtitle("histogram") +
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

#define a QQ plot.
#Calculate the average and standard deviation of theoretical distribution for opt1

avg_Sd <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))

p3 <- d %>%
#  ggplot(aes(sample = height)) + #opt 1
   ggplot(aes(sample = scale(height))) + #opt 2
#  geom_qq(dparams = avg_Sd) + #opt1
   geom_qq() + #opt 2
   geom_abline()

p3
