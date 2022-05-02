#Check if the required packages were installed and attached
packsAsked <- c("dslabs", "dplyr", "tidyverse", "ggrepel")

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
#1- Layers
#2- Tinkering
#3- Scales, labels, colors
#4- Add-on packages

#calculate the country-wise rate and store in variable "r"
r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6) %>% .$rate

#make a customised ggplot
p1 <- murders %>% ggplot(aes(population/10^6, total, col = region)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Population in millions (log10)") +
  ylab("Total number of murders (log10)") +
  scale_color_discrete(name = "Regions") +
  ggtitle("Gun murders in US in 2010") +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") + ####here, it isn't clear that why using intercept works####
  geom_point(size = 3) +
# geom_text(aes(label = abb), size = 3, nudge_x = 0.060, nudge_y = 0.080) #standard geom_text
  geom_text_repel(aes(label = abb), size = 3) #geom_text to prevent overlappings

p1