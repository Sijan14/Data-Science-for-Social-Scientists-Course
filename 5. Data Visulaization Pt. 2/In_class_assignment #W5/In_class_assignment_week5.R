library(tidyverse)
library(dslabs)

# Question 1
data("death_prob")
head(death_prob)

death_prob %>% 
  ggplot(aes(age, prob, col = sex, size = 0.75)) +
  geom_point() +
  xlab("Age (in years)") +
  ylab("Probability of Death") +
  scale_y_continuous(limit = c(0, 1))

# Question 2
data(diamonds)

diamonds %>% 
  filter(cut == "Premium") %>% 
  ggplot(aes(price)) +
  geom_histogram(binwidth = 200, col = "black") +
  xlab("Price (in dollars)") +
  scale_x_continuous(limit = c(0, 15000)) +
  facet_wrap(~ color, ncol = 2)

             