library(tidyverse)

# Bayes Theorem Simulation
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE,
                  prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease")
N_D

N_H <- sum(outcome == "Healthy")
N_H

accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace = TRUE,
                                     prob = c(accuracy, 1 - accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace = TRUE,
                                     prob = c(accuracy, 1 - accuracy))
table(outcome, test)

# Case study: Election Forecasting

## Regression 
# Case study: Is height hereditary?

install.packages("HistData")
library(HistData)
data("GaltonFamilies")

set.seed(1983)
galton_heights <- GaltonFamilies %>% 
  filter(gender == "male") %>% 
  group_by(family) %>%
  sample_n(1) %>% 
  ungroup() %>% 
  select(father, childHeight) %>% 
  rename(son = childHeight)

galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

cor(galton_heights$father, galton_heights$son) # or
galton_heights %>% 
  summarize(r = cor(father, son)) %>% 
  pull(r)




