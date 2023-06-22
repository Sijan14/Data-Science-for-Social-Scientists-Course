library(tidyverse)
library(dslabs)

data(murders)

x <- murders$population / 10^6
y <- murders$total
plot(x, y)

with(murders, plot(x, y))

x <- with(murders, total/population * 100000)
hist(x)

murders$state[which.max(x)]

murders$rate <- with(murders, total / population * 100000)
boxplot(rate ~ region, data = murders)

x <- matrix(1:120, 12, 10)
image(x)

# Exercise 1
population_in_millions <- log10(murders$population/10^6)
total_gun_murders <- log10(murders$total)
plot(population_in_millions, total_gun_murders)

hist(population_in_millions)

boxplot((population/10^6) ~ region, murders)

# Irizarry (2019)
# 12. 6 Exercises

install.packages("HistData")
library(HistData)

data("Galton")
x <- Galton$child

median(x)
mean(x)

mad(x)


# Case Study: Self-reported student heights
library(dslabs)
data("reported_heights")
head(reported_heights)

reported_heights <- reported_heights %>% 
  mutate(updated_heights = as.numeric(height))

reported_heights %>% 
  filter(is.na(updated_heights)) %>% 
  head()

reported_heights <- filter(reported_heights, !is.na(updated_heights))

reported_heights %>% 
  group_by(sex) %>% 
  summarize(mean = mean(updated_heights), sd = sd(updated_heights),
            meidan = median(updated_heights), MAD = mad(updated_heights))

reported_heights %>% 
  arrange(desc(updated_heights)) %>% 
  head()

max_height <- quantile(reported_heights$updated_heights, .75) + 3*IQR(reported_heights$updated_heights)
min_height <- quantile(reported_heights$updated_heights, .25) - 3*IQR(reported_heights$updated_heights)

c(min_height, max_height)

reported_heights %>% 
  filter(!between(updated_heights, min_height, max_height)) %>% 
  select(height) %>% 
  head(n = 10)

