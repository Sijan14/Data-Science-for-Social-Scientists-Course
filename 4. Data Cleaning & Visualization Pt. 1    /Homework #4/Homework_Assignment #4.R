# Question 1
library(tidyverse)

df <- read_csv("survey.csv")

hist(df$Mlifesat, 
     main = "Histogram of Life Satisfaction Scores",
     xlab = "Life Satisfaction Scores",
     ylim = c(0, 100),
     col = "turquoise")

# Question 2
boxplot(df$age ~ df$sex,
        names = c("Males", "Females"),
        xlab = "Biological Sex",
        ylab = "Age (in Years)")

# Question 3
plot(df$Mposaff, df$Mnegaff,
     xlab = "Positive Affect", ylab = "Negative Affect",
     ylim = c(1, 5),
     pch = 20)

# Question 4
count <- table(df$smoke)

?barplot
barplot(count,
        horiz = TRUE,
        names = c("Non-Smokers", "Smokers"),
        xlab = "Count")

# Question 5
data(midwest)

midwest %>% 
  ggplot(aes(percollege, percbelowpoverty)) +
  geom_point(aes(col = state), size = 1.5) +
  geom_smooth() +
  xlab("Percent with College Degree") +
  ylab("Percent below Poverty Line") +
  theme(legend.position = "top")

# Question 6
library(dslabs)
data("gapminder")

gapminder %>% 
  filter(country %in% c("United States", "Japan", "Egypt", "Romania")) %>% 
  ggplot(aes(year, life_expectancy)) +
  geom_line() +
  ylab("Life Expectancy") +
  xlab("Year") +
  facet_wrap(. ~ country, nrow = 2, ncol = 2)

# Question 7
data("diamonds")

P1 <- diamonds %>% 
filter(color == "I") %>% 
  ggplot(aes(carat, price)) +
  geom_point(size = 0.75) +
  xlab("Carat") +
  ylab("Price (in dollars)")
P1

library(gridExtra)
P2 <- P1 + geom_point(aes(col = cut))
P2

grid.arrange(P1, P2, ncol = 2)
