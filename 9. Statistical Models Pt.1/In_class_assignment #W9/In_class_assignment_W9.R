library(tidyverse)
library(stats)

# Question 1
df <- read_csv("survey.csv")

df <- df %>% 
  mutate(age_squared_deviation = (age - mean(age))^2)

sum_of_squares <- sum(df$age_squared_deviation)
n <- nrow(df)
sd <- sqrt(sum_of_squares/(n-1))
identical(sd, sd(df$age))

# Question 2
se <- 15/sqrt(36)
z <- (103.5 - 100)/se

# Question 3
t.test(df$Mposaff ~ df$child)

# Question 4
anovaTest <- aov(Mmast ~ factor(educ), data = df)
summary(anovaTest)

tukey <- TukeyHSD(anovaTest)

install.packages("effectsize")
library(effectsize)
eta_squared(anovaTest)

# Question 5
model <- lm(Moptim ~ sex + age + factor(educ) + Mposaff + Mnegaff, data = df)
summary(model)

hist(residuals(model))

library(car)
vif(model)
1/vif(model)
