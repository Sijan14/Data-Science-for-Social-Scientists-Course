survey <- read.csv("survey.csv")

#Question 8
table(survey$smoke)

#Question 9
library(psych)
describe(survey$Mslfest)

#Question 10
survey$age_squareroot <- sqrt(survey$age)

#Question 11
survey$Mnegaff_reversed <- 6 - survey$Mnegaff

#Question 12
mean(survey$Mnegaff, na.rm = TRUE)
sd(survey$Mnegaff, na.rm = TRUE)

mean(survey$Mnegaff_reversed, na.rm = TRUE)
sd(survey$Mnegaff_reversed, na.rm = TRUE)
