survey <- read.csv("survey.csv")

mean(survey$age)


install.packages("psych", dep=TRUE)
library(psych)

names(survey)

describe(survey$Moptim)  #Question 6

survey$Moptim_centered <- survey$Moptim - mean(survey$Moptim, na.rm = TRUE) #Question 7

#Question 8
class(survey$age)
class(survey$Mlifesat)

#Question 9
test_scores <- c(83, 91, 75, 85, 85, 67, 94, 78, 70, 89)

#Question 10
length(test_scores)
mean(test_scores)
median(test_scores)

#Question 11
rm(test_scores)
