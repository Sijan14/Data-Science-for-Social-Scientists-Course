#Question 1
survey <- read.csv("survey.csv")

class(survey$age_group)

survey$age_group <- as.factor(survey$age_group)
class(survey$age_group)

#Question 2
library(tidyverse)
survey %>% 
  group_by(child) %>% 
  summarize(average = mean(age, na.rm = TRUE), standard_deviation = sd(age, na.rm = TRUE))

#Question 3
library(dslabs)
data(murders)

murders$murder_rate_traditional <- murders$total/murders$population * 10^5

murders <- mutate(murders, murder_rate_tidy = total/population * 10^5)

#Question 4
test_grade <- 61

if(test_grade >= 60){
  print("Pass")
}else{
  print("Fail")
}

test_grade <- 59

#Question 5
squaring_function <- function(arg){
  output = arg^2
  return(output)
}

squaring_function(5)
squaring_function(10)
