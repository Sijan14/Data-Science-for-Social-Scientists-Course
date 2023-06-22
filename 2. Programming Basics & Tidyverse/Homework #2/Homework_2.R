library(tidyverse)

# Question 2
survey <- read.csv("survey.csv")
survey <- survey %>% 
  select(age, educ, Mposaff, Mnegaff) %>% 
  filter(age < 40)

# Question 3
library(dslabs)
data(heights)

heights %>% 
  group_by(sex) %>% 
  summarize(Average = mean(height),
            SD = sd(height),
            Min = min(height),
            Max = max(height))

# Question 4
test_scores <- c(45, 53, 67, 98, 58, 76, 87, 56, 65, 56)

for (i in test_scores) {
  if (i >= 80) {
    print("above average")
  } else if(between(i, 60, 80)){
    print("average")
  } else {
    print("below average")
  }
}

# Question 5
performance_prediction <- function(arg){
  output = (2.75 * arg) + 37.253
  return(output)
}

performance_prediction(c(14.5, 7, 20, 10.5, 16))
