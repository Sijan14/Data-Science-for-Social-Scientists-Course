# Question 4
library(dslabs)

data("nyc_regents_scores")
?hist

hist(nyc_regents_scores$us_history, main = "Histogram of US History Scores", xlab = "US History Scores")

# Question 5

plot(nyc_regents_scores$integrated_algebra, nyc_regents_scores$english, xlab = "Integrated Algebra Scores", ylab = "English Scores", pch = 5)

# Question 6

boxplot(nyc_regents_scores$global_history)

# Question 7

data("olive")
x <- table(olive$region)
barplot(x)

# Question 8
y <- table(olive$area)
pie(y)
