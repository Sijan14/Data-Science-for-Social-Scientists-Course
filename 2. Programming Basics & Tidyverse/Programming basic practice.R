## W&G: Chapter 2 (Workflow: Basics)

x <- 3*4

this_is_a_really_long_name <- 2.5

this_is_a_really_long_name <- 3.5

r_rocks <-  2 ^ 3

seq(1, 10)

x <- "hello"

y <- seq(1, 10, length.out = 5)
y

(y <- seq(1, 10, length.out = 5))

my_variable <- 10
my_variable

#Exercise 2
library(tidyverse)

ggplot(data = "mpg") +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  filter(mpg, cyl == 8) + 
  filter(diamond, carat > 3)

## Irizarry: Chapter 4 (Programming Basics)

# 4.1 Conditional Expressions (used for "flow control")

a <- 0

# Example 1 (if-else statement)
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0")
}

# Example 2 (if-else statement)
install.packages("dslabs")
library(dslabs)
data("murders")
murder_rate <- murders$total / murders$population*100000

ind <- which.min(murder_rate)

if(murder_rate[ind] < 0.5){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}

if(murder_rate[ind] < 0.25){
  print(murders$state[ind])
} else{
  print("No state has murder rate that low")
}

# Example 1 (ifelse statement)
a <- 0
ifelse(a > 0, 1/a, NA)

a <- c(0, 1, 2, -4, 5)
result <- ifelse(a > 0, 1/a, NA)
result

data("na_example")
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))

# Example 1 (any, all)
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


## 4.2 Defining functions

# General form of a function:
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}

# Example 1 (creating function)
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
x = 1:100
avg(x)

## 4.3 Namespaces

search()

stats4::filter #to use filter from specific packages
dplyr::filter

## 4.4 For-loops

# Example 1 
compute_s_n <- function(n){        #Function
  x <- 1:n
  sum(x)
}         

m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
print(s_n)

n <- 1:m #plot
plot(n, s_n)

head(tibble(s_n = s_n, formula = n*(n+1)/2)) #comparing with the formula in a table

plot(n, s_n)
lines(n, n*(n+1)/2)

## 4.5 Vectorization and functionals

#Example 1 (simple)
x <- 1:10
sapply(x, sqrt)

#Example 2 (complex)
n <- 1:25
s_n <- sapply(n, compute_s_n)
plot(n, s_n)

## 4.6 Exercises
# 1."Not all positives"
# 2. D) all(!x)
# 3. Assign the state abbreviation when the state name is longer than 8 characters 
new_names <- ifelse(nchar(murders$state) > 8, murders$abb, murders$state)
new_names

#4. function sum_n
sum_n <- function(n){
  x = 1:n
  sum(x)
}
x = 5000
sum_n(x)

#5. plots difference against sum
altman_plot <- function(x,y){
  plot(x+y, x-y)
}

altman_plot(10, 5)

#6. 3

#7. sum of squares 
compute_s_n <- function(n){
  x = 1:n
  sum(x^2)
}
compute_s_n(10)

#8. create a vector with the sum of squares
s_n <- vector("numeric", 25)
for(n in 1:25){
  s_n[n] = compute_s_n(n)
}
s_n

#9. create a vector using sapply
x = 1:25
s_n <- sapply(x, compute_s_n)
s_n

#10. create a vector using map_dbl
s_n <- map_dbl(x, compute_s_n)
s_n

#11. plot s_n vs. n
plot(s_n, x)

#12. confirm the formula is S-n = n(n+1)(2n+1)/6
head(data_frame(s_n = s_n, formula = (x*(x+1)*((2*x)+1))/6))


## Irizarry: Chapter 5 (The tidyverse)
library(tidyverse)
co2
head(ChickWeight)

# 5.3 Manipulating Data Frames
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total/population * 100000)
head(murders)

# Subsetting with filters
filter(murders, rate <= 0.71)

# Selecting columns with select
murders_three_column <- select(murders, state, region, rate)
filter(murders_three_column, rate <= 0.71)

# 5.4 Exercises
library(dplyr)
library(dslabs)
data("murders")

#Question 1
murders <- mutate(murders, population_in_millions = population / 10^6)
murders <- mutate(murders, rate = total/population * 100000)

#Question 2
murders <- mutate(murders, rank = rank(rate))
head(murders)

#Question 3
select(murders, state, population) %>% head()

#Question 4
filter(murders, state == "New York")

filter(murders, rank <= 5)

#Question 5
no_south = filter(murders, region != "South")
nrow(no_south)

#Question 6
murders_nw = filter(murders, region %in% c("Northeast", "West"))

#Question 7
my_states <- filter(murders, rate < 1 & region %in% c("Northeast", "West"))

# 5.5 The Pipe ( %>% )
# 5.6 Exercises

#Question 1
my_states <- filter(murders, rate < 1 & region %in% c("Northeast", "West")) %>% select(state, rate, rank)
head(my_states)

#Question 2
data(murders)
my_states <- murders %>% 
  mutate(rate = total/population * 100000, rank = rank(rate)) %>% 
  filter(region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state, rate, rank)

# 5.7 Summarizing data
# Summarize, Pull, Group then summarize with group_by

# 5.8 Sorting data frames
# Nested sorting (arrange()), top_n, 

# 5.9 Exercises
install.packages("NHANES")
library(NHANES)
data(NHANES)

#Question 1
ref <- NHANES %>% 
  filter(AgeDecade == " 20-29" & BPSysAve != "NA") %>% 
  summarize(average = mean(BPSysAve), standard_deviation = sd(BPSysAve))

ref1 <- NHANES %>% 
  filter(AgeDecade == " 20-29") %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

#Question 2
ref_avg <- NHANES %>% 
  filter(AgeDecade == " 20-29") %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
  pull(average)

#Question 3
min_max <- NHANES %>% 
  filter(AgeDecade == " 20-29") %>% 
  summarize(minimum = min(BPSysAve, na.rm = TRUE), maximum= max(BPSysAve, na.rm = TRUE))

#Question 4
female <- NHANES %>% 
  filter(Gender == "female") %>% 
  group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

#Question 5
male <- NHANES %>% 
  filter(Gender == "male") %>% 
  group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

#Question 6
Combined_gender <- NHANES %>% 
  group_by(AgeDecade, Gender) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

#Question 7
middle_age_male <- NHANES %>%
  filter(Gender == "male" & AgeDecade == " 40-49") %>%
  group_by(Race1) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
  arrange(average)

# 5.10 Tibbles
murders
as_tibble(murders)

# 5.11 The dot operator (don't really understand)
# 5.12 do (kind of understand)
# 5.13 Purr package
# 5.14 Tidyverse Conditionals (case_when, between)

# 5.16 Exercises
#Question 1
data(murders)
head(murders)

#Question 2
murders_tibbles <- as_tibble(murders)

#Question 3
murders %>% 
  group_by(region)

#Question 4
murders %>% 
  .$population %>% 
  log() %>% 
  mean() %>% 
  exp() 

#Question 5
install.packages("purrr", dep = TRUE)
library(purrr)

s_n <- function(n){
  x <- 1:n
 tibble(sum = sum(x))
}
num <- 
