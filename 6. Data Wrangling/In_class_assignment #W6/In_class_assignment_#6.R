library(tidyverse)

# Question 1
df <- read_csv("class_roster.csv")
df <- df %>% 
  separate(Student, into = c("LastName", "FirstName"))

# Question 2
df <- df %>% 
  unite(FullName, FirstName, LastName, sep = " ")

# Question 3
library(readxl)
df1 <- read_excel("wide_data_example.xlsx")

long_data <- df1 %>% 
  gather("time1", "time2", "time3", key = "time", value = "test_score")

# Question 4
library(haven)
df2 <- read_sav("time1.sav")
df3 <- read_sav("time2.sav")

time1 <- left_join(df2, df3, by = "ID")

# Question 5
time_2 <- full_join(df2, df3, by = "ID")
