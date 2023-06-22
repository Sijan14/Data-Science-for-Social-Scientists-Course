# Question 1
library(tidyverse)

df <- read_csv("hw5_wide.csv")

df <- df %>% 
  rename("time1" = "pre", "time2" = "post1", "time3" = "post2")

# Question 2
long_df <- df %>% 
  gather(time, SWL, time1:time3)

# Question 3
long_df <- long_df %>% 
  arrange(id)

# Question 4
long_df %>% 
  group_by(condition, time) %>% 
  summarize(mean(SWL))

# Question 5
library(readxl)

w1 <- read_excel("hw5_join.xlsx", sheet = 1)
w2 <- read_excel("hw5_join.xlsx", sheet = 2)

# Question 6
w2$id2 <- w2$id2 - 1000

# Question 7
df1 <- w1 %>% 
  semi_join(w2, by = c("id" = "id2"))

df2 <- w1 %>% 
  anti_join(w2, by = c("id" = "id2"))

# Question 8
df1 %>% 
  summarize("N (Sample size)" = n(),
            "Mean age" = mean(age),
            "Mean social support" = mean(social_support),
            "Mean ability" = mean(ability)) 

df2 %>% 
  summarize("N (Sample size)" = n(),
            "Mean age" = mean(age),
            "Mean social support" = mean(social_support),
            "Mean ability" = mean(ability)) 
