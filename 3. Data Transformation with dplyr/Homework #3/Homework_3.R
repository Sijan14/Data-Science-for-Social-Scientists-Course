library(tidyverse)
library(haven)

# Question 1
hw3 <- read_sav("hw3_data.sav")

attributes(hw3$education)

# Question 2
hw3 <- hw3 %>% 
  mutate(education_recoded = ifelse(education <= 4, 0, 1))

hw3 %>% group_by(education_recoded) %>% count()

# Question 3a
hw3 <- hw3 %>% 
  mutate(opt_no_missing = rowMeans(hw3[,6:11])) 

hw3 %>% 
  summarize(average = mean(opt_no_missing,  na.rm = TRUE), standard_deviation = sd(opt_no_missing,  na.rm = TRUE), count = sum(!is.na(opt_no_missing)))


# Question 3b
hw3 <- hw3 %>% 
  mutate(opt_missing_ignored = rowMeans(hw3[,6:11], na.rm = TRUE)) 

hw3 %>% 
  summarize(average = mean(opt_missing_ignored,  na.rm = TRUE), standard_deviation = sd(opt_missing_ignored,  na.rm = TRUE), count = n())

# describe(hw3$opt_missing_ignored) <- much easier to get descriptive analysis

# Question 3c
hw3 <- hw3 %>% 
  mutate(opt_no_extreme_missing = ifelse(is.na(op1) + is.na(op2) + is.na(op3) + is.na(op4) + is.na(op5) + is.na(op6) >= 4, NA, rowMeans(hw3[,6:11], na.rm = TRUE)))

hw3 %>% 
  summarize(count = sum(!is.na(opt_no_extreme_missing)))

# Question 4
hw3[hw3$ID == 215, "age"] <- 32

# Question 5
hw3 <- hw3 %>% 
  rename_all(toupper)

# could have also used rename_with(df, toupper)

# Question 6
write_csv(hw3, "hw3.csv")

# Question 7
df <- read_csv("race_recoding.csv")

df$race[df$AIAN == 1] <- 1
df$race[df$Asian == 1] <- 2
df$race[df$AfricanAmerican == 1] <- 3
df$race[df$NHPI == 1] <- 4
df$race[df$White == 1] <- 5

# Could have also used case_when()

df %>% group_by(race) %>% count()

# could have also used table(race_recoding$race)