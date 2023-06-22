library(tidyverse)
library(lubridate)

# Question 1
g_day <- as_date("2022-05-21")
diff <- today() - g_day

as.duration(diff)
days(diff)
wday(g_day, label = TRUE)
yday(g_day)

# Question 2
library(haven)
credit <- read_sav("credit_card.sav")

credit <- credit %>% 
  mutate(age_at_issue = round(time_length(as.duration(card_date - dob), "year")))

# Question 2.1
interval <- interval(credit$dob, credit$card_date)
credit$age_at_issue <- round(time_length(interval, "year"))

# Question 3
head(credit)
credit$card <- as_factor(credit$card)

credit$card <- recode(credit$card, `1` = "American Express",
                      `2` = "Visa", 
                      `3` = "Mastercard", 
                      `4` = "Discover",
                      `5` = "Other")
table(credit$card)

# Question 4
credit %>% 
  group_by(card) %>% 
  summarize(money_spent = median(spent)) %>% 
  ggplot(aes(money_spent, fct_reorder(card, money_spent))) +
  geom_point() +
  labs(x = "Money Spent Per Transaction", y = "Credit Card Type") +
  xlim(c(0, 200))
