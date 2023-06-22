library(tidyverse)
table1
table2
table3
table4a
table4b

# Reshaping data
table4a %>% 
  gather(`1999`, `2000`, key = "year", value = "cases")

table4b %>% 
  gather(`1999`, `2000`, key = "year", value = "population")

table2 %>% 
  spread(key = type, value = count)

table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")

table3 %>% 
  separate(rate, into = c("cases", "population"),
           convert = TRUE)

table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "")

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c( 1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)

stocks %>% 
  complete(year, qtr) # Makes implicit missing value explicit

treatment <- tribble(
  ~ person, ~ treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4)

treatment %>% 
  fill(person)

#Merging data
library(dslabs)
data(murders)
head(murders)
data("polls_us_election_2016")
head(results_us_election_2016)

tab <- left_join(murders, results_us_election_2016, by = "state") %>% select(-others)
head(tab)

tab1 <- slice(murders, 1:6) %>% 
  select(state, population)
tab1

tab2 <- results_us_election_2016 %>% 
  filter(state %in% c("Alabama", "Alaska", "Arizona", "California", "Connecticut", "Delaware")) %>% 
  select(state, electoral_votes)
tab2

left_join(tab_1, tab_2, by = "state")
right_join(tab_1, tab_2, by = "state")
inner_join(tab_1, tab_2, by = "state")
full_join(tab_1, tab_2, by = "state")
