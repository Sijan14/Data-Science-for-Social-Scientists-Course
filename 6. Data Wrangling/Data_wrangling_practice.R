## Chapter 10 (Tidy data with tidyr)
library(tidyverse)

# Gather
tidy4a <- table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)

#  Spread
spread(table2, key = "type", value = "count")

# Separating and Pull
table3 %>% 
  separate(rate, into = c("cases", "popultaion"))

table3 %>% 
  separate(rate, into = c("cases", "popultaion"), sep = "/")

table3 %>% 
  separate(rate, into = c("cases", "popultaion"), convert = TRUE)

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2)

# Unite
table5 %>% 
  unite(new, century, year)

table5 %>% 
  unite(new, century, year, sep = "")

# Missing Values
stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c( 1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)

stocks
stocks %>% 
  spread(year, return)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>% 
  complete(year, qtr)

treatment <- tribble(
  ~ person, ~ treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
)
treatment %>% 
  fill(person)

# Case Study
data(who)

who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, 
         key = "key", 
         value = "cases",
         na.rm = TRUE)
who1 %>% 
  count(key)

who2 <- who1 %>% 
  mutate(key = str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)
who4

who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5

# Case Study (all at once)
who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = stringr::str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

## Chapter 11(Relational data with dplyr)
library(nycflights13)

# Keys
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)

flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)

# Mutating Joins
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2 %>% 
  select(-origin, -dest) %>% 
  left_join(airlines, by = "carrier")

# Understanding Joins
x <- tribble(
  ~key, ~val_x,
  1, "x1", 
  2, "x2", 
  3, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1", 
  2, "y2", 
  4, "y3"
)
# Inner Join
x %>% 
  inner_join(y, by = "key")

# Outer Join
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2")
left_join(x, y, by = "key")

flights2 %>% 
  left_join(weather)

flights2 %>% 
  left_join(planes, by = "tailnum")

flights2 %>% 
  left_join(airports, c("dest" = "faa"))

flights2 %>% 
  left_join(airports, c("origin" = "faa"))

# Filtering Joins
top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)
top_dest

flights %>%
  filter(dest %in% top_dest$dest)

flights %>% 
  semi_join(top_dest)

flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

# Set Operations
df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)
df2 <- tribble(
  ~x, ~y,
  1, 1,
  1, 2
)

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

## Irizarry: Chapter 22 (Reshaping Data)
library(dslabs)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# Gather
wide_data
new_tidy_data <- wide_data %>% gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

new_tidy_data <- wide_data %>% 
  gather(year, fertility, -country)
head(new_tidy_data)

class(new_tidy_data$year)

new_tidy_data <- wide_data %>%  
  gather(year, fertility, -country, convert = TRUE) # to convert to integer
class(new_tidy_data$year)

new_tidy_data %>% ggplot(aes(year, fertility, color = country)) + geom_point()

# Separate
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% 
  gather(key, value, -country)
head(dat)

dat %>% 
  separate(key, c("year", "variable"), "_")

dat %>% separate(key, c("year", "variable_1", "variable_2"),fill = "right")

dat %>% separate(key, c("year", "variable_name"), extra = "merge")

dat %>% 
  separate(key, c("year", "variable_name"), extra = "merge") %>% 
  spread(variable_name, value)

# Unite
head(dat)
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>% 
  unite(variable_name, first_variable_name, second_variable_name, sep = "_") %>% 
  spread(variable_name, value) %>% 
  rename(fertility = fertility_NA)

## Irizarry: Chapter 22 (Joining tables)
data("murders")
head(murders)

data("polls_us_election_2016")
head(polls_us_election_2016)

# Joins
tab <- left_join(murders, results_us_election_2016, by = "state") %>% 
  select(-others)
head(tab)

library(ggrepel)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

tab_1 <- slice(murders, 1:6) %>% 
  select(state, population)
tab_1

tab_2 <- results_us_election_2016 %>%
  filter(state%in%c("Alabama", "Alaska", "Arizona",
                    "California", "Connecticut", "Delaware")) %>%
  select(state, electoral_votes)
tab_2              
  