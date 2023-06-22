library(nycflights13)
library(tidyverse)

# filter
data(flights)
jun12 <- filter(flights, month == 6, day == 12)

# comparison
near(sqrt(2)^2, 2)

# logical operators
filter(flights, month == 11 | month == 12)
filter(flights, month %in% c(11, 12))

# missing values
x <- c(1, NA, 4)
is.na(x)

## Exercises 1
# Question 1
arr_del2 <- filter(flights, arr_delay >= 120)
Houston <- filter(flights, dest == 	"IAH" | dest == "HOU")
U_A_D <- filter(flights, carrier %in% c("US", "AA", "DL"))
summer <- filter(flights, month %in% c(7, 8, 9))
not_leaving_late <- filter(flights, arr_delay >= 120 & dep_delay <= 0)
thirty_minutes <- filter(flights, dep_delay >= 60, arr_delay <= 30)
midnight <- filter(flights, dep_time >= 0 & dep_time <= 600)

# Question 2
midnight <- filter(flights, between(dep_time, 0, 600))

# Question 3
flights %>% count(dep_time == "NA")

# Arrange rows with arrange
## Exercises 2
# Question 1
no_departure <- arrange(flights, desc(is.na(dep_time)))

# Question 2
most_delayed <- arrange(flights, dep_delay)

# Question 3
fastest_flight <- arrange(flights, air_time)

# Question 4
shortest_flight <- arrange(flights, distance)
longest_flight <- arrange(flights, desc(distance))

# Select Columns with select()
# Exercises 3
# Question 1
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, contains("dep"), contains("arr"))

# Question 2
select(flights, dep_time, dep_time) # It still returns that same value

# Question 3
vars <- c(
  "year", "month", "day", "dep_delay", "arr_delay"
)
select(flights, one_of("day")) # didn't solve this one

# Question 4
select(flights, contains("TIME"))

# Add new variables with mutate()
flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time
)
mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

mutate(flights_sml, 
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)

transmute(flights, #adds new variable, but deletes all the older ones
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
          )

# Useful creation functions
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

(x <- 1:10)
lag(x)
lead(x)

(x - lag(x))
(x != lag(x))

# Exercise 4
# Question 1
flights <- mutate(flights, 
       dep_time_new = (dep_time %/% 100) * 60 + dep_time %% 100,
       sched_dep_time_new = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100,
       arr_time_new = (arr_time %/% 100) * 60 + arr_time %% 100,
       sched_arr_time_new = (sched_arr_time %/% 100) * 60 + sched_arr_time %% 100)

# Question 2
flights <- mutate(flights, 
       air_time_new = (arr_time_new - dep_time_new))
select(flights, air_time_new, air_time) # not correct

# Question 3
select(flights, dep_time_new, sched_dep_time_new, dep_delay)
select(flights, arr_time_new, sched_arr_time_new, arr_delay)

# Question 4
# research how to solve this

# Grouped Summaries with summarize
by_day <- group_by(flights, year, month, day)
summarize(by_day, delay = mean(dep_delay, na.rm = TRUE))

delays <- flights %>% 
  group_by(dest) %>% 
  summarize(count = n(),
            dist = mean(distance, na.rm = TRUE),
            delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(delay = mean(dep_delay))

# Counts
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarize(
    delay = mean(arr_delay),
    count = n()
      )

ggplot(data = delays, mapping = aes(x = count, y = delay)) + 
  geom_point(alpha = 1/10)

delays %>% 
  filter(count > 25) %>% 
  ggplot(mapping = aes(x = count, y = delay)) + 
  geom_point(alpha = 1/10)

# Counts ex 2
install.packages("Lahman")

batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

batters %>% 
  arrange(desc(ba))

# Useful summary functions
not_cancelled %>%
  group_by(year, month, day) %>%
  summarize(
    avg_delay1 = mean(arr_delay), # average delay
    avg_delay2 = mean(arr_delay[arr_delay > 0])) # average positive delay:

not_cancelled %>% 
  group_by(dest) %>% 
  summarize(
    distance_sd = sd(distance)
  ) %>% 
  arrange(desc(distance_sd))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(
    first = first(dep_time),
    last = last(dep_time)
  )

not_cancelled %>%
  group_by(year, month, day) %>%
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>% 
  count(dest)

not_cancelled %>% 
  count(tailnum, wt = distance)

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarize(n_early = sum(dep_time < 500))

# Exercises 5

# Grouped Mutates & Filters
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)

# Exercises 6

