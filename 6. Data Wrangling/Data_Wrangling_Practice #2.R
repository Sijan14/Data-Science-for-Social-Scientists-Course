# Chapter 11 (Strings with stringr)

library(tidyverse)
library(stringr)

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'

double_quote <- "\""
single_quote <- '\''
writeLines(double_quote) # to see the raw contents of the string

c("one", "two", "three") # multiple strings in a vector

str_length(c("a", "R for data science", NA))

str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a","b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
  )

str_c(c("x", "y", "z"), collapse = ", ")

# subsetting strings
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))

# Locals
str_to_upper(c("i", "ı"))
str_to_upper(c("i", "ı"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = "haw")

# Matching Patterns with regular expressions
x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x,".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

# Anchors
x <- c("apple", "banana", "pear")

str_view(x, "^a") # ^ to match the start of the string
str_view(x, "a$") # $ to match the end of the string

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$") # to match a complete string

# Character Classes & Alternatives
# \d matches any digit
# \s matches any whitespace
# [abc] matches a, b, or c
# [^abc] matches anything except a, b, or c

str_view(c("grey", "gray"), "gr(e|a)y")

# Repititions
# ?: 0 or 1
# +: 1 or more
# *: 0 or more

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, 'C[LX]+')

str_view(x, "C{2}") # {n}: exactly n
str_view(x, "C{2,}") # {n,}: n or more
str_view(x, "C{1,1}") # {n, m}: between n and m

str_view(x, 'C{2,3}?') # shortest string possible

# Grouping and Backreferences

# Detect Matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words,
  i = seq_along(word)
)
df %>%
  filter(str_detect(words, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

# Extract Matches






## Chapter 12: Factors with forcats
library(forcats)

# Creating factors
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

y2 <- factor(x2, levels = month_levels)
y2
y2 <- parse_factor(x2, levels = month_levels)

factor(x1) # omit the levels and it will be taken from data in alphabetical order

f1 <- factor(x1, levels = unique(x1)) # match the first appearance
f1

f2 <- x1 %>% factor() %>% fct_inorder() # match the first appearance
f2

# General Social Survey
gss_cat

gss_cat %>% 
  count(race) # see the levels of a factor

ggplot(gss_cat, aes(race)) +
  geom_bar() # see the levels of a factor

ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE) # to also see level that don't have any values

# Modifying Factor Order
relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig, aes(tvhours, relig)) +
  geom_point()

ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) + # ordering with fct_reorder #1
  geom_point()

relig %>% 
  mutate(relig = fct_reorder(relig, tvhours)) %>% # ordering with fct_reorder #2
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome <- gss_cat %>%
  group_by(rincome) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(
  rincome,
  aes(age, fct_reorder(rincome, age))
) + geom_point()

ggplot(rincome, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% # fct_infreq() to order levels in increasing frequency
  ggplot(aes(marital)) +
  geom_bar()

# Modifying Factor Levels
gss_cat %>% count(partyid)

gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat",
                              "other" = "No answer",
                              "other" = "Don't know",
                              "other" = "Other party"
                              )) %>% 
  count(partyid)

gss_cat %>% 
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
                                )) %>% 
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>% 
  print(n = Inf)

# Chapter 13: Dates and Times with lubridate
library(lubridate)

today()
now()

# Create date-time from Strings
ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31 Jan 2017")

ymd(20170131) # without a quote

ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")
ymd(20170131, tz = "UTC")

flights %>% 
  select(year, month, day, hour, minute)

flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(
      year, month, day, sched_dep_time
    ),
    sched_arr_time = make_datetime_100(
      year, month, day, sched_arr_time
    )
  ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 600)

# Create date-time from Other Types
as_datetime(today())
as_date(now())

as_datetime(60 * 60 * 10) # starts counting from 1970-01-01
as_date(365 * 10 + 2)

# Date-Time Components
datetime <- ymd_hms("2016-07-08 12:34:56")

year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)

month(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = FALSE)

flights_dt %>% 
  mutate(wday = wday(dep_time, label= TRUE)) %>% 
  ggplot(aes(wday)) +
  geom_bar()

flights_dt %>% 
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n()) %>%
  ggplot(aes(minute, avg_delay)) +
  geom_line()

sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n = n())
ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()

ggplot(sched_dep, aes(minute, n)) +
  geom_line()

# Rounding
?floor_date
x <- ymd_hms("2009-08-03 12:01:59.23")
floor_date(x, "minute")
floor_date(x, "year")

round_date(x, "minute")
round_date(x, "week")
# Setting components
(datetime <- ymd_hms("2016-07-08 12:34:56"))

year(datetime) <- 2020
month(datetime) <- 01
hour(datetime) <- hour(datetime) + 1
datetime

update(datetime, year = 2020, month = 2, mday = 2, hour = 2)

# Time Spans
h_age <- today() - ymd(19791014)
h_age

# Durations
as.duration(h_age)

dseconds(14)
dminutes(10)
dhours(c(10, 14))
ddays(0:5)
dweeks(3)
dyears(1)

2 * dyears(1) # multiply
dyears(1) + dweeks(12) + dhours(15) # ADD 

tomorrow <- today() + ddays(1) # Add and subtract
last_year <- today() - dyears(1)

# Periods
seconds(15)
minutes(10)
hours(c(12, 24))
days(7)
months(1:6)
weeks(3)
years(1)

10 * (months(6) + days(1))
days(50) + hours(25) + minutes(2)

# Intervals

# Time Zones
Sys.timezone()
head(OlsonNames())
