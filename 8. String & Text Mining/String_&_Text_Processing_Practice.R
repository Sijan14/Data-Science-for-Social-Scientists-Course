library(tidyverse)
library(stringr)

## Case study 1
library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167")
murders_raw <- read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[2]] %>%
  setNames(c("state", "population", "total", "murder_rate")) # doesn't work

library(dslabs)
data("murders")

murders_raw$population[1:3]

## Case study 2
data("reported_heights")
class(reported_heights$height)

x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

reported_heights %>% 
  mutate(new_heights = as.numeric(height)) %>% 
  filter(is.na(new_heights)) %>% 
  head(10)

# creating a function for fucked up heights data
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

problems <- reported_heights %>% 
  filter(not_inches(height)) %>% 
  pull(height)

length(problems)

print(problems)

# 25.4. Escaping charaacters

s <- "5\""
cat(s)

s <- '5\'10"' # first way (put backlash before the same kind of quotes)
cat(s)

s <- "5'10\"" # second way
cat(s)

# 25.5. Regular Expressions
pattern <- ","
str_detect(murders$total, pattern)
str_subset(reported_heights$height, "cm")

yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches") ## Special characters
str_detect(s, "cm|inches") # better and easier

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)

str_detect(s, "\\d")
str_view(s, "\\d")
str_view_all(s, "\\d")

str_view(s, "[56]") ## Character classes

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)

str_detect(s, "[4-7]")

yes <- c("1", "5", "9") ## Anchoring
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
pattern <- "^\\d$"

str_view_all(s, pattern)

yes <- c("1", "5", "9", "12") ## Quantifiers
no <- c("123", "a4", "b")
pattern <- "^\\d{1,2}$"

str_view_all(c(yes, no), pattern)

yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
pattern <- "^[4-7]'\\d{1,2}\"$"

str_view(c(yes, no), pattern)
str_detect(yes, pattern)
str_detect(no, pattern)

pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

yes <-  c("AB", "A1B", "A11B", "A111B", "A1111B")
x <- data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           none_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))
x

yes <- c(".3", "+2", "-0","*4") # Not (using \\D, \\S)
no <- c("A3", "B2", "C0", "E4")
pattern <- "[^a-zA-Z]\\d"

str_detect(yes, pattern)
str_detect(no, pattern)

pattern2 <- "\\D"
pattern3 <- "\\S"

str_detect(yes, pattern2)

pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$"

yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

pattern <- "^[4-7]'\\d{1,2}\"$" # Search and replace with regex
sum(str_detect(problems, pattern))

problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)

str_subset(problems, "inches")
str_subset(problems, "''")

pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot","'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_detect(pattern) %>% 
  sum()

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$" #pattern improved with space
problems %>% 
  str_replace("feet|ft|foot","'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_detect(pattern) %>% 
  sum()

pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
str_subset(problems, pattern_with_groups) %>% head()

str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>% head()

not_inches_or_cm <- function(x, smallest = 50, tallest = 84){ # Testing and Improving
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>% 
  pull(height)  
length(problems)

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>% # convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>% # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]

str_trim(" 5 ' 9 ")

s <- c("Five feet eight inches") # changing lettercase
str_to_lower(s)
str_to_upper(s)
str_to_title(s)

# Case study 2: self reported heights (continued)
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>% #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

words_to_numbers <- function(s){
  str_to_lower(s) %>%
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

converted <- problems %>% words_to_numbers() %>% convert_format()
remaining_problems <- converted[not_inches_or_cm((converted))]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# 25.10.1 The extract function
s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep = "'")

library(tidyr)
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

s <- c("5'10", "6'1", "5'8inches")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# Putting it all together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
  mutate(original = height, height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54,#meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

new_heights %>% 
  filter(not_inches(original)) %>% 
  select(original, height) %>% 
  arrange(height) %>% 
  view()
new_heights %>% arrange(height) %>% head(n=7)




