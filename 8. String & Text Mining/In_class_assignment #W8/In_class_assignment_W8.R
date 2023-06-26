# Question 1
library(tidyverse)
library(stringr)

string1 <- "Data Science is the best class ever"
string2 <- "and it is not even up for debate!"

combined_string <- str_c(string1, ", ", string2)
combined_string

# Question 2
combined_string <- combined_string %>% 
  str_replace("!", "!!!") %>% 
  str_to_upper()

combined_string %>% 
  str_count()

combined_string %>% 
  str_count("[AEIOU]")

# Question 3
library(janeaustenr)
data("stop_words")

pride_and_pred <- austen_books() %>% 
  filter(book == "Pride & Prejudice") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

pride_and_pred %>% 
  count(word, sort = TRUE)

# Question 4
install.packages("gutenbergr")
library(gutenbergr)

time_machine <- gutenberg_download(35)

time_machine <- time_machine %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

time_machine %>% 
  count(word, sort = TRUE)

# Question 5
bing <- get_sentiments("bing")

bing_positive <- bing %>% 
  filter(sentiment == "positive")

bing_negative <- bing %>% 
  filter(sentiment == "negative")

time_machine %>% 
  inner_join(bing_positive) %>% 
  count(word, sort = TRUE)

time_machine %>% 
  inner_join(bing_negative) %>% 
  count(word, sort = TRUE)
