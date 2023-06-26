# Question 1
library(tidyverse)
library(stringr)

string <- "This is the first statement.\n\nThis is the second statement."
cat(string)

# Question 2
df <- read_csv("names_data.csv")

df <- df %>% 
  mutate(full_name = str_c(`last name`, `first name`, sep = ","))

# Question 3

N <- str_count(df$state, "^N") %>% 
  sum()

N/sum(str_count(df$state, "^[A-Z]"))

# Question 4
df$phone %>% 
  str_count("[7-9]$") %>% 
  sum()

# Question 5
library(janeaustenr)
library(tidytext)
data("stop_words")
library(forcats)

df1 <- austen_books() %>% 
  filter(book != "Emma") %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

df1 %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 400) %>% 
  mutate(word = fct_reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  xlab("Frequency Count") +
  ylab("Most Frequent Words") +
  ggtitle("Text Mining Jane Austen (Minus Emma)")

# Question 6
library(psych)

sen <- data_frame(sentences)

sen <- sen %>% 
  mutate(char_count = str_count(sentences))

describe(sen$char_count)

# Question 7
library(gutenbergr)

tgg <- gutenberg_download(64317)

tgg <- tgg %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

bing <- get_sentiments("bing")

tgg <- tgg %>% 
  inner_join(bing, by = "word")

count <- tgg %>% 
  count(sentiment)

pie(count$n, labels = count$sentiment, main = "Great Gatsby Sentiments")
