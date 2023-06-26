## Strings
# String Processing

string1 <- "This is a string"
string2 <- 'To put a "quote" inside a string, use single quotes'

x <- c("\"", "\\")
x
writeLines(x)
cat(x)

\n #creates a new line
\t #creates a tab

y <- "\u00b5"
y
?"'" # more special characters

# Regular Expressions
x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.") # period means any character

str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

\d # matches any digit
\s # matches any white space
[abc] # matches a, b, or c
[^abc] # matches anything except a, b, or c (within a brcket ^ means negation)

## Text Mining
install.packages("tidytext")
library(tidytext)

# Jane Austin example
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number()) %>% 
  ungroup()

original_books

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

tidy_books

data("stop_words")
tidy_books <- tidy_books %>% 
  anti_join(stop_words, by = "word")
tidy_books

tidy_books %>% 
  count(word, sort = TRUE)

tidy_books %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 500) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

install.packages("textdata")
library(textdata)
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = TRUE)

install.packages("wordcloud")
library(wordcloud)

tidy_books %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 50))
