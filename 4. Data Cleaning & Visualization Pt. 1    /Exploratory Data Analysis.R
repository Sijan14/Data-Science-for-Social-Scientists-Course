# Visualizing Distributions
head(diamonds)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% 
  count(cut)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% 
  count(cut_width(carat, 0.5))

view(diamonds)
names(diamonds)

smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

# unusual values
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  arrange(y)
unusual

# Exercise 1
ggplot(diamonds) +
  geom_histogram(mapping = aes(x), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(y), binwidth = 1)

ggplot(diamonds) +
  geom_histogram(mapping = aes(z), binwidth = 1)

ggplot(diamonds) +
  geom_histogram(mapping = aes(price), binwidth = 5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.05)

diamonds %>% 
  filter(carat == 0.99) %>% 
  count()

diamonds %>% 
  filter(carat == 1) %>% 
  count()

# Missing Values
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) # if you put y1 then it would have added a new column, but it just replaces the current y
diamonds2

ggplot(data = diamonds2, mapping = aes(x, y)) +
  geom_point(na.rm = TRUE)

# Covariation

# A categorical and a continuous variable
ggplot(data = diamonds, 
       mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds,mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) + 
  coord_flip()

# Exercise 2 (Do this one)

# Two Categorical Variables
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut)

diamonds %>% 
  count(color, cut) %>% 
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

# Exercise 3 (Do this one as well)

# Two Continuous variables
ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

install.packages("hexbin")
library(hexbin)

ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# Exercise 4 (Do this one too)

# Patterns and Models
library(modelr)

mod <-lm(log(price) ~ log(carat), data = diamonds)               

diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(diamonds2) + 
  geom_point(aes(carat, resid))

ggplot(diamonds2) + 
  geom_boxplot(aes(cut, resid))

