library(tidyverse)

ggplot(mpg) +
  geom_point(aes(displ, hwy), color = "blue")

library(dslabs)
data("heights")

heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# Faceting
ggplot(mpg) +
  geom_point(aes(displ, hwy)) + 
  facet_wrap(~ class, nrow = 2)

data("gapminder")
filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# Layering (General Aesthetics)
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_smooth()

# Time series plots
gapminder %>% 
  filter(country == "Bangladesh") %>% 
  ggplot(aes(year, fertility)) +
  geom_line()

# Statistical transformations
ggplot(diamonds) +
  geom_bar(aes(cut))

ggplot(diamonds) +
  stat_summary(
    aes(cut, depth),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median
  )

# Putting it all together
library(ggthemes)
library(ggrepel)

