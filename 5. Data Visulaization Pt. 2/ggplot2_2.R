## Chapter 8

library(dplyr)
library(ggplot2)

library(dslabs)
data("murders")

murders %>% 
  ggplot() +
  geom_point(aes(population / 10^6, total))

p <- ggplot(murders)

p +
  geom_point(aes(population / 10^6, total), size = 3) +
  geom_text(aes(population / 10^6, total, label = abb), nudge_x = 1)

# global aesthetic mapping

p <- ggplot(murders, aes(population / 10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# Scales 

p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5) +
  scale_x_continuous(trans = log10) +
  scale_y_continuous(trans = log10)

# labels & titles

p + geom_point(size = 3, color = "blue") +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
  
p <- ggplot(murders, aes(population / 10^6, total, label = abb)) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# Annotations, Shapes and adjustments

r <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^6) %>% 
  pull(rate)

p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgray") +
  scale_color_discrete(name = "Region")

print(p)

# Add- on packages
install.packages("ggthemes")
library(ggthemes)
library(ggrepel)

p + theme_economist()

# Quick plots with qplot

x <- log10(murders$population)
y <- murders$total

qplot(x, y)

# Grids of plots
library(gridExtra)

p1 <- murders %>%
  mutate(rate = total/population*10^5) %>%
  filter(population < 2*10^6) %>%
  ggplot(aes(population/10^6, rate, label = abb)) +
  geom_text() +
  ggtitle("Small States")

p2 <- murders %>%
  mutate(rate = total/population*10^5) %>%
  filter(population > 10*10^6) %>%
  ggplot(aes(population/10^6, rate, label = abb)) +
  geom_text() +
  ggtitle("Large States")

grid.arrange(p1, p2, ncol = 2)

## Chapter 9

library(tidyverse)
library(dslabs)
data(heights)
head(heights)

index <- heights$sex == "Male"
x <- heights$height[index]

m <- mean(x)
s <- sd(x)
c(average = m, sd = s)

z <- scale(x)

mean(abs(z) < 2)

# Quantile-quantile plots

pnorm(-1.96)

qnorm(0.975)

qnorm(0.975, mean = 5, sd = 2)

mean(x <= 69.5)

# Example QQ plots
p <- seq(0.05, 0.95, 0.05)
sample_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()


# Example Standard QQ plots
sample_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

# QQplots using ggplot
heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# ggplot2 geometries
# Barplots

murders %>% ggplot(aes(region)) + geom_bar()

data(murders)
tab <- murders %>% 
  count(region) %>% 
  mutate(proportion = n/sum(n))
tab

tab %>% ggplot(aes(region, proportion)) + geom_bar(stat = "identity")

# Histograms

heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1)

heights %>%
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")

# Density plots

heights %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(height)) +
  geom_density(fill = "blue")

heights %>%
  filter(sex == "Female") %>% 
  ggplot(aes(height)) +
  geom_density(fill="blue", adjust = 2)

# QQplots

heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = height)) +
  geom_qq()

# Example 1
params <- heights %>% filter(sex=="Male") %>%
  summarize(mean = mean(height), sd = sd(height))

heights %>% filter(sex == "Male") %>% 
  ggplot(aes(sample = height)) +
  geom_qq(dparams = params) +
  geom_abline()

# Example 2
heights %>%
  filter(sex=="Male") %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# Images
?expand.grid

x <- expand.grid(x = 1:12, y = 1:10) %>%
  mutate(z = 1:120)
x %>% ggplot(aes(x, y, fill = z)) +
  geom_raster() +
  scale_fill_gradientn(colors = terrain.colors(10))

# Quick plots
x <- heights %>%
  filter(sex=="Male") %>%
  pull(height)
qplot(x)

qplot(sample = scale(x)) + geom_abline()

heights %>% qplot(sex, height, data = .)

heights %>% qplot(sex, height, data = ., geom = "boxplot")

qplot(x, geom = "density")

qplot(x, bins = 15, color = I("black"), xlab = "Population")

## Chapter 10
# Case study: new insights on poverty
library(tidyverse)
library(dslabs)
data("gapminder")
gapminder %>% as_tibble()

gapminder %>% 
  filter(year == "2015" & country %in% c("Sri Lanka", "Turkey")) %>% 
  select(country, infant_mortality)

# Scatterplot
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()

# Faceting
gapminder %>% 
  filter(year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

gapminder %>% 
  filter(year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# Facet wrap
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot( aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~ year)

# Time series plots
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year,fertility)) +
  geom_line()

gapminder %>% filter(country %in% countries & ! is.na(fertility)) %>%
  ggplot(aes(year,fertility, group = country)) +
  geom_line()

gapminder %>% filter(country %in% countries & ! is.na(fertility)) %>%
  ggplot(aes(year,fertility, col = country)) +
  geom_line()

# Labels instead of legends 
labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

# Data transformations (Log transformations)
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

gapminder %>% 
  filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

filter(gapminder, year == past_year) %>%
  summarize(min = min(population), max = max(population))

gapminder %>% 
  filter(year == past_year) %>% 
  ggplot(aes(log10(population))) +
  geom_histogram(binwidth = 0.5, color = "black")

# transform the scale instead of values
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

# Comparing multiple distributions
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_point()

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + scale_y_continuous(trans = "log2")

# Boxplot
gapminder <- gapminder %>%
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America", "Australia and New Zealand") ~ "West",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")  
p + geom_point(alpha = 0.5)

# Ridgeplot
install.packages("ggridges")
library(ggridges)
p <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>%
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2")

p + geom_density_ridges()

p + geom_density_ridges(jittered_points = TRUE)

p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(width = 0.05, height = 0),
                        point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7)
