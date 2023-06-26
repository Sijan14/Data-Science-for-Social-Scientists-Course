library(tidyverse)

# Question 1
data("gss_cat")

count(gss_cat, marital)
gss_cat <- gss_cat %>% 
  mutate(marital_recoded = fct_collapse(marital,
                                        "married" = c("Married"),
                                        "not married" = c("No answer", "Never married", "Separated", "Divorced", "Widowed")
                                        ))
count(gss_cat, marital_recoded)

# Question 2
library(haven)
gss_cat$year <- as_factor(gss_cat$year)
gss_cat

count(gss_cat, year)
gss_cat <- gss_cat %>% 
  mutate(year_recoded = fct_collapse(year, 
                                     "2000s" = c("2000","2002","2004","2006", "2008"),
                                     "2010s" = c("2010", "2012", "2014")))
count(gss_cat, year_recoded)
# Question 3
mar_age <- gss_cat %>% 
  group_by(marital) %>% 
  summarize(mean_age = mean(age, na.rm = TRUE))

ggplot(mar_age, 
       aes(mean_age, fct_reorder(marital, mean_age))) +
  geom_point()

# Question 4
library(lubridate)

ds_journey <- as_datetime("2023-01-18 20:15:00 EST")

# Question 5
as.duration(now() - ds_journey)
