library(tidyverse)
library(haven)

# Question 1
contacts <- read_sav("contacts.sav")

# Question 2
attributes(contacts$dept)
contacts$dept_recoded[contacts$dept == 1] <- 0
contacts$dept_recoded[contacts$dept == 3] <- 0
contacts$dept_recoded[contacts$dept == 2] <- 1
contacts$dept_recoded[contacts$dept == 4] <- NA
contacts$dept_recoded[contacts$dept == 9] <- NA

contacts %>% group_by(contacts$dept_recoded) %>% count()

# Question 3
attributes(contacts$rank)
class(contacts$rank)

contacts$rank_recoded <- contacts$rank %>%
  as.numeric() %>% 
  recode(`1` = 0,  `2` = 1, `3` = 1, `4` = 2, `5` = 2)
  
contacts %>% group_by(contacts$rank_recoded) %>% count()

# Question 4

contacts <- contacts %>%
  mutate(sales_recoded = ifelse(sale >= 65, 1, 0))

contacts %>% group_by(contacts$sales_recoded) %>% count()  

# Question  5
id <- c(1,2,3,4,5)
item1 <- c(4,2,3,4,5)
item2 <- c(5,4,2,4,3)
item3 <- c(4,3,2,NA,4)

swl <- data.frame(id, item1, item2, item3)

# Question 6
help("rowMeans")
swl$swl_mean <- rowMeans(swl[2:4], na.rm = TRUE)

# Question 7
swl <- arrange(swl, swl_mean)

# Question 8
swl[swl$id == 5, c("swl_mean")]


