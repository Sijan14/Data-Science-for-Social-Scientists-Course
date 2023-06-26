library(tidyverse)
library(rvest)

url <- paste0("https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm")
h <- read_html(url)

html_text(h)

nodes <- h %>% html_nodes("table")
html_text(nodes[[8]])

tab <- nodes[[8]] %>% html_table
class(tab)

tab1 <- nodes[[3]] %>% html_table
tab2 <- nodes[[4]] %>% html_table

tab3 <- nodes[[-3]] %>% html_table
