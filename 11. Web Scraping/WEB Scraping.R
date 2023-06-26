library(tidyverse)
library(rvest)

url <- paste0("https://en.wikipedia.org/w/index.php?title=","Gun_violence_in_the_United_States_by_state&oldid=843135608")

h <- read_html(url)

tab <- h %>% html_nodes("table")
tab

tab <- tab[[2]] %>% html_table
head(tab)

webpage <- read_html("https://www.geeksforgeeks.org/data-structures-in-r-programming/")

heading = html_node(webpage, 'h1')

text = html_text(heading)
print(text)

paragraph = html_nodes(webpage, 'p')

pText = html_text(paragraph)

print(head(pText))

# JSON Example
library(jsonlite)

citibike <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json") 
stations <- citibike$data$stations 
colnames(stations) 

stations$name

stations <- citibike$data$stations %>% as_tibble()

