library(rvest)
library(dplyr)

col_link = "https://www.patriotsoftware.com/blog/accounting/average-cost-living-by-state/"
col_page = read_html(col_link)

col_table = col_page %>% html_nodes("table#featuresGrid") %>%
  html_table() %>% .[[1]]


wiki_link = "https://en.wikipedia.org/wiki/List_of_apple_cultivars"
wiki_page = read_html(wiki_link)

apple_table = wiki_page %>% html_nodes("table") %>% .[2] %>% 
  html_table(fill = TRUE) %>% .[[1]]
