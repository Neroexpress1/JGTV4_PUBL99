library(xml2)
library(rvest)
library(tidyverse)



url <- "https://hris.parliament.go.th/ss_th.php"

page <- read_html(url)
names <- page %>% html_nodes('ul.user_list li h4:first-of-type') %>% html_text()
names_no_quotes <- gsub('["\']', '', names)

parties <- page %>%
  html_nodes('ul.user_list li h4:last-of-type') %>%
  html_text()

mp_data <- data.frame(names = names_no_quotes, parties = parties, stringsAsFactors = FALSE)

save(mp_data, file = "mp_data.RData")


