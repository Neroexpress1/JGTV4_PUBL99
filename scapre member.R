library(xml2)
library(rvest)
library(tidyverse)


# Specify the URL of the Wikipedia page you're interested in
url <- "https://hris.parliament.go.th/ss_th.php"

# Read the HTML content of the page
page <- read_html(url)
names <- page %>% html_nodes('ul.user_list li h4:first-of-type') %>% html_text()
names_no_quotes <- gsub('["\']', '', names)

parties <- page %>%
  html_nodes('ul.user_list li h4:last-of-type') %>%
  html_text()

mp_data <- data.frame(names = names_no_quotes, parties = parties, stringsAsFactors = FALSE)

save(mp_data, file = "mp_data.RData")

#########################################################
#extract from wiki
library(rvest)
library(dplyr)

# Extract from wiki
url_25 <- "https://th.wikipedia.org/wiki/%E0%B8%AA%E0%B8%A0%E0%B8%B2%E0%B8%9C%E0%B8%B9%E0%B9%89%E0%B9%81%E0%B8%97%E0%B8%99%E0%B8%A3%E0%B8%B2%E0%B8%A9%E0%B8%8E%E0%B8%A3%E0%B9%84%E0%B8%97%E0%B8%A2_%E0%B8%8A%E0%B8%B8%E0%B8%94%E0%B8%97%E0%B8%B5%E0%B9%88_25"

html <- read_html(url_25)


members_list <- html %>%
  html_node("h2 + p + table") %>%
  html_table()
names <- members_table[[1]]$Name  # Adjust column names as per actual content
parties <- members_table[[1]]$Party  # Adjust column names as per actual content

# Combine into a new data frame
extracted_info <- data.frame(Name = names, Party = parties)

print(extracted_info)
# Extract names and parties for those elected from areas
names_areas <- html %>% html_nodes('tr td:nth-child(3) a, tr td:nth-child(1) a') %>% html_text()
parties_areas <- html %>% html_nodes('tr td:nth-child(4) a, tr td:nth-child(3) a:last-child') %>% html_text()

# Create a dataframe for area-elected members
df_areas <- data.frame(Name = names_areas, Party = parties_areas)

# Find common parties
common_parties <- intersect(df_party_list$Party, df_areas$Party)

# Filter both data frames to only include rows with parties that are common between them
filtered_df_party_list <- df_party_list %>% filter(Party %in% common_parties)
filtered_df_areas <- df_areas %>% filter(Party %in% common_parties)

# Optionally, combine the filtered data frames if needed
combined_filtered_df <- bind_rows(filtered_df_party_list, filtered_df_areas)

# Print the filtered data frames
print(filtered_df_party_list)
print(filtered_df_areas)

# Print the combined filtered data frame
print(combined_filtered_df)

