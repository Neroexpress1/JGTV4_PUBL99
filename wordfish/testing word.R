#load umbrella package
library(tidyverse)
library(reshape2)
library(stringdist)
#load visual
library(ggplot2)
#load text  packages
library(quanteda) 
library(quanteda.textmodels)
library(quanteda.textstats)

#load topic modeling packages
library(topicmodels)

#import our segment speech for each year
year2564_03 <- read.csv("segmented_data_03_year2564.csv", encoding = "UTF-8")
year2564_031 <- read.csv("segmented_data_031_year2564_02.csv", encoding = "UTF-8")
year2564_032 <- read.csv("segmented_data_032_year2564_02.csv", encoding = "UTF-8")

parliement_speech_withwordsegmeted <- bind_rows(year2564_03, year2564_031, year2564_032)

merged_speech_data <- parliement_speech_withwordsegmeted %>%
  group_by(Speaker) %>%
  summarise(MergedWords = list(unique(unlist(segmented_speech)))) %>%
  ungroup()

# Extract speaker_name and position
merged_speech_data <- merged_speech_data %>%
  mutate(
    speaker_name = str_extract(Speaker, "^[^(]+"),
    position = str_extract(Speaker, "\\(.*?\\)|สมาชิกวุฒิสภา|สมาชิกสภาผู้แทนราษฎร"),
    speaker_name = str_trim(speaker_name),
    position = str_replace_all(position, "[()]", "") %>% str_replace_all("\\s+", ""),
    position = case_when(
      str_detect(position, "สมาชกสภาผู้แทนราษฎร|สมาชิกสภาผู้แทนราษฎร") ~ "สมาชิกสภาผู้แทนราษฎร",
      str_detect(position, "สมาชิกรัฐสภา") ~ "สมาชิกวุฒิสภา",
      TRUE ~ position 
    )
  )

#remove their position/title from their name
merged_speech_data$speaker_name <-  gsub("\\s*สมาชิกวุฒิสภา\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <-  gsub("\\s*สมาชิกสภาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <-  gsub("\\s*สมาชิกสภาสภาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*นาง\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*นางสาว\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*นาย\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*สมาชิกสาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*สมาชกสภาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*สมาชิกสภาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*สมาชิกสภาแทนราษฎร\\s*", "", merged_speech_data$speaker_name)
merged_speech_data$speaker_name <- gsub("\\s*สมาชิกสาผู้แทนราษฎร\\s*", "", merged_speech_data$speaker_name)

###########################################################################################################
#This piece of code must be custom adjusted for each year of the meeting since there might be some error that passed from our speech split's code
merged_speech_data[ c( 43, 57, 58, 74) ,  "position"] <- "สมาชิกสภาผู้แทนราษฎร"
#merged_speech_data[c(133,218), "position"] <- "สมาชิกวุฒิสภา"
merged_speech_data <- merged_speech_data[-c( 6, 28), ]
#merged_speech_data$position[merged_speech_data$speaker_name == "ศักดิ์สยาม ชิดชอบ"] <- "สมาชิกสภาผู้แทนราษฎร"
#merged_speech_data <- merged_speech_data[merged_speech_data$speaker_name != "ชวน หลีกภัย" | is.na(merged_speech_data$speaker_name), ]
#merged_speech_data <- merged_speech_data[merged_speech_data$speaker_name != "พรเพชร วิชิตชลชัย" | is.na(merged_speech_data$speaker_name), ]
###########################################################################################################

# read the member name and their party
member_associated_data <- read.csv("member of parliemnt 25 (02_2563-2566).csv", encoding = "UTF-8", stringsAsFactors = FALSE)

# collapse their name to matching code
collapse_name <- function(name) {
  collapsed <- str_replace_all(name, "[^a-zA-Z0-9ก-๙]", "")
  tolower(collapsed)
}
member_associated_data$speakername <- sapply(member_associated_data$speakername, collapse_name) 
merged_speech_data$speaker_name <- sapply(merged_speech_data$speaker_name, collapse_name)

merged_speech_data$speaker_name <- tolower(gsub("\\s+", "", merged_speech_data$speaker_name))
member_associated_data$speakername <- tolower(gsub("\\s+", "", member_associated_data$speakername))

#Assign thier party based on thier name
merged_speech_data <- left_join(merged_speech_data, member_associated_data, by = c("speaker_name" = "speakername"))

merged_speech_data <- rename(merged_speech_data, party = party)

#assign non-party to member of upper house
merged_speech_data <- merged_speech_data %>%
  mutate(party = ifelse(position == "สมาชิกวุฒิสภา", "non-party", party))

merged_speech_data <- merged_speech_data %>%
  mutate(speaker_name = gsub("สมาชิกสภาผู้แทนราษฎร", "", speaker_name))

# For those who are not match by list of member due to spelling mistake. we use fuzzy matching
fuzzy_match_indices <- which(is.na(merged_speech_data$party))
if (length(fuzzy_match_indices) > 0) {
  closest_names <- stringdist::amatch(merged_speech_data$speaker_name[fuzzy_match_indices], member_associated_data$speakername, maxDist = 20)
  merged_speech_data$party[fuzzy_match_indices] <- member_associated_data$party[closest_names]
}

#upload our stop word and common words that doesn't represent any meaningful 

thai_stopwords <- readLines("C:/Users/khuna/OneDrive/Desktop/QTA assisgn/word segment/stopwords_th.txt", encoding = "UTF-8")
thai_stopwords <- trimws(thai_stopwords)

# transform the segment_speech into Vector
convert_to_vector <- function(segmented_string) {
  words <- str_split(segmented_string, "\\[|\\]|', '|',|\\'")[[1]]
  words <- words[words != "" & words != " " & !grepl("\\\\r\\\\n", words)]
  return(words)
}
merged_speech_data$MergedWords_vector <- lapply(merged_speech_data$MergedWords, convert_to_vector)

merged_speech_data <- merged_speech_data %>%
  group_by(party) %>%
  summarise(combined_text = paste(MergedWords_vector, collapse = " ")) %>%
  ungroup()
###########
# This code only need for the year 2564 due to there are duplicate party
combined_row <- merged_speech_data %>%
  slice(2:3) %>%  # Select rows 2 and 3
  summarise(
    party = "พรรคก้าวไกล",  
    combined_text = str_c(combined_text, collapse=" ")  
  )

merged_speech_data <- merged_speech_data[-c(2, 3), ]  # Remove rows 2 and 3
merged_speech_data <- bind_rows(merged_speech_data[1, ], combined_row, merged_speech_data[-(1:2), ])
############

# Tokenization the text
tokens_speech <- tokens(merged_speech_data$combined_text, remove_punct = TRUE)

# Remove Thai stopwords
clean_tokens <- tokens_speech %>%
  tokens_remove(pattern = thai_stopwords)

#create dfm 
dfm_object <- dfm(clean_tokens)
dfm_object <- dfm_trim(dfm_object, min_docfreq = 2)
docvars(dfm_object, "party") <- merged_speech_data$party

#save(dfm_object, file = "dfm_2564.RData")

wf <- textmodel_wordfish(dfm_object, dir = c(1, 2))
summary(wf)

#create our data frame
results_df <- data.frame(
  party = docvars(dfm_object, "party"),
  theta = wf$theta,
  se = wf$se.theta
)

print(results_df)

results_df$party <- factor(results_df$party, levels = results_df$party[order(results_df$theta)])

# Plot
ggplot(results_df, aes(y = party, x = theta)) +
  geom_point() +
  geom_errorbarh(aes(xmin = theta - se, xmax = theta + se), height = 0.2) +
  labs(title = "Wordfish Analysis Results for 2021",
       x = "Estimated theta",
       y = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank())

# Assuming 'wf' is your Wordfish model object
save(wf, file = "wordfish_model_2564.RData")

# Apply LDA on the filtered matrix
set.seed(123)  # For reproducibility
lda_result <- LDA(dfm_object, k = 5, control = list(seed = 123))

top_terms_lda <- terms(lda_result, 10)
top_terms_lda

top_terms <- textstat_frequency(dfm_object)
head(top_terms, 10)



