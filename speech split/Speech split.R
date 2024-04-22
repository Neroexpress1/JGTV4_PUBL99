library(stringr)
library(openxlsx)
library(tidyverse)

text_raw <- "#We need to manually copy and paste the text from the pre-adjusted document, having removed irrelevant details such as the vote count.
The reason for manually copying and pasting is that there are no tools or packages in R capable of retaining 100% of the PDF format (next paragraph).
Therefore, we have to manage the document in this overly complicated manner.#"


#remove footer number of pages
text <- gsub("\\-\\s*\\p{N}+/\\p{N}+", "", text_raw, perl = TRUE)

# the speaker pattern for first part of speech
speaker_pattern <- "(^|\\n)\\s*(นาย|นางสาว|นาง|พัน|ว่าที่|ผู้ช่วย|ร้อย|หม่อม|รอง|เรือ|ศาสตรา|พล|จ่า)[^:]+?\\(.*?\\)\\s*:"

# Perform initial segmentation based on speaker introductions
segments <- str_split(text, "(?=(^|\\n)\\s*(นาย|นางสาว|นาง|พัน|ว่าที่|ผู้ช่วย|ร้อย|ศาสตรา|หม่อม|รอง|เรือ|พล|จ่า)[^:]+?\\(.*?\\)\\s*:)", n = Inf, simplify = FALSE)[[1]]

# Create variables to hold the results
speakers <- c()
speeches <- c()
merged_segments <- c(segments[1])

# Merge segments that do not start with a valid speaker introduction
for (i in 2:length(segments)) {
  if (str_detect(segments[i], "^\\s*(นาย|นางสาว|นาง|พัน|ว่าที่|ศาสตรา|ผู้ช่วย|ร้อย|หม่อม|รอง|เรือ|พล|จ่า)[^:]+?\\(.*?\\)\\s*:")) {
    # This segment starts with a valid speaker introduction, so it's a new speech
    merged_segments <- c(merged_segments, segments[i])
  } else {
    # This segment is a continuation of the previous one
    merged_segments[length(merged_segments)] <- paste(merged_segments[length(merged_segments)], segments[i], sep=" ")
  }
}

# Extract speaker names and speeches from the merged segments
for (seg in merged_segments) {
  speaker <- str_extract(seg, speaker_pattern)
  if (!is.na(speaker)) {
    speakers <- c(speakers, speaker)
    speech <- str_remove(seg, paste0("^", speaker))
    speeches <- c(speeches, str_trim(speech))
  }
}

# Combine speakers and speeches into a data frame
speaker_data <- data.frame(Speaker = speakers, Speech = speeches, stringsAsFactors = FALSE)


# Clean the speaker name within the speech
cleaned_speeches <- sapply(speeches, function(speech) {
  internal_speaker_pattern <- "\\n?\\s*(นาย|นางสาว|นาง|พัน|ศาสตรา|ว่าที่|ผู้ช่วย|ร้อย|หม่อม|รอง|เรือ|พล|จ่า)[^:]+?:"
  cleaned_speech <- gsub(internal_speaker_pattern, "", speech)
  return(cleaned_speech)
})

# Update the cleaned speeches in the data frame
speaker_data$Speech <- cleaned_speeches

# remove any speaker that row are neutral
filtered_speaker_data <- speaker_data %>%
  filter(
    !grepl("ประธานรัฐสภา", Speaker),
    !grepl("รองประธานรัฐสภา", Speaker),
    !grepl("เลขาธิการรัฐสภา", Speaker)
  )
 
# view the filtered data frame
print(head(filtered_speaker_data))
# Export it to CSV file for text segment for tokenization
write.csv(filtered_speaker_data, "joint_sitting_16_year2555.csv", row.names = FALSE, fileEncoding = "UTF-8")


