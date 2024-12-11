# Load required libraries
library(readxl)
library(syuzhet)
library(stats)
library(base)
library(dplyr)
library(lubridate)

# List of file paths for each creator
file_paths <- c(
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Spencer Hunt/spencer hunt.xlsx",
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Liza Koshy/liza koshy.xlsx",
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Caitlin_Reilly/caitlin reilly.xlsx",
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Adam Waheed/adam waheed.xlsx",
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Daniel_Labelle/daniel labelle.xlsx",
  "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/Hannah Stocking/hannah stocking.xlsx"
)

# Initialize an empty data frame to store results
all_sentiment_data <- data.frame()

# Process each file
for (file_path in file_paths) {
  # Read data
  data <- read_excel(file_path)
  
  # Combine text columns
  combined_text <- paste(data$video_descriptions, data$text_on_cover, data$transcriptions, sep = " ")
  
  # Clean the combined text
  cleaned_text <- tolower(combined_text) %>%
    gsub("[[:punct:]]", "", .) %>%
    gsub("[[:cntrl:]]", "", .) %>%
    gsub("\\d+", "", .) %>%
    gsub("[^[:alnum:][:space:]]", "", .)
  
  # Extract sentiment scores
  sentiments <- get_nrc_sentiment(cleaned_text)
  
  # Add a Date column in case the source file doesn't include month grouping
  if (!"Date" %in% names(data)) {
    stop("The file does not contain a 'Date' column. Ensure all files include this column.")
  }
  
  # Extract month and year
  data$Month <- floor_date(ymd(data$Date), "month")
  
  # Calculate sentiment percentages by month
  sentiment_by_month <- sentiments %>%
    mutate(Month = data$Month) %>%
    group_by(Month) %>%
    summarise(across(everything(), sum)) %>%
    mutate(total_score = rowSums(across(negative:positive))) %>%
    mutate(across(negative:positive, ~ . / total_score * 100)) %>%
    select(-total_score)
  
  # Add creator information
  creator_name <- tools::file_path_sans_ext(basename(file_path))
  sentiment_by_month <- sentiment_by_month %>%
    mutate(Creator = creator_name)
  
  # Combine with the overall data frame
  all_sentiment_data <- bind_rows(all_sentiment_data, sentiment_by_month)
}

# Save the combined data as a CSV file
output_path <- "D:/Surface 文件备份/Study/【博士】埃塞克斯/PhD_Dissertation/Drafts/TikTok Data/monthly_sentiment_us_dv.csv"
write.csv(all_sentiment_data, output_path, row.names = FALSE)

print(paste("Data successfully saved to", output_path))
