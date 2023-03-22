library(yaml)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
library(janitor)
source("collate_sheets.R")
source("text_matching.R")

config <- yaml::read_yaml("config.yml")

data <- join_data(config)

text_df <- concat_text_cols(data, config$support_owner$idicator_columns)

labels <- match_sub_theme(text_df$text_fields, 
                config$support_owner$owners)

combined_labels <- data.frame("support_function" = combine_label_cols(labels))

data <- dplyr::bind_cols(data, combined_labels, labels)

data$action_owner_division <- dplyr::case_when(
  is.na(data$ownership_bucket) | data$ownership_bucket == "2. Control - With support" | data$ownership_bucket == "3. Outside control" ~ data$support_function,
  data$ownership_bucket == "1. Control - Division" ~ data$division,
  data$ownership_bucket == "DQHub to decide" ~ "DQHub to decide", 
  .default = data$ownership_bucket
)

data$QA <- NA

save_data(data, config)