library(yaml)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
library(janitor)
source("collate_sheets.R")
source("text_matching.R")
source("create_tables.R")

config <- yaml::read_yaml("config.yml")

data <- join_data(config)

text_df <- concat_text_cols(data, config$support_owner$idicator_columns)

labels <- match_sub_theme(text_df$text_fields, 
                config$support_owner$owners)

combined_labels <- data.frame("support_function" = combine_label_cols(labels))

data <- dplyr::bind_cols(data, combined_labels, labels)

data$ownership_bucket_imputed <- dplyr::case_when(
          is.na(data$ownership_bucket) |
            data$ownership_bucket == "DQHub to decide" ~ "2. Control - With support",
          TRUE ~ data$ownership_bucket)
data$action_owner_division = dplyr::case_when(
          data$ownership_bucket_imputed == "2. Control - With support" | 
            data$ownership_bucket_imputed == "3. Outside control" ~ data$support_function,
          data$ownership_bucket_imputed == "1. Control - Division" ~ data$division)

data$QA <- NA

owner_data <- create_owner_tables(data = data, 
                                  owner_bucket = "1. Control - Division", 
                                  config = config)
save_owner_data(owner_data, config)

owner_data <- create_owner_tables(data = data, 
                                  owner_bucket = c("2. Control - With support", "3. Outside control"), 
                                  config = config)
save_owner_data(owner_data, config)

save_data(data, config)
