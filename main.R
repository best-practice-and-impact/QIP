library(yaml)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
library(janitor)
source("collate_sheets.R")

config <- yaml::read_yaml("config.yml")

data <- join_data(config)
text_df <- concat_text_cols(data, config$support_owner$idicator_columns)

data_owner <- dplyr::bind_cols(data, match_sub_theme(text_df$text_fields, 
                                                     config$support_owner$owners))
