library(yaml)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
library(janitor)
source("collate_sheets.R")

config <- yaml::read_yaml("config.yml")

data <- join_data(config)

data_owner <- dplyr::bind_cols("description" = data$description,
                               "rap" = match_sub_theme(data$description, config$owner)[,"RAP"])
