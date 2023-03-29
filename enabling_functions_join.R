library(yaml)
library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
library(janitor)
source("collate_sheets.R")

read_sheets <- function(sheet_name, config) {
  
  files <- list.files(config$input_path)
  
  if (length(grep("~", files)) >= 1) {
    files <- files[-grep("~", files)]
  }
  
  files <- paste0(config$input_path, "/",  files)
  
  all_tables <- lapply(files, function(file) {
    print(file)
    
    data <- openxlsx::read.xlsx(file, sheet = sheet_name)
    
    # Drop empty data sets because some divisions have no actions
    if (nrow(data) > 0) {
      
      split_filename <- stringr::str_split(file, "/")[[1]]
      name <- split_filename[length(split_filename)]
      
      name <- stringr::str_split(name, "[.]")[[1]][1]
      
      data %<>% 
        dplyr::mutate(division = name, .before = dplyr::everything(),
                      # some dates included so I think the following line could be improved
                      dplyr::across(dplyr::everything(), as.character)) %>% 
        janitor::clean_names() %>% 

      return(data)
    }
    
  })
  
  do.call(dplyr::bind_rows, all_tables)

}


config <- yaml::read_yaml("enabling_functions_config.yml")

data <- read_sheets(config$sheet_names, config)

save_data(data, config)
