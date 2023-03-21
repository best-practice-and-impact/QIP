library(yaml)
library(xlsx)
library(magrittr)

config <- yaml::read_yaml("config.yml")


read_sheets <- function(sheet_name, config) {
  
  files <- list.files(config$input_path)
  
  files <- paste0(config$input_path, "/",  files)
  
  files <- files[-grep("~", files)]
  
  all_tables <- lapply(files, function(file) {
    data <- openxlsx::read.xlsx(file, sheet = sheet_name)
    division_info <- openxlsx::read.xlsx(file, sheet = "Division_info")
    
    # TODO: deal with empty sheets
    
    data$division <- colnames(division_info)[2]
    
    data %<>% 
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    
    # TODO: reorder data so that division name is the first column
    
    return(data)
  })
  
  do.call(dplyr::bind_rows, all_tables)
  
}

# TODO: load all relevant sheets and save to output folder (outputs/) - folder name should come from config file. Save as CSV