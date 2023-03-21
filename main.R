library(yaml)
library(xlsx)
library(magrittr)
library(dplyr)

config <- yaml::read_yaml("config.yml")


read_sheets <- function(sheet_name, config) {
  
  files <- list.files(config$input_path)
  
  files <- paste0(config$input_path, "/",  files)
  
  files <- files[-grep("~", files)]
  
  all_tables <- lapply(files, function(file) {
    data <- openxlsx::read.xlsx(file, sheet = sheet_name)
    division_info <- openxlsx::read.xlsx(file, sheet = "Division_info")
    
    # Drop empty data sets because some divisions have no actions
    if (nrow(data) > 0) {
      
      data %<>% 
        dplyr::mutate(divsion = colnames(division_info)[2], .before = everything(),
                      dplyr::across(dplyr::everything(), as.character))

      return(data)
    }
    
  })
  
  do.call(dplyr::bind_rows, all_tables)
  
}

for (sheet_name in config$sheet_names) {
  write.csv(read_sheets(sheet_name, config), paste0(config$output_path, "/", sheet_name, ".csv"))
}
