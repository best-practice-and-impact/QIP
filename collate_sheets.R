# TODO:
# read_sheets is doing a lot more than reading the data.
# I think we should split this into a read and a clean

#' @title Read Sheets
#' 
#' @description 
#' Read in the given sheet from all files in the input folder 
#' provided in the config.Attach division name to all rows, 
#' coerce all columns to strings, clean col_names, and drops rows 
#' without a risk/issue number as this is used as a key later. 
#' Finally the cleaned data sets are all combined via bind_rows.
#' 
#' @param sheet_name `string` name of the sheet to be loaded
#' @param config `list` configuration, typically read in from a yaml config file
#' 
#' @return `data.frame` combine data set
#' 
read_sheets <- function(sheet_name, config) {
  
  files <- list.files(config$input_path)
  
  if (length(grep("~", files)) >= 1) {
    files <- files[-grep("~", files)]
  }
  
  files <- paste0(config$input_path, "/",  files)
  
  all_tables <- lapply(files, function(file) {
    data <- openxlsx::read.xlsx(file, sheet = sheet_name)
    division_info <- openxlsx::read.xlsx(file, sheet = "Division_info")
    
    # Drop empty data sets because some divisions have no actions
    if (nrow(data) > 0) {
      
      data %<>% 
        dplyr::mutate(division = colnames(division_info)[2], .before = dplyr::everything(),
                      # some dates included so I think the following line could be improved
                      dplyr::across(dplyr::everything(), as.character)) %>% 
        janitor::clean_names() %>% 
        tidyr::drop_na(risk_issue_number)
      
      return(data)
    }
    
  })
  
  do.call(dplyr::bind_rows, all_tables)
  
}

#' @title Join Data
#' 
#' @description 
#' Read and clean all sheets provided in config using `read_sheets`. Where 
#' quality_risk_and_issues and Actions sheets are provided, these are joined. 
#' The joining key is `division` and `risk_issue_number`. This is written to 
#' the output path provided in the config
#' 
#' @param config `list` configuration, typically read in from a yaml config file
#' 
#' @return 
#' 
join_data <- function(config) {
  data <- list()
  for (sheet_name in config$sheet_names) {
    data[[sheet_name]] <- read_sheets(sheet_name, config)
  }
  
  # TODO: 
  # Not sure how metrics fits into this....
  # Would be good to do something more like,
  # if division and risk issue number are in the data then join
  # then it would be a bit more flexible
  if("quality_risks_and_issues" %in% config$sheet_names & "Actions" %in% config$sheet_names){
    all_risk_data <- dplyr::full_join(data$quality_risks_and_issues, 
                                      data$Actions, 
                                      by = c("division", "risk_issue_number"),
                                      multiple = "all")
  }
  
  return(all_risk_data)
}

#' @title Save data
#' 
#' @description 
#' Checks if the output file path exists, and if not creates it and then saves
#' the dataframe to a csv file.
#' 
#' @param config `list` configuration, typically read in from a yaml config file
#'  
save_data <- function(data, config) {
  if (!dir.exists(sub('/[^/]*$', '', config$output_path))){
    dir.create(sub('/[^/]*$', '', config$output_path))
  }
  
  write.csv(data, config$output_path)
}
