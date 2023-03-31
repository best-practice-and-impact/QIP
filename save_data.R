#' @title Save data
#' 
#' @description 
#' Checks if the output file path exists, and if not creates it and then saves
#' the dataframe to a csv file.
#' 
#' @param data data to be saved
#' @param config `list` configuration, typically read in from a yaml config file
#'  
save_data <- function(data, config) {
  if (!dir.exists(sub('/[^/]*$', '', config$output_path))){
    dir.create(sub('/[^/]*$', '', config$output_path))
  }
  
  write.csv(data, config$output_path)
}

#' @title Save Owner Data
#' 
#' @description 
#' Checks if the output file path exists, and if not creates it and then saves
#' the data frame to a csv file.
#' 
#' @param data_list a `list` of data frames to be saved.
#' @param config `list` configuration, typically read in from a yaml config file
#'  
save_owner_data <- function(data_list, config, folder) {
  if (!dir.exists(paste0(sub('/[^/]*$', '', config$output_path), "/", folder))){
    dir.create(paste0(sub('/[^/]*$', '', config$output_path), "/", folder))
  }
  
  for (owner in names(data_list)){
    write.csv(data_list[[owner]], 
              paste0(sub('/[^/]*$', '', config$output_path), 
                     "/", folder, "/", owner, ".csv"),
              row.names = FALSE,
              na = "")
  }
}