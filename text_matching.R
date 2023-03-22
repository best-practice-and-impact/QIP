# label_themes <- function(data, theme_dict, sub_themes = TRUE) {
#   
#   if (sub_themes) {
#     unnested_sub_themes <- theme_dict %>% unlist %>% as.vector
#     
#     sub_theme_cols <- match_sub_theme(data, unnested_sub_themes)
#     
#     major_theme_cols <- match_themes(sub_theme_cols, theme_dict)
#     
#     data <- cbind(data, major_theme_cols, sub_theme_cols)
#   }
#   
# }


match_theme <- function(data, theme_dict){
  
  # TODO: Create subtheme matching list from theme dict
  
  apply(data, 1, function(row){
    
    sapply(theme_dict, function(theme){
      
      any(row[theme])
      
    })
    
  }) %>% t %>% data.frame
  
}


match_sub_theme <- function(col, theme_dict) {
  
  sapply(theme_dict, function(theme) {
    
    theme <- paste0("(", paste0(theme, collapse = ")|("), ")")
    
    grepl(pattern = theme, x = col, ignore.case = T)
    
  }) %>% data.frame
  
}

concat_text_cols <- function(data, text_cols) {
  
  text_cols <- stringr::str_subset(colnames(data), paste0(text_cols, collapse = "|"))
  
  output <- tidyr::unite(data, "text_fields", tidyr::all_of(text_cols), sep = " ", na.rm = T) %>% 
    select(text_fields)
  
  return(output)
}

combine_label_cols <- function(label_df) {
  apply(label_df, 1, function(row) {
    names(row)[unlist(row)] %>% paste0(collapse = ", ")
  }) 
  # if all flags are false then should this produce na not blank?
}
