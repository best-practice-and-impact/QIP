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
    
    theme <- paste0(theme, collapse = "|")
    
    grepl(pattern = theme, x = col, ignore.case = T)
    
  }) %>% data.frame
  
}

# TODO: add concat_text_cols 
