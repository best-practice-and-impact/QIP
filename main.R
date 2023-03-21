library(yaml)
library(xlsx)
library(magrittr)
library(dplyr)
library(tidyr)
source("collate_sheets.R")

config <- yaml::read_yaml("config.yml")

dummy_data <- data.frame("col1" = c("a", "e", "c"),
                         "col2" = c("d", "a", "f"),
                         "col3" = c("a", "d", "c"))

theme = list(sub1 = c("a", "b", "c"), sub2 = c("d", "e", "f"))


label_themes <- function(data, theme_dict, sub_themes = TRUE) {
  
  if (sub_themes) {
    unnested_sub_themes <- theme_dict %>% unlist %>% as.vector
    
    sub_theme_cols <- match_sub_theme(data, unnested_sub_themes)
    
    major_theme_cols <- match_themes(sub_theme_cols, theme_dict)
    
    data <- cbind(data, major_theme_cols, sub_theme_cols)
  }
  
}


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
    
    grepl(pattern = theme, x = col)
    
  }) %>% data.frame
  
}
