library(yaml)
library(xlsx)
library(magrittr)
library(dplyr)
source("collate_sheets.R")

config <- yaml::read_yaml("config.yml")

dummy_data <- data.frame("col1" = c("a", "e", "c"),
                         "col2" = c("d", "a", "f"),
                         "col3" = c("a", "d", "c"))

theme = list(sub1 = c("a", "b", "c"), sub2 = c("d", "e", "f"))


match_theme <- function() {
  
}

match_theme <- function(data, theme_dict){
  
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
