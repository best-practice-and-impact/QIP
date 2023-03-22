#' @title Identify Ownership
#' 


library(yaml)
library(magrittr)
library(dplyr)
library(tidyr)

config <- yaml::read_yaml("config.yml")

dummy_data <- data.frame("col1" = c("x", "w", "c"),
                         "col2" = c("w", "y", "f"),
                         "col3" = c("2. Control - With support", "3. Outside control", "1. Control - Division"))
