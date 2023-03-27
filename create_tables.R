create_divisional_tables <- function(data) {
  output <- list()
  for (owner in names(config$support_owner$owners)){
    output[[owner]] <- data %>% 
      dplyr::filter(!!as.name(owner)) %>% 
      dplyr::select("Division" = division, 
                    "Risk title" = title.x, 
                    "Risk description" = description,
                    "Action" = action, 
                    "Action definition of Done" = definition_of_done,
                    ownership_bucket_imputed) %>% 
      dplyr::mutate("Will this action be delivered under the planning based on provisional allocated funding for the next financial year? Y or N" = NA,
                    "Any comments on the action" = NA,
                    "Is this action part of your must-have plans or is it dependent on the final funding allocaton?" = NA,
                    "If the action is not delivered what would be the impact on the risk to quality?" = NA,
                    "Completed by" = NA)
  }
  return(output)
}

format_owner_table <- function(data){
  output <- data %>% 
    dplyr::filter(ownership_bucket_imputed %in%
                    c("2. Control - With support", "3. Outside control")) %>% 
    dplyr::select(-ownership_bucket_imputed)
    
  return(output)
}