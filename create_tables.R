create_owner_tables <- function(data, owner_bucket, config) {

  data <- data %>%
    dplyr::filter(ownership_bucket_imputed %in% owner_bucket)
  
  if ("1. Control - Division" %in% owner_bucket) {
    owner_list <- unique(data$division)
  } else {
    owner_list <- names(config$support_owner$owners)
  }
  
  output <- list()
  for (owner in owner_list){
    
    if ("1. Control - Division" %in% owner_bucket) {
      output[[owner]] <- dplyr::filter(data, division == owner)
    } else {
      output[[owner]] <- dplyr::filter(data, !!as.name(owner))
    }
    
    output[[owner]] <- output[[owner]] %>%
      dplyr::select("Division" = division,
                    "Risk title" = title.x,
                    "Risk description" = description,
                    "Action" = action,
                    "Action definition of Done" = definition_of_done) %>%
      dplyr::mutate("Will this action be delivered under the planning based on provisional allocated funding for the next financial year? Y or N" = NA,
                    "Any comments on the action" = NA,
                    "Is this action part of your must-have plans or is it dependent on the final funding allocation?" = NA,
                    "If the action is not delivered what would be the impact on the risk to quality?" = NA,
                    "Completed by" = NA)
  }

  if ("1. Control - Division" %in% owner_bucket) {
    names(output) <- paste0(names(output), "_1")
  } else {
    names(output) <- paste0(names(output), "_2")
  }
  
  return(output)
}