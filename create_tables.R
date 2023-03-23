create_divisional_tables <- function(data) {
  output <- list()
  for (owner in names(config$support_owner$owners)){
    output[[owner]] <- data %>% 
      dplyr::filter(!!as.name(owner)) %>% 
      dplyr::select(division, 
                    "risk_title" = title.x, 
                    "risk_description" = description,
                    action, 
                    definition_of_done,
                    ownership_bucket_imputed) %>% 
      dplyr::mutate("will_action_be_delivered_in_financial_year" = NA,
                    "comments" = NA,
                    "must_have_or_funding_dependent" = NA,
                    "impact_if_not_delivered" = NA,
                    "completed_by" = NA)
  }
  return(output)
}

