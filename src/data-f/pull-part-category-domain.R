################################################################################
# Pull participant by category and domain data
################################################################################

pull.part.category.domain <- function(data, participant, category_keep,
                                      domain_keep){
  return(
    subset(x = data, subset = domain_char == domain_keep) |>
      subset(subset = id == participant) |>
      subset(subset = condition_char == category_keep)
  )
}