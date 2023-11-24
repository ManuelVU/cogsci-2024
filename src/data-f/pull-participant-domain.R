################################################################################
# Pull participant and domain data
################################################################################

pull.part.domain <- function(data, participant, domain_keep){
  return(
    subset(x = data, subset = domain_char == domain_keep) |>
      subset(subset = id == participant)
  )
}