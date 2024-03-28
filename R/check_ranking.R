#' Check that the values in the Rank attribute are valid; allowable values are
#' None, Primary, Principal, Secondary, Distributed. Note that Principal and
#' Distributed will be replaced with Primary and Secondary, respectively, for
#' standardization purposes.
#'
#' @param sf_object The sf object.
#'
#' @return The sf object with rank cleaned-up as needed. The function will stop
#'   execution and throw an error if an invalid Rank is found.
#' @export
check_ranking <- function(sf_object) {
  # Standardizing rank nomenclature; update ranks and track changes
  original_rank <- sf_object$Rank

  sf_object <- sf_object %>%
    dplyr::mutate(Rank = dplyr::case_when(
      Rank == "Principal" ~ "Primary",
      Rank == "Distributed" ~ "Secondary",
      TRUE ~ as.character(Rank)
    ))

  if (!all(original_rank == sf_object$Rank)) {
    warning("Some values in `Rank` were renamed for the purposes of standardization.\n")
  }

  # Check for invalid entries
  valid_options <- c("None", "Primary", "Secondary")
  invalid_entries <- !sf_object$Rank %in% valid_options

  if (any(invalid_entries)) {
    invalid_values <- unique(sf_object$Rank[invalid_entries])
    stop("Error: The following invalid `Rank` values were found: ",
         paste(invalid_values, collapse = ", "), ".")
  }

  return(sf_object)
}
