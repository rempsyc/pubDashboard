#' @title Add regions to pubDashboard dataframe
#' @param data The dataframe on which to add region.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' x <- add_region(x)
#' names(x)
#' }
#' @export
add_region <- function(data) {
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(address = extract_author_info(.data$author, object = "address"),
                  country_code = extract_author_info(.data$author, object = "country"),
                  country = countrycode::countrycode(.data$country_code, "genc2c", "country.name"),
                  region = countrycode::countrycode(
                    .data$country_code, "genc2c", "un.regionsub.name"
                  ),
                  continent = countrycode::countrycode(
                    .data$country_code, "genc2c", "continent"
                  ),
                  continent = dplyr::case_when(
                    .data$continent == "Americas" ~ .data$region,
                    TRUE ~ .data$continent
                  ))
  data
}

extract_author_info <- function(author_list, object = "country", author_position = "first") {
  # extract_author_info(author_list = author_list, object = "country")
  # extract_author_info(author_list = author_list, object = "address")
  author_list <- as.data.frame(author_list)
  if (object == "country") {
    object_name <- "institution_country_code"
  } else if (object == "address") {
    object_name <- "au_affiliation_raw"
  }
  if("author_position" %in% names(author_list)) {
    out <- author_list %>%
      dplyr::filter(author_position == !!author_position) %>%
      dplyr::pull(object_name)
  } else {
    out <- NA
  }
  out
}
