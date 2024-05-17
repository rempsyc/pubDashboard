#' @title Add regions to pubDashboard dataframe
#' @param data The dataframe on which to add region.
#' @param progress_bar Logical, whether to print a progress bar.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' x <- add_region(x)
#' names(x)
#' }
#' @export
add_region <- function(data,
                       progress_bar = FALSE) {
  if (progress_bar) {
    pb <- progress::progress_bar$new(
      format = paste(
        "Adding continent information",
        "[:bar] :current/:total (:percent) [Elapsed: :elapsedfull || Remaining: :eta]"
      ),
      total = nrow(data),
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = FALSE, # If TRUE, clears the bar when finish
      show_after = 0, # Seconds necessary before showing progress bar
      width = 100 # Width of the progress bar
    )
  }

  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(address = extract_author_info(.data$author,
                                                object = "address",
                                                pb = pb,
                                                progress_bar = progress_bar),
                  country_code = extract_author_info(.data$author,
                                                     object = "country",
                                                     pb = pb,
                                                     progress_bar = FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      country = countrycode::countrycode(.data$country_code, "genc2c", "country.name"),
      region = countrycode::countrycode(
        .data$country_code, "genc2c", "un.regionsub.name"),
      continent = countrycode::countrycode(
        .data$country_code, "genc2c", "continent"
        ),
      continent = dplyr::case_when(
        .data$continent == "Americas" ~ .data$region,
        TRUE ~ .data$continent))
  data
}

extract_author_info <- function(author_list,
                                object = "country",
                                author_position = "first",
                                progress_bar = FALSE,
                                pb = NULL) {
  # extract_author_info(author_list = author_list, object = "country")
  # extract_author_info(author_list = author_list, object = "address")
  if (progress_bar) {
    pb$tick()
  }
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
