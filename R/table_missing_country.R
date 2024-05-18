#' @title Generate table of journal paper percentages, by country
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' data[1, c(4, 6)] <- NA
#' table_missing_country(data)
#' }
#' @importFrom rlang .data
#' @export

table_missing_country <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::select("journal", "continent", "region", "country",
                  "country_code", "author", "author_position", "institution",
                  "address", "date", "doi", "id", "title", "cited_by_count",
                  "jabbrv", "alt_title", "field") %>%
    dplyr::arrange(.data$address) %>%
    dplyr::mutate(doi = paste0("<a href='", .data$doi, "' target='_blank'>", .data$doi, "</a>"),
                  id = paste0("<a href='", .data$id, "' target='_blank'>", .data$id, "</a>"))

  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(x,
      extensions = "Responsive",
      options = list(iDisplayLength = 5),
      caption = "Journal paper percentages, by country",
      escape = FALSE
    )
  }
  x
}
