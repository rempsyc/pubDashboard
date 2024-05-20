#' @title Count number of papers per journal, with year range
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' table_journal_count(data)
#' }
#' @export
table_journal_count <- function(data, datatable = TRUE) {
  x <- dplyr::count(data, .data$journal, .data$field, .data$year_range, sort = TRUE) %>%
    dplyr::mutate(field = stringr::str_to_title(.data$field)) %>%
    as.data.frame() %>%
    dplyr::rename("year range" = "year_range") %>%
    dplyr::rename_with(stringr::str_to_title)
  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(
      x,
      options = list(searching = TRUE, paging = TRUE),
      caption = "Count of journals, with year range")
  }
  x
}

