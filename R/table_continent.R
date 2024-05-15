#' @title Generate table of journal paper percentages, by continent
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' table_continent(data)
#' }
#' @importFrom rlang .data
#' @export

table_continent <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::summarize(
      Papers = dplyr::n(),
      `North America` = sum(.data$continent == "Northern America") / dplyr::n(),
      Europe = sum(.data$continent == "Europe") / dplyr::n(),
      Asia = sum(.data$continent == "Asia") / dplyr::n(),
      Oceania = sum(.data$continent == "Oceania") / dplyr::n(),
      `Latin America` = sum(.data$continent == "Latin America and the Caribbean") / dplyr::n(),
      Africa = sum(.data$continent == "Africa") / dplyr::n(),
      Missing = dplyr::first(missing),
    ) %>%
    dplyr::mutate(dplyr::across("North America":"Missing", ~ .x * 100)) %>%
    dplyr::mutate(dplyr::across("North America":"Missing", ~ round(.x, 2))) %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::rename("Missing*" = "Missing")
  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(x,
      options = list(searching = FALSE, paging = FALSE),
      caption = "Journal paper percentages, by continent"
    )
  }
  x
}
