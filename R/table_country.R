#' @title Generate table of journal paper percentages, by country
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' table_country(data)
#' }
#' @importFrom rlang .data
#' @export

table_country <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::filter(!is.na(.data$country_code)) %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::count(.data$country, .data$country_code, .data$nrow, sort = TRUE, name = "Papers") %>%
    dplyr::mutate(Percentage = .data$Papers / nrow) %>%
    dplyr::select(-"nrow") %>%
    dplyr::add_row(
      country = "Missing*",
      Papers = sum(is.na(data$country)),
      Percentage = sum(is.na(data$country)) / nrow(data),
      .before = 1
    ) %>%
    dplyr::rename_with(stringr::str_to_title) %>%
    dplyr::mutate(Percentage = round(.data$Percentage * 100, 2)) %>%
    dplyr::rename("Country Code" = "Country_code")

  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(x,
      caption = "Journal paper percentages, by country"
    )
  }
  x
}
