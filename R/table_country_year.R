#' @title Generate table of journal paper percentages, by country and year
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' table_country_year(data)
#' }
#' @importFrom rlang .data
#' @export

table_country_year <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::group_by(.data$year, .data$country) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_year_papers(data, .data$year) * 100, 2)))

  df_country_year_missing <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::group_by(.data$year) %>%
    dplyr::count(.data$year, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers)) %>%
    dplyr::left_join(by = "year", data %>%
      dplyr::group_by(.data$year) %>%
      dplyr::count(.data$year, name = "all_papers") %>%
      dplyr::arrange(dplyr::desc(.data$year))) %>%
    dplyr::mutate(
      percentage = round(.data$Papers / .data$all_papers * 100, 2),
      country = "Missing*"
    ) %>%
    dplyr::select(-"all_papers")

  x <- x %>%
    dplyr::ungroup() %>%
    dplyr::add_row(df_country_year_missing) %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers)) %>%
    dplyr::rename_with(stringr::str_to_title)

  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(x,
      caption = "Journal paper percentages, by country and year"
    )
  }
  x
}
