#' @title Generate a dygraph of journal paper percentages, by country and year
#' @param data The processed dataframe of data
#' @param level Level of analysis, either country or continent
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' dygraph_year(data)
#' dygraph_year(data, "country")
#' }
#' @importFrom rlang .data
#' @export

dygraph_year <- function(data, level = "continent") {
  insight::check_if_installed(c("dygraphs", "xts"))
  df_country_year <- data %>%
    dplyr::group_by(.data$year, .data[[level]]) %>%
    dplyr::filter(!is.na(.data[[level]])) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_year_papers(
      data, .data$year
    ) * 100, 2)))

  # Time series dygraph
  q <- df_country_year %>%
    dplyr::ungroup() %>%
    dplyr::select("year", dplyr::all_of(level), "percentage") %>%
    dplyr::mutate(year = as.Date(as.character(.data$year), "%Y")) %>%
    tidyr::pivot_wider(names_from = level, values_from = "percentage") %>%
    xts::as.xts()

  q %>%
    dygraphs::dygraph() %>%
    dygraphs::dyRangeSelector() %>%
    dygraphs::dyUnzoom() %>%
    dygraphs::dyCrosshair(direction = "vertical") %>%
    dygraphs::dyOptions(strokeWidth = 3)
}
