#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param datatable Whether to output a [DT::datatable] HTML table widget
#'  instead of a regular dataframe (defaults to TRUE).
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' table_country_journal(data)
#' }
#' @importFrom rlang .data
#' @export

table_country_journal <- function(data, datatable = TRUE) {
  x <- data %>%
    dplyr::group_by(.data$journal, .data$jabbrv, .data$country) %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(name = "Papers") %>%
    dplyr::mutate(percentage = as.numeric(round(.data$Papers / get_journal_papers2(
      data, .data$journal
    ) * 100, 2))) %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers))

  df_country_journal_missing <- data %>%
    dplyr::filter(is.na(.data$country)) %>%
    dplyr::group_by(.data$journal, .data$jabbrv) %>%
    dplyr::count(.data$journal, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers)) %>%
    dplyr::left_join(by = "journal", data %>%
      dplyr::group_by(.data$journal) %>%
      dplyr::count(.data$journal, name = "all_papers") %>%
      dplyr::arrange(dplyr::desc(.data$journal))) %>%
    dplyr::mutate(
      percentage = as.numeric(round(.data$Papers / .data$all_papers * 100, 2)),
      country = "Missing*"
    ) %>%
    dplyr::select(-"all_papers")

  x <- x %>%
    dplyr::ungroup() %>%
    dplyr::add_row(df_country_journal_missing) %>%
    dplyr::arrange(.data$journal, dplyr::desc(.data$Papers)) %>%
    dplyr::rename(`Journal Abbreviation` = "jabbrv") %>%
    dplyr::rename_with(stringr::str_to_title)

  if (isTRUE(datatable)) {
    insight::check_if_installed("DT")
    x <- DT::datatable(x,
      caption = "Journal paper percentages, by country and journal"
    )
  }
  x
}

#' @noRd
get_journal_papers2 <- function(data, journal) {
  df_country_journal_missing2 <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(.data$journal, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers))

  df_country_journal_missing2[which(
    df_country_journal_missing2$journal == journal
  ), "Papers"]
}

