#' @title Generate a waffle chart of journal paper percentages, by continent (each square = 1% of data)
#' @param data The processed dataframe of data
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @param journal_abbreviation Logical, whether to use the journal abbreviation
#'  to fit the entire plot, otherwise some journal names can be quite long and
#'  accordingly be cropped.
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' waffle_country_journal(data)
#' }
#' @importFrom rlang .data
#' @export

waffle_country_journal <- function(data, citation = NULL, citation_size = NULL, journal_abbreviation = TRUE) {
  insight::check_if_installed(c("waffle", "ggplot2", "RColorBrewer"))
  . <- NULL
  if (isTRUE(journal_abbreviation)) {
    data <- data %>%
      dplyr::mutate(journal = .data$jabbrv)
  }

  df_country_journal <- table_country_journal(data, datatable = FALSE) %>%
    dplyr::select(-c("Papers", "Journal Abbreviation")) %>%
    dplyr::filter(Country != "Missing*") %>%
    dplyr::mutate(
      Country = dplyr::case_when(
        .data$Percentage < 5 ~ "Other",
        TRUE ~ .data$Country
      )
    ) %>%
    dplyr::summarize(Percentage = sum(.data$Percentage),
                     .by = c("Journal", "Country")) %>%
    dplyr::arrange(dplyr::desc(.data$Percentage))

  # df_country_journal <- data %>%
  #   dplyr::filter(!is.na(.data$country)) %>%
  #   dplyr::group_by(.data$journal, .data$country) %>%
  #   dplyr::add_count(name = "Papers") %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(.data$journal) %>%
  #   dplyr::add_count(name = "journal_count") %>%
  #   dplyr::mutate(
  #     percentage = .data$Papers / .data$journal_count,
  #     country = dplyr::case_when(
  #       .data$percentage < 0.1 ~ "Other",
  #       TRUE ~ .data$country
  #     )
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::count(.data$journal, .data$country, sort = TRUE, name = "Papers") %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     percentage = as.numeric(round(.data$Papers / get_journal_papers2(
  #       data, .data$journal
  #     ) * 100, 2)),
  #     country = as.factor(.data$country)
  #   ) %>%
  #   dplyr::arrange(dplyr::desc(.data$journal), dplyr::desc(.data$Papers)) %>%
  #   stats::na.omit()

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(data$Continent)), "Set2"
  ))

  colours.country2 <- grDevices::colorRampPalette(colors)(length(unique(df_country_journal$Country)))

  p <- df_country_journal %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$Country, values = .data$Percentage)) +
    waffle::geom_waffle(color = "white", size = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~ .data$Journal) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal(base_size = 6) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 5)
    ) +
    ggplot2::scale_fill_manual(values = colours.country2)

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
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
