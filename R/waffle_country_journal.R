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
    dplyr::filter(.data$Country != "Missing*")

  top_seven <- df_country_journal %>%
    dplyr::slice(1:7) %>%
    dplyr::pull("Country")

  df_country_journal <- df_country_journal %>%
    dplyr::select(-c("Papers", "Journal Abbreviation")) %>%
    dplyr::summarize(Percentage = sum(.data$Percentage),
                     .by = c("Journal", "Country")) %>%
    dplyr::arrange(dplyr::desc(.data$Percentage)) %>%
    dplyr::mutate(Country = dplyr::if_else(
      .data$Country %in% top_seven, .data$Country, "Other"),
      Country = factor(.data$Country, levels = unique(.data$Country)))

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
    ggplot2::scale_fill_brewer(palette = "Set2")

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}
