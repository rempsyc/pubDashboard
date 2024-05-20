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
#' waffle_continent_journal(data)
#' }
#' @importFrom rlang .data
#' @export

waffle_continent_journal <- function(data, citation = NULL, citation_size = NULL, journal_abbreviation = TRUE) {
  insight::check_if_installed(c("waffle", "ggplot2", "RColorBrewer"))
  if (isTRUE(journal_abbreviation)) {
    data <- data %>%
      dplyr::mutate(journal = .data$jabbrv)
  }

  x <- table_continent_journal(data, datatable = FALSE) %>%
    dplyr::select(-c("Missing*", "Papers", "Journal Abbreviation")) %>%
    tidyr::pivot_longer(-"Journal", names_to = "continent", values_to = "papers_percentage") %>%
    dplyr::mutate(continent = factor(.data$continent, levels = continent_order(short = TRUE)))

  # x <- data %>%
  #   dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
  #   dplyr::filter(!is.na(.data$continent)) %>%
  #   dplyr::group_by(.data$journal) %>%
  #   dplyr::summarize(
  #     `North America` = sum(.data$continent == "Northern America") / dplyr::n(),
  #     Europe = sum(.data$continent == "Europe") / dplyr::n(),
  #     Asia = sum(.data$continent == "Asia") / dplyr::n(),
  #     Oceania = sum(.data$continent == "Oceania") / dplyr::n(),
  #     `Latin America` = sum(.data$continent == "Latin America and the Caribbean") / dplyr::n(),
  #     Africa = sum(.data$continent == "Africa") / dplyr::n(),
  #   ) %>%
  #   dplyr::mutate(dplyr::across(2:6, ~ .x * 100)) %>%
  #   dplyr::arrange("journal") %>%
  #   tidyr::pivot_longer(-.data$journal, names_to = "continent", values_to = "number") %>%
  #   dplyr::mutate(continent = factor(.data$continent, levels = continent_order(short = TRUE))) %>%
  #   stats::na.omit()

  # if (!"Latin America" %in% x$continent) {
  #   x <- x %>%
  #     dplyr::add_row(continent = "Latin America", number = 0)
  # }
  # if (!"Africa" %in% x$continent) {
  #   x <- x %>%
  #     dplyr::add_row(continent = "Africa", number = 0)
  # }

  # Reorder continents for consistent continent order
  # x <- x %>%
  #   dplyr::arrange(match(x$continent, continent_order(short = TRUE)))

  # Bump < 1 values to 1
  x <- x %>%
    dplyr::mutate(papers_percentage = dplyr::if_else(
      .data$papers_percentage < 1, 1, .data$papers_percentage))

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(x$continent)), "Set2"
  ))

  p <- x %>%
    ggplot2::ggplot(ggplot2::aes(fill = .data$continent, values = .data$papers_percentage)) +
    waffle::geom_waffle(color = "white", size = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~Journal) +
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
    ggplot2::scale_fill_manual(values = colors)

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}
