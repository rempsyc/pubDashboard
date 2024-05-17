#' @title Generate a waffle chart of journal paper percentages, by continent (each square = 1% of data)
#' @param data The processed dataframe of data
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' waffle_continent(data)
#' }
#' @importFrom rlang .data
#' @export

waffle_continent <- function(data, citation = NULL, citation_size = NULL) {
  insight::check_if_installed(c("waffle", "ggplot2", "RColorBrewer"))
  x <- data %>%
    dplyr::mutate(missing = sum(is.na(.data$continent)) / dplyr::n()) %>%
    dplyr::filter(!is.na(.data$continent)) %>%
    dplyr::group_by(.data$continent) %>%
    dplyr::add_count(name = "Papers") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::count(.data$continent, nrow, sort = TRUE, name = "Papers") %>%
    dplyr::mutate(
      continent = dplyr::case_match(
        .data$continent,
        continent_order()[1] ~ continent_order(short = TRUE)[1],
        continent_order()[5] ~ continent_order(short = TRUE)[5],
        .data$continent ~ .data$continent
      ),
      Percentage = .data$Papers / nrow * 100
    ) %>%
    dplyr::select(-c("nrow", "Papers")) %>%
    dplyr::rename_with(stringr::str_to_title, .cols = 1)

  if (!"Latin America" %in% x$Continent) {
    x <- x %>%
      dplyr::add_row(Continent = "Latin America", Percentage = 0)
  }

  p <- waffle::waffle(x, legend_pos = "right",
                      # colors = c(RColorBrewer::brewer.pal(nrow(x) + 1, "Set2"))
                      ) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 15)) #+
    # ggplot2::scale_fill_manual(
    #   values = c(RColorBrewer::brewer.pal(nrow(x), "Set2")),
    #   labels = continent_order(short = TRUE),
    #   name = NULL,
    #   drop = FALSE)

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

gg_citation <- function(x, citation, citation_size) {
  insight::check_if_installed("ggtext")
  x +
    ggplot2::xlab(citation) +
    ggplot2::theme(axis.title.x = ggtext::element_markdown(
      hjust = 1, size = citation_size))
}
