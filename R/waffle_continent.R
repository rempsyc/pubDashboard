#' @title Generate a waffle chart of journal paper percentages, by continent (each square = 1% of data)
#' @param data The processed dataframe of data
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Journal of Economic Psychology", pages = 1)
#' data <- clean_journals_continents(data)
#' waffle_continent(data)
#' }
#' @importFrom rlang .data
#' @export

waffle_continent <- function(data, citation = NULL, citation_size = NULL) {
  insight::check_if_installed(c("waffle", "ggplot2", "RColorBrewer"))

  x <- table_continent(data, datatable = FALSE) %>%
    dplyr::select(-c("Missing*", "Papers")) %>%
    tidyr::pivot_longer("North America":"Africa", names_to = "Continent", values_to = "Papers_percentage")

  # Bump < 1 values to 1 and add percentages to labels
  x <- x %>%
    dplyr::mutate(Continent = paste0(
      .data$Continent, " (", round(.data$Papers_percentage, 1), " %)"),
      Papers_percentage = dplyr::if_else(.data$Papers_percentage < 1, 1, .data$Papers_percentage))

  p <- waffle::waffle(x, legend_pos = "right") +
    ggplot2::theme(legend.text = ggplot2::element_text(size = 10))

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
