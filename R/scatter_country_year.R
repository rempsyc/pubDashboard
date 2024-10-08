#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param method Which method to use for the regression line, either "lm" (default) or "loess".
#' @param ymin Minimum value for y-axis
#' @param ymax Maximum value for y-axis
#' @param yby Tick increments for y-axis
#' @param plotly Logical, whether to use plotly for dynamic data visualization.
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @param text_size Size of the element_text ggplot2 element
#' @param height Height argument of [plotly::ggplotly]
#' @param ... Further arguments passed to [rempsyc::nice_scatter]
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' suppressWarnings(scatter_country_year(data))
#' }
#' @importFrom rlang .data
#' @export

scatter_country_year <- function(data,
                                 method = "lm",
                                 ymin = 0,
                                 ymax = 100,
                                 yby = 20,
                                 plotly = TRUE,
                                 citation = NULL,
                                 citation_size = 15,
                                 text_size = NULL,
                                 height = NULL,
                                 ...) {
  x <- table_country_year(data, datatable = FALSE) %>%
    clean_top()

  top_order <- x %>%
    dplyr::group_by(.data$Country) %>%
    dplyr::summarize(Percentage = sum(.data$Percentage)) %>%
    dplyr::arrange(dplyr::desc(.data$Percentage)) %>%
    dplyr::arrange(.data$Country == "Other") %>%
    dplyr::pull("Country") %>%
    as.character()

  x <- x %>%
    dplyr::mutate(Country = factor(.data$Country, levels = top_order))

  getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
  colours.country2 <- getPalette(8)

  p <- x %>%
    rempsyc::nice_scatter(
      predictor = "Year",
      response = "Percentage",
      group = "Country",
      colours = colours.country2,
      method = method,
      ymin = ymin,
      ymax = ymax,
      yby = yby,
      # groups.order = "decreasing",
      ytitle = "% of All Papers",
      ...
    )

  if (!is.null(text_size)) {
    p <- p +
      ggplot2::theme(text = ggplot2::element_text(size = text_size))
  }

  if (isTRUE(plotly)) {
    insight::check_if_installed("plotly")
    p <- plotly::ggplotly(tooltip = c("x", "y"), height = height)
    if (!is.null(citation)) {
      p <- plotly_citation(p, citation, citation_size = citation_size)
    }
  } else if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

#' @noRd
get_year_papers <- function(data, year) {
  x <- data %>%
    dplyr::filter(!is.na(.data$country)) %>%
    dplyr::count(.data$year, name = "Papers") %>%
    dplyr::arrange(dplyr::desc(.data$year), dplyr::desc(.data$Papers))

  x[which(x$year == year), "Papers"]
}
