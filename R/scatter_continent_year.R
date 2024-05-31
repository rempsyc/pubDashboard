#' @title Generate table of journal paper percentages, by continent and year
#' @param data The processed dataframe of data
#' @param method Which method to use for the regression line, either "lm" (default) or "loess"
#' @param ymin Minimum value for y-axis
#' @param ymax Maximum value for y-axis
#' @param yby Tick increments for y-axis
#' @param plotly Logical, whether to use plotly for dynamic data visualization
#' @param citation Optionally, a citation to add as a footer
#' @param citation_size Font size of the citation
#' @param ... Further arguments passed to [rempsyc::nice_scatter]
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' scatter_continent_year(data)
#' }
#' @importFrom rlang .data
#' @export

scatter_continent_year <- function(data,
                                   method = "lm",
                                   ymin = 0,
                                   ymax = 100,
                                   yby = 20,
                                   plotly = TRUE,
                                   citation = NULL,
                                   citation_size = 15,
                                   ...) {
  insight::check_if_installed("RColorBrewer")
  data <- table_continent_year(data, datatable = FALSE) %>%
    dplyr::select(-c("Missing*", "Papers")) %>%
    tidyr::pivot_longer(-"Year", names_to = "continent", values_to = "papers_percentage") %>%
    dplyr::mutate(continent = factor(.data$continent, levels = continent_order(short = TRUE)))

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(data$continent)), "Set2"
  ))

  p <- rempsyc::nice_scatter(
    data,
    predictor = "Year",
    response = "papers_percentage",
    group = "continent",
    colours = colors,
    method = method,
    ymin = ymin,
    ymax = ymax,
    yby = yby,
    # groups.order = "decreasing",
    ytitle = "% of All Papers",
    ...
  )

  if (isTRUE(plotly)) {
    insight::check_if_installed("plotly")
    p <- plotly::ggplotly(p = p, tooltip = c("x", "y"))
    if (!is.null(citation)) {
      p <- plotly_citation(p, citation, citation_size = citation_size)
    }
  } else if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

plotly_citation <- function(x, citation, citation_size) {
  plotly::layout(
    x,
    annotations = list(
      # x = 2020,
      y = 100,
      text = citation,
      showarrow = F,
      # xref = 'container',
      yref = 'container',
      xanchor = 'center',
      yanchor = 'top',
      # yshift = -1,
      # automargin = TRUE,
      # margin = list(b=90),
      font = list(size = citation_size))
  )
}
