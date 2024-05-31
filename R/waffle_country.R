#' @title Generate a waffle plot made of country flags
#' @param data The processed dataframe of data
#' @param citation Optionally, a citation to add as a footer.
#' @param citation_size Font size of the citation.
#' @examples
#' \dontrun{
#' data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
#' data <- clean_journals_continents(data)
#' waffle_country(data)
#' }
#' @importFrom rlang .data
#' @export
waffle_country <- function(data, citation = NULL, citation_size = NULL) {
  insight::check_if_installed(c("ggflags", "ggplot2", "RColorBrewer"))
  layer <- ggplot2::layer

  x <- table_country(data, datatable = FALSE) %>%
    clean_top(8)

  colors <- suppressWarnings(RColorBrewer::brewer.pal(
    length(unique(x$continent)), "Set2"
  ))

  colours.country <- grDevices::colorRampPalette(colors)(length(x$Country))

  my_prop <- x %>%
    dplyr::mutate(
      Country = countrycode::countrycode(.data$Country, "country.name", "genc2c"),
      Country = tolower(.data$Country)
    ) %>%
    dplyr::filter(!is.na(.data$Country))
  in_map_var <- lapply(seq_len(nrow(my_prop)), \(x) {
    rep(my_prop$Country[x], my_prop$Percentage[x])
  }) %>%
    unlist()
  p <- waffle_country_internal(in_map_var)

  if (!is.null(citation)) {
    p <- gg_citation(p, citation, citation_size = citation_size)
  }

  p
}

#' @noRd
waffle_country_internal <- function(in_map_var, len_x = NA, na_flag = "ac") {
  in_map_var <- data.frame(country = in_map_var)
  my_prop <- in_map_var %>%
    dplyr::count(.data$country, sort = TRUE) %>%
    dplyr::mutate(n2 = round(.data$n / nrow(in_map_var) * 100))
  in_map_var <- lapply(seq_len(nrow(my_prop)), \(x) {
    rep(my_prop$country[x], my_prop$n2[x])
  }) %>%
    unlist()
  # work out grid dimensions
  var_count <- length(in_map_var)
  if (is.na(len_x)) {
    x_count <- ceiling(sqrt(var_count))
  } else {
    x_count <- len_x
  }
  y_count <- ceiling(var_count / x_count)
  # y_count <- 10
  grid_count <- x_count * y_count
  df <-
    data.frame(
      x = rep(1:y_count, each = x_count),
      y = rep(1:x_count, y_count),
      country = c(in_map_var, rep(na_flag, grid_count - var_count))
    )
  country_4legend <- unique(df$country)[unique(df$country) != na_flag]
  p <-
    ggplot2::ggplot(df, ggplot2::aes(.data$x, .data$y, country = .data$country)) +
    ggflags::geom_flag(size = 8.5) +
    ggflags::scale_country(breaks = country_4legend) +
    ggplot2::theme_void() +
    ggplot2::coord_equal() +
    ggplot2::theme(legend.position = "right")
  if (grid_count > var_count) {
    p <-
      p +
      ggplot2::geom_point(
        data = df[var_count:grid_count, ],
        ggplot2::aes(.data$x, .data$y), colour = "white", size = 10
      )
  }
  p
}
