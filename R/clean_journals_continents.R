#' @title Clean dataframe, for names of journals and continents
#' @param data The processed dataframe of data
#' @param progress_bar Logical, whether to print a progress bar.
#' @examples
#' \dontrun{
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' x <- clean_journals_continents(x)
#' names(x)
#' }
#' @export
clean_journals_continents <- function(data, progress_bar = FALSE) {
  data <- data %>%
    add_region(progress_bar = progress_bar) %>%
    dplyr::mutate(
      journal = clean_journal_names(.data$journal),
      original_journal = .data$journal %in% pubDashboard::journal_field$journal[1:6],
      field = pubDashboard::journal_field$field[match(
        toupper(.data$journal), toupper(pubDashboard::journal_field$journal)
      )],
      field = ifelse(is.na(.data$field), pubDashboard::journal_field$field[match(
        toupper(.data$journal), toupper(pubDashboard::journal_field$journal_abbr))], .data$field),
      continent = factor(.data$continent, levels = continent_order())
    ) %>%
    dplyr::group_by(.data$journal) %>%
    dplyr::mutate(first_Year = min(.data$year), last_year = max(.data$year),
                  year_range = paste0(.data$first_Year, "-", .data$last_year)) %>%
    dplyr::ungroup()

  # Add concepts
  concepts <- lapply(data$concepts, function(x) {
    toString(x$display_name)
    }) %>%
    unlist()

  data$concepts <- concepts

  data
}

#' @noRd
continent_order <- function(short = FALSE) {
  if (short) {
    x <- c("North America", "Europe", "Asia", "Oceania", "Latin America", "Africa")
  } else {
    x <- c("Northern America", "Europe", "Asia", "Oceania", "Latin America and the Caribbean", "Africa")
  }
  x
}

#' @noRd
clean_journal_names <- function(journal) {
  x <- gsub("\u0098", "", journal, fixed = TRUE)
  x <- gsub("\u009c", "", x, fixed = TRUE)
  x <- gsub(":.*", "", x) # removes content after colon
  x <- gsub("/.*", "", x) # removes content after forward slash
  x <- gsub("[(].*", "", x) # removes content in parentheses
  x <- tolower(x)
  x <- tools::toTitleCase(x)
  trimws(x)
}

#' @title Detect missing journals
#' @param data The processed dataframe of data
#' @export
detect_missing_journals <- function(data) {
  data.frame(journal = pubDashboard::journal_field$journal) %>%
    dplyr::mutate(found = toupper(pubDashboard::journal_field$journal) %in%
                    toupper(unique(data$journal)),
                  found = ifelse(.data$found == FALSE, toupper(pubDashboard::journal_field$journal_abbr) %in%
                                   toupper(unique(data$jabbrv)), .data$found)) %>%
    dplyr::arrange(.data$found)
}
