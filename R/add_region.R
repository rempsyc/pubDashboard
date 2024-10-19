#' @title Add regions to pubDashboard dataframe
#' @param data The dataframe on which to add region.
#' @param progress_bar Logical, whether to print a progress bar.
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' x <- add_region(x)
#' names(x)
#' }
#' @export
add_region <- function(data,
                       progress_bar = FALSE) {
  if (progress_bar) {
    pb <- progress::progress_bar$new(
      format = paste(
        "Processing...",
        "[:bar] :current/:total (:percent) [Elapsed: :elapsedfull || Remaining: :eta]"
      ),
      total = 2,
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = FALSE, # If TRUE, clears the bar when finish
      show_after = 0, # Seconds necessary before showing progress bar
      width = 100 # Width of the progress bar
    )
  } else {
    pb <- NULL
  }

  pipe_progress(NULL, "1. Extracting author information...", pb = pb)

  data_first_author <- lapply(data$author, function (x) {
    x <- as.data.frame(x)[1, ]
    x <- as.data.frame(x)
    x
    }) %>% dplyr::bind_rows() %>%
    dplyr::select(dplyr::any_of(c("au_display_name", "author_position", "institution_display_name",
                                  "institution_country_code", "au_affiliation_raw")))

  data_first_author$institution_country_code <-
    as.character(data_first_author$institution_country_code)

  # pipe_progress(NULL, "2. Binding datasets...", pb = pb)
  pipe_progress(NULL, "3. Adding region...", pb = pb)

  data <- data %>%
    dplyr::bind_cols(data_first_author) %>%
    dplyr::rename(authors = "author",
                  author = "au_display_name",
                  institution = "institution_display_name",
                  country_code = "institution_country_code",
                  address = "au_affiliation_raw") %>%
    # pipe_progress("3. Adding region...", pb = pb) %>%
    dplyr::mutate(
      country = countrycode::countrycode(.data$country_code, "genc2c", "country.name"),
      region = countrycode::countrycode(
        .data$country_code, "genc2c", "un.regionsub.name", custom_match = c("TW" = "Eastern Asia")),
      continent = countrycode::countrycode(
        .data$country_code, "genc2c", "continent"
        ),
      continent = dplyr::case_when(
        .data$continent == "Americas" ~ .data$region,
        TRUE ~ .data$continent))
  data
}

pipe_progress <- function(x, step, pb) {
  if (!is.null(pb)) {
    pb$message(step)
    pb$tick()
  }
  invisible(x)
}

extract_author_info <- function(author_list,
                                object = "country",
                                author_position = "first",
                                progress_bar = FALSE,
                                pb = NULL) {
  # extract_author_info(author_list = author_list, object = "country")
  # extract_author_info(author_list = author_list, object = "address")
  if (progress_bar) {
    pb$tick()
  }
  author_list <- as.data.frame(author_list)
  if (object == "country") {
    object_name <- "institution_country_code"
  } else if (object == "address") {
    object_name <- "au_affiliation_raw"
  } else if (object == "author") {
    object_name <- "au_display_name"
  }
  if("author_position" %in% names(author_list)) {
    out <- author_list %>%
      dplyr::filter(author_position == !!author_position) %>%
      dplyr::pull(object_name)
  } else {
    out <- NA
  }
  out
}
