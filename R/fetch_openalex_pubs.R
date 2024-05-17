#' @title Downloads relevant publication data using `openalexR`
#' @details As recommended by the authors of the `openalexR` package,
#'
#' _Before we go any further, we highly recommend you set `openalexR.mailto`
#' option so that your requests go to the polite pool for faster response times.
#' If you have OpenAlex Premium, you can add your API key to the
#' `openalexR.apikey` option as well. These lines best go into .Rprofile with
#' `file.edit("~/.Rprofile")`._
#' ```
#' options(openalexR.mailto = "example@email.com")
#' options(openalexR.apikey = "EXAMPLE_APIKEY")"
#' ```
#' @param journal_name The list of desired journals (by journal name).
#' @param journal_id The list of desired journals (by OpenAlex ID).
#' @param clean_journals_continents Logical, whether to also process the
#'  dataframe with the [pubDashboard::clean_journals_continents] function.
#'  It is set to FALSE by default because on large datasets it can be
#'  very time consuming.
#' @param progress_bar Logical, whether to print a progress bar.
#' @param ... Arguments passed to [openalexR::oa_fetch()]
#' @examples
#' \dontrun{
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' names(x)
#' # Same as:
#' x <- fetch_openalex_pubs(journal_id = "S4210175756", pages = 1, per_page = 1)
#' names(x)
#' }
#' @export
fetch_openalex_pubs <- function(journal_name = NULL,
                                journal_id = NULL,
                                clean_journals_continents = FALSE,
                                progress_bar = FALSE,
                                ...) {
  if (is.null(journal_id)) {
    sources <- openalexR::oa_fetch(
      entity = "sources",
      display_name.search = journal_name
    )
  } else {
    sources <- openalexR::oa_fetch(
      entity = "sources",
      openalex = journal_id
    )
  }

  if (is.null(sources)) {
    stop("Journal name may be incorrect.")
  }

  sources2 <- sources %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      journal = .data$display_name,
      jabbrv = .data$alternate_titles[1],
      jabbrv = dplyr::if_else(is.na(.data$jabbrv), .data$journal, .data$jabbrv),
      alt_title = .data$alternate_titles[2]
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select("journal", "jabbrv", "alt_title")

  sources2 <- sources2 %>%
    dplyr::mutate(journal = dplyr::if_else(
      .data$journal == "Collabra", "Collabra. Psychology", .data$journal)) %>%
    dplyr::left_join(pubDashboard::journal_field, by = "journal") %>%
    dplyr::mutate(jabbrv = .data$journal_abbr) %>%
    dplyr::select(-"journal_abbr")

  data <- openalexR::oa_fetch(
    entity = "works",
    journal = sources$id,
    abstract = FALSE,
    options = list(select = c(
      "title", "id", "doi", "cited_by_count", "concepts",
      "authorships", "publication_date", "primary_location")),
    ...
  )

  data <- data %>%
    dplyr::rename(journal = "so",
                  date = "publication_date") %>%
    dplyr::mutate(date = lubridate::as_date(.data$date),
                  year = lubridate::year(.data$date))

  data <- data %>%
    dplyr::left_join(sources2, by = "journal", relationship = "many-to-many") %>%
    dplyr::select("title", "author", "date", "year", "id",
                  "doi", "cited_by_count", "concepts", "journal", "jabbrv", "alt_title",
                  "original_journal", "field")

  if (clean_journals_continents) {
    data <- clean_journals_continents(data, progress_bar = progress_bar)
  }

  data
}
