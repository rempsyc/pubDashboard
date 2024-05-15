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
#'
#' @param journal_name The list of desired journals (by journal name).
#' @param journal_id The list of desired journals (by OpenAlex ID).
#' @param ... Arguments passed to [openalexR::oa_fetch()]
#' @examples
#' \dontrun{
#'
#' x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
#' names(x)
#' # Same as:
#' x <- fetch_openalex_pubs(journal_id = "S4210175756", pages = 1, per_page = 1)
#' names(x)
#' }
#' @export
fetch_openalex_pubs <- function(journal_name = NULL, journal_id = NULL, ...) {
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
    left_join(journal_field, by = "journal")

  data <- openalexR::oa_fetch(
    entity = "works",
    journal = sources$id,
    abstract = FALSE,
    ...
  )

  data <- data %>%
    dplyr::rename(journal = so,
                  date = publication_date) %>%
    dplyr::mutate(date = lubridate::as_date(date),
                  year = lubridate::year(date))


  data <- dplyr::left_join(data, sources2, by = "journal")

  data
}



