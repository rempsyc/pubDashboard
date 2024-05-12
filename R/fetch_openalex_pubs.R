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
#' @param journal The list of desired journals.
#' @param ... Arguments passed to [openalexR::oa_fetch()]
#' @examples
#' \dontrun{
#'
#' x <- fetch_openalex_pubs(journal = "Collabra", pages = 1, per_page = 1)
#' names(x)
#' }
#' @export
fetch_openalex_pubs <- function(journal = NULL, ...) {
  sources <- openalexR::oa_fetch(
    entity = "sources",
    display_name.search = journal,
    ...
  )

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

  data
}


