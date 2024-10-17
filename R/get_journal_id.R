#' @title Extract `openalexR` journal ID from journal names
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
#' @param verbose Passed to [openalexR::oa_fetch()] and defaults to `TRUE.`
#' @examples
#' \dontrun{
#' x <- get_journal_id(journal_name = "Collabra")
#' x
#' x <- get_journal_id(journal_name = c(
#'   "Social Psychological and Personality Science", "Nature Human Behaviour"))
#' x$id
#' }
#' @export

get_journal_id <- function(journal_name, verbose = TRUE) {
  sources <- openalexR::oa_fetch(
    entity = "sources",
    display_name.search = journal_name,
    verbose = verbose
  )
}
