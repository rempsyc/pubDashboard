#' @title Loop `fetch_openalex_pubs` over journals or years
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
#' @param years Desired list to loop over (for option `over = "year"`).
#' @param journal_id The list of desired journals (by OpenAlex ID).
#' @param over Looping over what. Options are `"year"` or `"journal"`.
#' @param from_publication_date Start date (for option `over = "journal"`).
#' @param save Whether to save the data to disk.
#' @param file_suffix What suffix to add to the file (e.g., "_new").
#' @param verbose Passed to [openalexR::oa_fetch()] and defaults to `TRUE.`
#' @param ... Arguments passed to [openalexR::oa_fetch()]
#' @examples
#' \dontrun{
#' lapply_fetch_openalex_pubs(
#'   over = "journal",
#'   journal_id = c("https://openalex.org/S90392387",
#'                  "https://openalex.org/S33443600"),
#'  from_publication_date = "2024-01-01",
#'  to_publication_date = "2024-10-15",
#'  save = FALSE)
#' }
#' @export

lapply_fetch_openalex_pubs <- function(years = 1987:2023,
                                       journal_id,
                                       over = "year",
                                       from_publication_date = "1987-01-01",
                                       save = TRUE,
                                       file_suffix = "",
                                       verbose = TRUE,
                                       ...) {
  if (over == "year") {
    pb <- progress::progress_bar$new(
      format = paste(
        "\nDownloading OpenAlex data for", pubDashboard::journal_field %>%
          dplyr::filter(.data$openalex_id == journal_id) %>%
          dplyr::pull(.data$journal) %>%
          substr(0, 47),
        "[:bar] :current/:total (:percent) [Elapsed: :elapsedfull || Remaining: :eta]\n"
      ),
      total = length(years),
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = FALSE, # If TRUE, clears the bar when finish
      show_after = 0, # Seconds necessary before showing progress bar
      width = 130) # Width of the progress bar
    journal_name <- pubDashboard::journal_field %>%
      dplyr::filter(.data$openalex_id %in% journal_id) %>%
      dplyr::pull(.data$journal_abbr)
    # fetch_openalex_pubs
    lapply(years, function(x) {
      pb$message(as.character(x))
      pb$tick()
      zz <- fetch_openalex_pubs(
        journal_id = journal_id,
        publication_year = x,
        clean_journals_continents = TRUE,
        progress_bar = TRUE,
        verbose = verbose,
        ...)
      if (!is.null(zz) && isTRUE(save)) {
        saveRDS(zz, paste0("data/data_", journal_name, "_", x, file_suffix, ".rds"))
        }
    })
  } else if (over == "journal") {
    pb <- progress::progress_bar$new(
      format = paste(
        "Downloading",
        ":journal [:bar] :current/:total (:percent) [Elapsed: :elapsedfull || Remaining: :eta]"
      ),
      total = length(journal_id),
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = FALSE, # If TRUE, clears the bar when finish
      show_after = 0, # Seconds necessary before showing progress bar
      width = 130) # Width of the progress bar
    # fetch_openalex_pubs
    lapply(journal_id, function(x) {
      jname <- pubDashboard::journal_field %>%
        dplyr::filter(.data$openalex_id %in% x) %>%
        dplyr::pull(.data$journal) %>%
        toupper()
      pb$message(as.character(x))
      pb$tick(tokens = list(journal = jname))
      zz <- fetch_openalex_pubs(
        journal_id = x,
        clean_journals_continents = TRUE,
        progress_bar = TRUE,
        from_publication_date = from_publication_date,
        # to_publication_date = to_publication_date,
        verbose = verbose,
        ...)
      jabb <- pubDashboard::journal_field %>%
        dplyr::filter(.data$openalex_id %in% x) %>%
        dplyr::pull(.data$journal_abbr)
      if (!is.null(zz) && isTRUE(save)) {
        saveRDS(zz, paste0("data/data_", jabb, file_suffix, ".rds"))
      }
    })
  }
  invisible()
}
