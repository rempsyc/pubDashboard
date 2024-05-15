#' @title Render complete pubDashboard dashboard
#' @param file_name Desired file name.
#' @param title Desired dashboard title.
#' @param author Desired displayed dashboard author.
#' @param journal_name The list of desired journals (by journal name).
#' @param journal_id The list of desired journals (by OpenAlex ID).
#' @param data_folder Folder where to save the data.
#' @param tab_continent Whether to render the "Continent" tab.
#' @param tab_continent_year Whether to render the "Continent by year" tab.
#' @param tab_continent_journal Whether to render the "Continent by journal" tab.
#' @param tab_country Whether to render the "Country" tab.
#' @param tab_country_journal Whether to render the "Country by journal" tab.
#' @param tab_psychology Whether to render the "Psychology" tab.
#' @param tab_economics Whether to render the "Economics" tab.
#' @param tab_general Whether to render the "General" tab.
#' @param tab_figure1 Whether to render the "Figure 1" tab.
#' @param tab_missing Whether to render the "Missing" tab.
#' @param ... Arguments passed to [openalexR::oa_fetch()]
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @examples
#' \dontshow{
#' .old_wd <- setwd(tempdir())
#' }
#' \dontrun{
#' render_dashboard(
#'   file_name = "my_dashboard",
#'   title = "Wonderful Dashboard",
#'   author = "Rémi Thériault",
#'   journal_name = c("Journal of Personality and Social Psychology", "Health Psychology"),
#'   from_publication_date = "2024-01-01",
#'   tab_figure1 = TRUE
#' )
#' }
#' \dontshow{
#' unlink("data/data_new.rds")
#' setwd(.old_wd)
#' }
#' @export

render_dashboard <- function(file_name = "dashboard",
                             title = "title",
                             author = "author",
                             journal_name = NULL,
                             journal_id = NULL,
                             data_folder = "data",
                             tab_continent = TRUE,
                             tab_continent_year = TRUE,
                             tab_continent_journal = TRUE,
                             tab_country = TRUE,
                             tab_country_journal = TRUE,
                             tab_psychology = FALSE,
                             tab_economics = FALSE,
                             tab_general = FALSE,
                             tab_figure1 = FALSE,
                             tab_missing = TRUE,
                             ...) {
  insight::check_if_installed(c("rstudioapi", "rmarkdown"))
  rmarkdown::render(system.file("dashboard.Rmd", package = "pubDashboard"),
    output_dir = getwd(),
    output_file = file_name
  )
  if (interactive()) {
    rstudioapi::viewer(paste0(file_name, ".html"))
  }
}
