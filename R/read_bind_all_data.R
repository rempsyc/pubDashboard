#' @title Read local pubDashboard data files and bind them in a single dataframe
#' @param data_folder The folder in which the data lives
#' @param check_duplicate whether to check article ids with [rempsyc::best_duplicate]
#' @export
read_bind_all_data <- function(data_folder = "data", check_duplicate = FALSE) {
  filenames <- list.files(paste0(data_folder, "/"), pattern = "data_.*.rds", full.names = TRUE)
  ldf <- lapply(filenames, readRDS)
  df <- dplyr::bind_rows(ldf)
  if (check_duplicate) {
    df <- rempsyc::best_duplicate(df, "id")
  }
  df
}
