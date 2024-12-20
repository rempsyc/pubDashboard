% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_journals_continents.R
\name{clean_journals_continents}
\alias{clean_journals_continents}
\title{Clean dataframe, for names of journals and continents}
\usage{
clean_journals_continents(data, progress_bar = FALSE)
}
\arguments{
\item{data}{The processed dataframe of data}

\item{progress_bar}{Logical, whether to print a progress bar.}
}
\description{
Clean dataframe, for names of journals and continents
}
\examples{
\dontrun{
x <- fetch_openalex_pubs(journal_name = "Collabra",
  pages = 1, per_page = 1, publication_year = 2024)
x <- clean_journals_continents(x)
names(x)
}
}
