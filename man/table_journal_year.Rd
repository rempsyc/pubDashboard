% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_journal_year.R
\name{table_journal_year}
\alias{table_journal_year}
\title{Generate table of journal paper percentages, by journal, continent and year}
\usage{
table_journal_year(data, datatable = TRUE)
}
\arguments{
\item{data}{The processed dataframe of data}

\item{datatable}{Whether to output a \link[DT:datatable]{DT::datatable} HTML table widget
instead of a regular dataframe (defaults to TRUE).}
}
\description{
Generate table of journal paper percentages, by journal, continent and year
}
\examples{
\dontrun{
data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
data <- clean_journals_continents(data)
table_journal_year(data)
}
}
