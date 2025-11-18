# Render complete pubDashboard dashboard

Render complete pubDashboard dashboard

## Usage

``` r
render_dashboard(
  file_name = "dashboard",
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
  ...
)
```

## Arguments

- file_name:

  Desired file name.

- title:

  Desired dashboard title.

- author:

  Desired displayed dashboard author.

- journal_name:

  The list of desired journals (by journal name).

- journal_id:

  The list of desired journals (by OpenAlex ID).

- data_folder:

  Folder where to save the data.

- tab_continent:

  Whether to render the "Continent" tab.

- tab_continent_year:

  Whether to render the "Continent by year" tab.

- tab_continent_journal:

  Whether to render the "Continent by journal" tab.

- tab_country:

  Whether to render the "Country" tab.

- tab_country_journal:

  Whether to render the "Country by journal" tab.

- tab_psychology:

  Whether to render the "Psychology" tab.

- tab_economics:

  Whether to render the "Economics" tab.

- tab_general:

  Whether to render the "General" tab.

- tab_figure1:

  Whether to render the "Figure 1" tab.

- tab_missing:

  Whether to render the "Missing" tab.

- ...:

  Arguments passed to
  [`openalexR::oa_fetch()`](https://docs.ropensci.org/openalexR/reference/oa_fetch.html)

## Examples

``` r
if (FALSE) { # \dontrun{
render_dashboard(
  file_name = "my_dashboard",
  title = "Wonderful Dashboard",
  author = "Rémi Thériault",
  journal_name = c("Journal of Personality and Social Psychology", "Health Psychology"),
  from_publication_date = "2024-01-01",
  tab_figure1 = TRUE
)
} # }
```
