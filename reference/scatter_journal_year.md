# Generate table of journal paper percentages, by continent and year

Generate table of journal paper percentages, by continent and year

## Usage

``` r
scatter_journal_year(
  data,
  method = "lm",
  ymin = 0,
  ymax = 100,
  yby = 20,
  plotly = TRUE,
  citation = NULL,
  citation_size = 15,
  ncol = 4,
  ...
)
```

## Arguments

- data:

  The processed dataframe of data

- method:

  Which method to use for the regression line, either "lm" (default) or
  "loess"

- ymin:

  Minimum value for y-axis

- ymax:

  Maximum value for y-axis

- yby:

  Tick increments for y-axis

- plotly:

  Logical, whether to use plotly for dynamic data visualization

- citation:

  Optionally, a citation to add as a footer

- citation_size:

  Font size of the citation

- ncol:

  How many columns for
  [ggplot2::facet_wrap](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- ...:

  Further arguments passed to
  [rempsyc::nice_scatter](https://rempsyc.remi-theriault.com/reference/nice_scatter.html)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
data <- clean_journals_continents(data)
scatter_journal_year(data)
} # }
```
