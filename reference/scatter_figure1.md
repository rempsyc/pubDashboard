# Generate table of journal paper percentages, by continent and year

Generate table of journal paper percentages, by continent and year

## Usage

``` r
scatter_figure1(data, method = "lm", original = TRUE, plotly = TRUE, ...)
```

## Arguments

- data:

  The processed dataframe of data

- method:

  Which method to use for the regression line, either "lm" (default) or
  "loess".

- original:

  Logical; if `TRUE`, attempts to mimic Arnett's (2008) Figure 1 in
  style.

- plotly:

  Logical, whether to use plotly for dynamic data visualization.

- ...:

  Further arguments passed to
  [rempsyc::nice_scatter](https://rempsyc.remi-theriault.com/reference/nice_scatter.html)

## Examples

``` r
if (FALSE) { # \dontrun{
journals <- c("Developmental Psychology",
             "Journal of Personality and Social Psychology",
             "Journal of Abnormal Psychology",
             "Journal of Family Psychology",
             "Health Psychology",
             "Journal of Educational Psychology"
)

data <- fetch_openalex_pubs(journal_name = journals, pages = 10)
data <- clean_journals_continents(data)
scatter_figure1(data)
} # }
```
