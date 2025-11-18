# Generate table of journal paper percentages, by country

Generate table of journal paper percentages, by country

## Usage

``` r
table_country(data, datatable = TRUE)
```

## Arguments

- data:

  The processed dataframe of data

- datatable:

  Whether to output a
  [DT::datatable](https://rdrr.io/pkg/DT/man/datatable.html) HTML table
  widget instead of a regular dataframe (defaults to TRUE).

## Examples

``` r
if (FALSE) { # \dontrun{
data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
data <- clean_journals_continents(data)
table_country(data)
} # }
```
