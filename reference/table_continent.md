# Generate table of journal paper percentages, by continent

Generate table of journal paper percentages, by continent

## Usage

``` r
table_continent(data, datatable = TRUE)
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
table_continent(data)
} # }
```
