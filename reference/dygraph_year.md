# Generate a dygraph of journal paper percentages, by country and year

Generate a dygraph of journal paper percentages, by country and year

## Usage

``` r
dygraph_year(data, level = "continent")
```

## Arguments

- data:

  The processed dataframe of data

- level:

  Level of analysis, either country or continent

## Examples

``` r
if (FALSE) { # \dontrun{
data <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
data <- clean_journals_continents(data)
dygraph_year(data)
dygraph_year(data, "country")
} # }
```
