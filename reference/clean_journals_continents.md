# Clean dataframe, for names of journals and continents

Clean dataframe, for names of journals and continents

## Usage

``` r
clean_journals_continents(data, progress_bar = FALSE)
```

## Arguments

- data:

  The processed dataframe of data

- progress_bar:

  Logical, whether to print a progress bar.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- fetch_openalex_pubs(journal_name = "Collabra",
  pages = 1, per_page = 1, publication_year = 2024)
x <- clean_journals_continents(x)
names(x)
} # }
```
