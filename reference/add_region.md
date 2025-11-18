# Add regions to pubDashboard dataframe

Add regions to pubDashboard dataframe

## Usage

``` r
add_region(data, progress_bar = FALSE)
```

## Arguments

- data:

  The dataframe on which to add region.

- progress_bar:

  Logical, whether to print a progress bar.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- fetch_openalex_pubs(journal_name = "Collabra", pages = 1, per_page = 1)
x <- add_region(x)
names(x)
} # }
```
