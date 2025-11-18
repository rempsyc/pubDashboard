# Downloads relevant publication data using `openalexR`

Downloads relevant publication data using `openalexR`

## Usage

``` r
fetch_openalex_pubs(
  journal_name = NULL,
  journal_id = NULL,
  clean_journals_continents = FALSE,
  progress_bar = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- journal_name:

  The list of desired journals (by journal name).

- journal_id:

  The list of desired journals (by OpenAlex ID).

- clean_journals_continents:

  Logical, whether to also process the dataframe with the
  [clean_journals_continents](https://rempsyc.github.io/pubDashboard/reference/clean_journals_continents.md)
  function. It is set to FALSE by default because on large datasets it
  can be very time consuming.

- progress_bar:

  Logical, whether to print a progress bar.

- verbose:

  Passed to
  [`openalexR::oa_fetch()`](https://docs.ropensci.org/openalexR/reference/oa_fetch.html)
  and defaults to `TRUE.`

- ...:

  Arguments passed to
  [`openalexR::oa_fetch()`](https://docs.ropensci.org/openalexR/reference/oa_fetch.html)

## Details

As recommended by the authors of the `openalexR` package,

*Before we go any further, we highly recommend you set
`openalexR.mailto` option so that your requests go to the polite pool
for faster response times. If you have OpenAlex Premium, you can add
your API key to the `openalexR.apikey` option as well. These lines best
go into .Rprofile with `file.edit("~/.Rprofile")`.*

    options(openalexR.mailto = "example@email.com")
    options(openalexR.apikey = "EXAMPLE_APIKEY")"

## Examples

``` r
if (FALSE) { # \dontrun{
x <- fetch_openalex_pubs(journal_name = "Collabra",
  pages = 1, per_page = 1, publication_year = 2024)
names(x)
# Same as:
x <- fetch_openalex_pubs(journal_id = "S4210175756",
  pages = 1, per_page = 1, publication_year = 2024)
names(x)
} # }
```
