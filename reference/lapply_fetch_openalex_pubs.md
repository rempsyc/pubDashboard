# Loop `fetch_openalex_pubs` over journals or years

Loop `fetch_openalex_pubs` over journals or years

## Usage

``` r
lapply_fetch_openalex_pubs(
  years = 1987:2023,
  journal_id,
  over = "year",
  from_publication_date = "1987-01-01",
  save = TRUE,
  file_suffix = "",
  verbose = TRUE,
  ...
)
```

## Arguments

- years:

  Desired list to loop over (for option `over = "year"`).

- journal_id:

  The list of desired journals (by OpenAlex ID).

- over:

  Looping over what. Options are `"year"` or `"journal"`.

- from_publication_date:

  Start date (for option `over = "journal"`).

- save:

  Whether to save the data to disk.

- file_suffix:

  What suffix to add to the file (e.g., "\_new").

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
lapply_fetch_openalex_pubs(
  over = "journal",
  journal_id = c("https://openalex.org/S90392387",
                 "https://openalex.org/S33443600"),
 from_publication_date = "2024-01-01",
 to_publication_date = "2024-10-15",
 save = FALSE)
} # }
```
