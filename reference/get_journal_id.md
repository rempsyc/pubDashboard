# Extract `openalexR` journal ID from journal names

Extract `openalexR` journal ID from journal names

## Usage

``` r
get_journal_id(journal_name, verbose = TRUE)
```

## Arguments

- journal_name:

  The list of desired journals (by journal name).

- verbose:

  Passed to
  [`openalexR::oa_fetch()`](https://docs.ropensci.org/openalexR/reference/oa_fetch.html)
  and defaults to `TRUE.`

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
x <- get_journal_id(journal_name = "Collabra")
x
x <- get_journal_id(journal_name = c(
  "Social Psychological and Personality Science", "Nature Human Behaviour"))
x$id
} # }
```
