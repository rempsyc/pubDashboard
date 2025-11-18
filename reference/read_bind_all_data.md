# Read local pubDashboard data files and bind them in a single dataframe

Read local pubDashboard data files and bind them in a single dataframe

## Usage

``` r
read_bind_all_data(data_folder = "data", check_duplicate = FALSE)
```

## Arguments

- data_folder:

  The folder in which the data lives

- check_duplicate:

  whether to check article ids with
  [rempsyc::best_duplicate](https://rempsyc.remi-theriault.com/reference/best_duplicate.html)
