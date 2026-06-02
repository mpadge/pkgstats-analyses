# Extract all releases and dates for all pypi packages ever

Extract all releases and dates for all pypi packages ever

## Usage

``` r
all_pypi(
  chunk_size = 1001,
  results_file = "pypi.Rds",
  data_dir = "./data-temp"
)
```

## Arguments

- chunk_size:

  Size of chunks into which parallel job is to be broken. Intermediate
  results are saved for each chunk.

- results_file:

  Name of file (potentially including path) where results are to be
  saved.

- data_dir:

  Directory in which temporary results for each chunk are to be saved
  prior to final aggregation.

## Value

Nothing (data are deposited in 'data_dir\`).

## Note

This function may be stopped at any time, and intermediate results
collated with
[collate_temp_pypi_files](https://mpadge.github.io/pkgstats-analyses/reference/collate_temp_pypi_files.md).
Passing the name of the file constructed by that function as
`results_file` will then re-start analyses where they left off.
