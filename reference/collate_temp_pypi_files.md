# Collate all temporary files produced by the chunked parallel run of `all_pypi`.

Collate all temporary files produced by the chunked parallel run of
`all_pypi`.

## Usage

``` r
collate_temp_pypi_files(results_file = "pypi.Rds", data_dir = "./data-temp")
```

## Arguments

- results_file:

  Name of file (potentially including path) where results are to be
  saved.

- data_dir:

  Directory in which temporary results for each chunk are to be saved
  prior to final aggregation.

## Value

Collated results, which are also (re-)saved to specified file
