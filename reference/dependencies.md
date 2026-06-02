# Extract all dependencies from the "external_calls" component of the main data.

Extract all dependencies from the "external_calls" component of the main
data.

## Usage

``` r
dependencies(x, cran_by_year = TRUE)
```

## Arguments

- x:

  Result of
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md).

- cran_by_year:

  If `TRUE`, calculate dependencies for each year from a full snapshot
  of all latest CRAN packages in that year, regardless of when these
  were uploaded. If `FALSE`, calculate annual values from packages which
  were uploaded in that year.

## Value

A list of 2 `data.frame` objects with annual rows for (1) dependency
data, and (2) Gini coefficient data.
