# Return post-processed summary of coupling data

Return post-processed summary of coupling data

## Usage

``` r
summarise_coupling_data(x, cran_by_year = TRUE)
```

## Arguments

- x:

  Result of
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md).

- cran_by_year:

  If `TRUE`, implement annual analyses as CRAN would have existed at
  each year; otherwise annual analyses are only for packages uploaded in
  that year alone.

## Value

A `data.frame` of annual summary statistics on coupling instability.
