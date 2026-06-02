# Transform data to form new variables

The raw data contain many related raw count variables. This function
converts many of these to relative proportions.

## Usage

``` r
transform_pkgstats_data(x)
```

## Arguments

- x:

  Result of
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md)
  with `raw = TRUE`.

## Value

Transformed version of input, so some variables transformed and new
variables added.
