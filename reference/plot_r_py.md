# Time series plot for R versus python

Time series plot for R versus python

## Usage

``` r
plot_r_py(
  x_r,
  x_p,
  bimonthly = FALSE,
  start_date = "2018-01-01",
  type = "bars",
  lwd = 1
)
```

## Arguments

- x_r:

  CRAN data load with
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md)
  using `raw = FALSE`.

- x_p:

  pypi data load with
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md)
  using `raw = FALSE`.

- bimonthly:

  If `TRUE`, aggregate data first into bimonthly intervals (which
  generally produces a nicer looking plot).

- start_date:

  First date to display, or set to `NULL` to display full range.

- type:

  Either "bars" for a bar (column) graph, or "lines" for a line graph.

- lwd:

  For type = "lines" only.

## Value

A ggplot2 object (invisibly)
