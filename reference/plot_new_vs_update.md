# Plot time series of rates of new submissions versus updated packages

Plot time series of rates of new submissions versus updated packages

## Usage

``` r
plot_new_vs_update(
  datafile = "pkgstats-results.Rds",
  bimonthly = FALSE,
  start_date = "2018-01-01",
  type = "lines",
  lwd = 1
)
```

## Arguments

- datafile:

  Name of local file containing data to load

- bimonthly:

  If `TRUE`, aggregate data first into bimonthly intervals (which
  generally produces a nicer looking plot).

- start_date:

  First date to display, or set to `NULL` to display full range.

- type:

  Either "bars" for a bar (column) graph, or "lines" for a line graph.

- lwd:

  For type = "lines" only.
