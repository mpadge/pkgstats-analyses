# Load one lot of either R or python data

Load one lot of either R or python data

## Usage

``` r
load_pkgstats_data(
  datafile = "pkgstats-results.Rds",
  raw = TRUE,
  latest = TRUE
)
```

## Arguments

- datafile:

  Name of local file containing data to load

- raw:

  If `FALSE`, return tabulated counts of packages per month, otherwise
  return raw data.

- latest:

  If `TRUE`, return data only on latest CRAN version of each package,
  otherwise return data on all releases of all packages.

## Value

The data file, with all dates appropriately converted, and an additional
"month" column added.
