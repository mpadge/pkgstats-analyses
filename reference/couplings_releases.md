# Get couplings for each release of each package

These are couplings between packages but calculated for each release, to
enable examination of changes in coupling stability across releases.
This takes around 15 minutes to calculate, so is not worth doing in
parallel here.

## Usage

``` r
couplings_releases(x)
```

## Arguments

- x:

  Result of
  [load_pkgstats_data](https://mpadge.github.io/pkgstats-analyses/reference/load_pkgstats_data.md).
