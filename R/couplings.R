
#' Calculate afferent and efferent couplings between packages
#'
#' @param x Result of \link{load_pkgstats_data}.
#' @param year Year for which coupling data are to be obtained
#' @param summarise if `TRUE`, convert full `data.frame` of results to summary
#' statistics.
#' @return The results
#' @export
couplings <- function (x, year = 2015, summarise = TRUE) {

    cran_by_year <- !is.na (year)

    if (cran_by_year) {
        x <- x |>
            dplyr::filter (year <= !!year) |>
            dplyr::group_by (package) |>
            dplyr::slice_max (date)
    }

    x <- x [which (!(is.na (x$external_calls) | x$external_calls == "")), ]
    x <- x [which (!(x$imports == "NA" | nchar (x$imports) == 0L)), ]

    if (nrow (x) == 0L) {
        return (rep (NA, 7))
    }

    recommended <- recommended_pkgs ()

    deps <- lapply (seq (nrow (x)), function (i) {

                        # a few have rogue colons at start:
                        ex <- gsub ("^\\:", "", x$external_calls [i])
                        out <- strsplit (strsplit (ex, ",") [[1]], "\\:")
                        lens <- vapply (out, length, integer (1))
                        out <- do.call (rbind, out [which (lens == 3)])
                        
                        this_pkg <- x$package [i]
                        out <- out [which (out [, 1] != this_pkg), , drop = FALSE]

                        n_total <- as.integer (out [, 2])
                        n_unique <- as.integer (out [, 3])
                        pkg <- out [, 1]

                        i_base <- which (pkg == "base")
                        i_rcmd <- which (pkg %in% recommended)
                        i_ctb <- seq_along (pkg) [-c (i_base, i_rcmd)]

                        if (length (i_ctb) == 0L)
                            return (NULL)

                        out <- data.frame (year = x$year [i],
                                           from = this_pkg,
                                           to = out [i_ctb, 1],
                                           n_total = n_total [i_ctb],
                                           n_unique = n_unique [i_ctb])

                        return (out)
    })

    deps <- do.call (rbind, deps)
    if (cran_by_year) {
        deps$year <- year
    }

    deps$from <- gsub ("^[[:punct:]]+", "", deps$from)
    deps$to <- gsub ("^[[:punct:]]+", "", deps$to)
    deps <- deps [which (nchar (deps$from) > 1L &
                         nchar (deps$to) > 1L), ]

    # outward or EFFERENT couplings:
    if (cran_by_year) {

        deps_from <- deps |>
            dplyr::group_by (from) |>
            dplyr::summarise (n_total = sum (n_total),
                              n_unique = sum (n_unique)) |>
            dplyr::rename (package = from,
                           n_total_from = n_total,
                           n_unique_from = n_unique)

        # inward or AFFERENT couplings:
        deps_to <- deps |>
            dplyr::group_by (to) |>
            dplyr::summarise (n_total = sum (n_total),
                              n_unique = sum (n_unique)) |>
            dplyr::rename (package = to,
                           n_total_to = n_total,
                           n_unique_to = n_unique)
    } else {

        deps_from <- deps |>
            dplyr::group_by (year, from) |>
            dplyr::summarise (n_total = sum (n_total),
                              n_unique = sum (n_unique)) |>
            dplyr::rename (package = from,
                           n_total_from = n_total,
                           n_unique_from = n_unique)

        # inward or AFFERENT couplings:
        deps_to <- deps |>
            dplyr::group_by (year, to) |>
            dplyr::summarise (n_total = sum (n_total),
                              n_unique = sum (n_unique)) |>
            dplyr::rename (package = to,
                           n_total_to = n_total,
                           n_unique_to = n_unique)
    }

    if (cran_by_year) {
        deps <- dplyr::full_join (deps_from, deps_to, by = "package")
    } else {
        deps <- dplyr::full_join (deps_from, deps_to, by = c ("package", "year"))
    }
    deps$n_total_from [which (is.na (deps$n_total_from))] <- 0L
    deps$n_unique_from [which (is.na (deps$n_unique_from))] <- 0L
    deps$n_total_to [which (is.na (deps$n_total_to))] <- 0L
    deps$n_unique_to [which (is.na (deps$n_unique_to))] <- 0L

    deps$instability_total <- deps$n_total_from / (deps$n_total_to + deps$n_total_from)
    deps$instability_unique <- deps$n_unique_from / (deps$n_unique_to + deps$n_unique_from)

    if (summarise) {

        if (cran_by_year) {

            deps <- c (
                year = year,
                tot_med = stats::median (deps$instability_total),
                tot_mean = mean (deps$instability_total),
                tot_se = sd (deps$instability_total) / nrow (deps),
                un_med = stats::median (deps$instability_unique),
                un_mean = mean (deps$instability_unique),
                un_se = sd (deps$instability_unique) / nrow (deps)
                )
        } else {

            deps <- deps |>
                dplyr::group_by (year) |>
                dplyr::summarise (
                    tot_med = stats::median (instability_total),
                    tot_mean = mean (instability_total),
                    tot_se = sd (instability_total) / length (instability_total),
                    un_med = stats::median (instability_unique),
                    un_mean = mean (instability_unique),
                    un_se = sd (instability_unique) / length (instability_total))

        }
    }

    return (deps)
}

#' Return post-processed summary of coupling data
#'
#' @inheritParams couplings
#' @return A `data.frame` of annual summary statistics on coupling instability.
#' @export
summarise_coupling_data <- function (x, cran_by_year = TRUE) {

    m_coupling_summary_internal (x, cran_by_year)
}

coupling_summary_internal <- function (x, cran_by_year) {

    if (cran_by_year) {

        years <- sort (unique (x$year))
        cp <- vapply (years, function (i)
                      couplings (x, i, summarise = TRUE),
                      numeric (7))
        cp <- data.frame (t (cp))

    } else {

        cp <- couplings (x, year = NA)
    }

    names (cp) <- c ("year", "total_median", "total_mean", "total_se",
                     "unique_median", "unique_mean", "unique_se")
    cp <- cp [which (!is.na (cp$year)), ]

    class (cp$year) <- as.integer (cp$year)

    return (cp)
}
m_coupling_summary_internal <- memoise::memoise (coupling_summary_internal)

#' Get couplings for each release of each package
#'
#' These are couplings between packages but calculated for each release, to
#' enable examination of changes in coupling stability across releases. This
#' takes around 15 minutes to calculate, so is not worth doing in parallel here.
#'
#' @inheritParams couplings
#' @export
couplings_releases <- function (x) {

    deps <- coupling_dependencies (x)

    base_rec <- c ("base", recommended_pkgs ())

    pkgs <- unique (deps$from)
    latest <- max (deps$date)
    n <- pbapply::pblapply (pkgs, function (p) {

        deps_p <- deps [which (deps$from == p | deps$to == p), ]
        deps_from <- deps_p [which (deps_p$from == p &
                                    !deps_p$to %in% base_rec), ]
        deps_to <- deps_p [which (deps_p$to == p), ]
        # dependences have to be mapped to intervals between releases up to current
        # date:
        dates <- c (sort (unique (deps_p$date)), latest)
        # Then remove that date, so only consider deps subsequent to each release:
        dates <- dates [-1]

        if (length (dates) < 2L) {
            return (matrix (nrow = 0L, ncol = 5L))
        }

        out <- vapply (dates, function (d) {

            deps_from_d <- deps_from [deps_from$date == d, ]

            deps_to_d <- deps_to [deps_to$date <= d, ]
            deps_to_d <- deps_to_d |>
                dplyr::group_by (from) |>
                dplyr::slice_max (date) |>
                dplyr::ungroup ()

            c ("efferent_unique" = sum (deps_from_d$n_unique),
               "efferent_total" = sum (deps_from_d$n_total),
               "afferent_unique" = sum (deps_to_d$n_unique),
               "afferent_total" = sum (deps_to_d$n_total),
               "afferent_npkgs" = length (deps_to_d$from),
               "afferent_fns_per_pkg" = mean (deps_to_d$n_unique))
        }, numeric (6))

        out <- cbind (package = rep (p, length (dates)),
                      seq = seq_along (dates),
                      date = dates,
                      t (out))

        return (out)
    })
    index <- which (vapply (n, nrow, integer (1)) > 1L)
    n <- do.call (rbind, n [index])

    n <- data.frame (package = n [, 1],
                     seq = as.integer (n [, 2]),
                     date = lubridate::as_date (as.integer (n [, 3])),
                     efferent_unique = as.integer (n [, 4]),
                     efferent_total = as.integer (n [, 5]),
                     afferent_unique = as.integer (n [, 6]),
                     afferent_total = as.integer (n [, 7]),
                     afferent_npkgs = as.integer (n [, 8]),
                     afferent_fns_per_pkg = as.numeric (n [, 9]))

    n$instability_unique <- n$efferent_unique / (n$efferent_unique + n$afferent_unique)
    n$instability_total <- n$efferent_total / (n$efferent_total + n$afferent_total)

    n$instability_unique [!is.finite (n$instability_unique)] <- NA
    n$instability_total [!is.finite (n$instability_total)] <- NA
    n$afferent_fns_per_pkg [!is.finite (n$afferent_fns_per_pkg)] <- NA

    return (n)
}

#' Convert raw data (`x`) into `data.frame` of coupling dependencies as `from`
#' and `to` columns for each package.
#'
#' @inheritParams couplings
#' @export
coupling_dependencies <- function (x) {

    deps <- lapply (seq (nrow (x)), function (i) {
        # a few have rogue colons at start:
        ex <- gsub ("^\\:", "", x$external_calls [i])
        if (is.na (ex) | ex == "") {
            return (NULL)
        }
        out <- strsplit (strsplit (ex, ",") [[1]], "\\:")
        lens <- vapply (out, length, integer (1))
        out <- do.call (rbind, out [which (lens == 3)])

        this_pkg <- x$package [i]
        out <- out [which (out [, 1] != this_pkg), , drop = FALSE]

        out <- cbind (out,
                      rep (x$package [i], nrow (out)),
                      rep (x$version [i], nrow (out)),
                      rep (x$month [i], nrow (out)))

        return (out)
    })

    deps <- do.call (rbind, deps)

    # manual cleaning until https://github.com/ropensci-review-tools/pkgstats/issues/33
    # '\' is punct, but 'n' is not, so first get rid of '\n':
    deps [, 1] <- gsub ("^\\\\\\\\n", "", deps [, 1])
    deps [, 1] <- gsub ("^[[:punct:]]*", "", deps [, 1])
    deps <- deps [which (deps [, 1] != ""), ]

    deps <- data.frame (from = deps [, 4],
                        to = deps [, 1],
                        version = deps [, 5],
                        date =  lubridate::as_date (as.integer (deps [, 6])),
                        n_total = as.integer (deps [, 2]),
                        n_unique = as.integer (deps [, 3]))
    
    return (deps)
}
