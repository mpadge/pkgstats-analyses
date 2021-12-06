
#' Calculate afferent and efferent couplings between packages
#'
#' @param x Result of \link{load_pkgstats_data}.
#' @param y Year for which coupling data are to be obtained
#' @param summarise if `TRUE`, convert full `data.frame` of results to summary
#' statistics.
#' @return The results
#' @export
couplings <- function (x, year = 2015, summarise = TRUE) {

    x <- x |>
        dplyr::filter (year <= !!year) |>
        dplyr::group_by (package) |>
        dplyr::slice_max (date)

    x <- x [which (!(is.na (x$external_calls) | x$external_calls == "")), ]
    x <- x [which (!(x$imports == "NA" | nchar (x$imports) == 0L)), ]

    if (nrow (x) == 0L) {
        return (rep (NA, 5))
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

                        out <- data.frame (from = this_pkg,
                                           to = out [i_ctb, 1],
                                           n_total = n_total [i_ctb],
                                           n_unique = n_unique [i_ctb])

                        return (out)
    })

    deps <- do.call (rbind, deps)

    deps$from <- gsub ("^[[:punct:]]+", "", deps$from)
    deps$to <- gsub ("^[[:punct:]]+", "", deps$to)
    deps <- deps [which (nchar (deps$from) > 1L &
                         nchar (deps$to) > 1L), ]

    # outward or EFFERENT couplings:
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

    deps <- dplyr::full_join (deps_from, deps_to, by = "package")
    deps$n_total_from [which (is.na (deps$n_total_from))] <- 0L
    deps$n_unique_from [which (is.na (deps$n_unique_from))] <- 0L
    deps$n_total_to [which (is.na (deps$n_total_to))] <- 0L
    deps$n_unique_to [which (is.na (deps$n_unique_to))] <- 0L

    deps$instability_total <- deps$n_total_to / (deps$n_total_to + deps$n_total_from)
    deps$instability_unique <- deps$n_unique_to / (deps$n_unique_to + deps$n_unique_from)

    if (summarise) {

        deps <- c (
            year = year,
            tot_med = stats::median (deps$instability_total),
            tot_mean = mean (deps$instability_total),
            un_med = stats::median (deps$instability_unique),
            un_mean = mean (deps$instability_unique)
            )
    }

    return (deps)
}

#' Return post-processed summary of coupling data
#'
#' @inheritParams couplings
#' @return A `data.frame` of annual summary statistics on coupling instability.
#' @export
summarise_coupling_data <- function (x) {

    m_coupling_summary_internal (x)
}

coupling_summary_internal <- function (x) {

    x <- load_pkgstats_data (datafile, raw = TRUE, latest = FALSE)
    years <- sort (unique (x$year))
    cp <- vapply (years, function (i)
                  couplings (x, i, summarise = TRUE),
                  numeric (5))
    cp <- data.frame (t (cp))

    names (cp) <- c ("year", "total_median", "total_mean",
                     "unique_median", "unique_mean")
    cp <- cp [which (!is.na (cp$year)), ]

    class (cp$year) <- as.integer (cp$year)

    return (cp)
}
m_coupling_summary_internal <- memoise::memoise (coupling_summary_internal)
