
#' Get list of current recommended R packages
#'
#' @return Vector of names of recommended packages
#' @export
recommended_pkgs <- function () {

    if (!curl::has_internet ()) {

        return (c ("compiler", "datasets", "grDevices",
                   "graphics", "grid", "methods", "parallel",
                   "profile", "splines", "stats", "stats4",
                   "tcltk", "tools", "translations", "utils"))
    }

    u <- "https://github.com/wch/r-source/tree/trunk/src/library"
    r <- rvest::read_html (u)

    a <- rvest::html_elements (r, "div") |>
        rvest::html_elements ("a")

    titles <- rvest::html_attr (a, "title")
    classes <- rvest::html_attr (a, "class")
    recommended <- titles [grep ("primary", classes)]
    index <- which (!(is.na (recommended) |
                      grepl ("\\.|^Rec|^base", recommended)))
    recommended <- recommended [index]

    return (recommended)
}

#' Extract all dependencies from the "external_calls" component of the main
#' data.
#'
#' @param x Result of \link{load_pkgstats_data}.
#' @param cran_by_year If `TRUE`, calculate dependencies for each year from a
#' full snapshot of all latest CRAN packages in that year, regardless of when
#' these were uploaded. If `FALSE`, calculate annual values from packages which
#' were uploaded in that year.
#' @return A list of 2 `data.frame` objects with annual rows for (1) dependency
#' data, and (2) Gini coefficient data.
#' @export
dependencies <- function (x, cran_by_year = TRUE) {

    recommended <- recommended_pkgs ()

    if (cran_by_year) {
        years <- sort (unique (x$year))
        deps <- lapply (years, function (y)
                        dependencies_one_year (x, recommended, y, cran_by_year))
        deps0 <- deps
        gini <- do.call (rbind, lapply (deps, function (i) i$gini))
        deps <- do.call (rbind, lapply (deps, function (i) i$deps))
    } else {
        deps <- dependencies_one_year (x, recommended, cran_by_year = cran_by_year)
    }

    return (deps)
}

#' If !cran_by_year, this calculates the whole thing in one call.
#' @noRd
dependencies_one_year <- function (x, recommended, year = 2018, cran_by_year = TRUE) {

    # suppress no visible binding notes:
    package <- n_total_base <- n_total_rmcd <- n_total_ctb <-
        n_unique <- base_total <- recommended_total <-
        base_unique <- recommended_unique <- contributed_unique <-
        n_unique_base <- n_unique_ctb <- contributed_total <-
        n_total <- value <- total <- NULL

    if (cran_by_year) {
        x_y <- x |>
            dplyr::filter (year <= !!year) |>
            dplyr::group_by (package) |>
            dplyr::slice_max (date)
    } else {
        x_y <- x
    }

    deps <- lapply (seq (nrow (x_y)), function (i) {
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

                        n <- nrow (out)

                        return (c (
                            package = this_pkg,
                            month = x$month [i],
                            n_deps = length (i_ctb),

                            n_total_base <- sum (n_total [i_base]),
                            n_total_rmcd <- sum (n_total [i_rcmd]),
                            n_total_ctb <- sum (n_total [i_ctb]),

                            n_unique_base <- sum (n_unique [i_base]),
                            n_unique_rmcd <- sum (n_unique [i_rcmd]),
                            n_unique_ctb <- sum (n_unique [i_ctb])
                        ))
    })

    deps <- do.call (rbind, deps)

    deps_full <- data.frame (package = deps [, 1],
                             date =  lubridate::as_date (as.integer (deps [, 2])),
                             n_deps = as.integer (deps [, 3]),
                             n_total_base = as.integer (deps [, 4]),
                             n_total_rmcd = as.integer (deps [, 5]),
                             n_total_ctb = as.integer (deps [, 6]),
                             n_unique_base = as.integer (deps [, 7]),
                             n_unique_rmcd = as.integer (deps [, 8]),
                             n_unique_ctb = as.integer (deps [, 9]))

    deps <- deps_full |>
        dplyr::mutate (year = lubridate::year (date),
                       n_total = n_total_base + n_total_rmcd + n_total_ctb,
                       n_unique = n_unique_base + n_unique_rmcd + n_unique_ctb)

    gini_lin <- function (n) {
        sum (abs (dist (n, method = "manhattan"))) /
            (2 * length (n) ^ 2 * mean (n, na.rm = TRUE))
    }
    gini_log <- function (n) {
        n <- log10 (n)
        n <- n [which (is.finite (n))]
        sum (abs (dist (n, method = "manhattan"))) /
            (2 * length (n) ^ 2 * mean (n, na.rm = TRUE))
    }

    if (!cran_by_year) {

        deps <- dplyr::group_by (deps, year) |>
            dplyr::summarise (base_total = sum (n_total_base) / sum (n_total),
                              recommended_total = sum (n_total_rmcd) / sum (n_total),
                              contributed_total = sum (n_total_ctb) / sum (n_total),
                              base_unique = sum (n_unique_base) / sum (n_unique),
                              recommended_unique = sum (n_unique_rmcd) / sum (n_unique),
                              contributed_unique = sum (n_unique_ctb) / sum (n_unique),
                              gini_lin_deps = gini_lin (n_deps),
                              gini_log_deps = gini_log (n_deps),
                              gini_lin_tot = gini_lin (n_total_ctb),
                              gini_lin_unique = gini_lin (n_unique_ctb),
                              gini_log_tot = gini_log (n_total_ctb),
                              gini_log_unique = gini_log (n_unique_ctb))
    } else {

        deps <- deps |>
            dplyr::summarise (year = !!year,
                              base_total = sum (n_total_base) / sum (n_total),
                              recommended_total = sum (n_total_rmcd) / sum (n_total),
                              contributed_total = sum (n_total_ctb) / sum (n_total),
                              base_unique = sum (n_unique_base) / sum (n_unique),
                              recommended_unique = sum (n_unique_rmcd) / sum (n_unique),
                              contributed_unique = sum (n_unique_ctb) / sum (n_unique),
                              gini_lin_deps = gini_lin (n_deps),
                              gini_log_deps = gini_log (n_deps),
                              gini_lin_tot = gini_lin (n_total_ctb),
                              gini_lin_unique = gini_lin (n_unique_ctb),
                              gini_log_tot = gini_log (n_total_ctb),
                              gini_log_unique = gini_log (n_unique_ctb))
    }

    gini <- deps |>
        dplyr::select (c (year, starts_with ("gini"))) |>
        dplyr::rename_with (~ gsub ("^gini\\_", "", .x)) |>
        tidyr::pivot_longer (cols = c (lin_deps, log_deps,
                                       lin_tot, log_tot,
                                       lin_unique, log_unique),
                             names_to = c ("total", "unique"),
                             names_sep = "\\_") |>
        dplyr::rename (gini = value,
                       scale = total,
                       category = unique)

    deps <- deps |>
        dplyr::select (!tidyr::starts_with ("gini")) |>
        tidyr::pivot_longer (cols = c (base_total, recommended_total, contributed_total,
                                       base_unique, recommended_unique, contributed_unique),
                             names_to = c ("total", "unique"),
                             names_sep = "\\_") |>
        dplyr::rename (proportion = value,
                       type = total,
                       category = unique)

    return (list (deps = deps, gini = gini))
}
