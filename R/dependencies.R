
#' Get list of current recommended R packages
#'
#' @return Vector of names of recommended packages
#' @export
recommended_pkgs <- function () {

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
#' @param x Result of \link{load_pkgstats_data} with additional `year` column.
#' @return A `data.frame` of dependency data with `year` column.
#' @export
dependencies <- function (x) {

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

                        n <- nrow (out)

                        return (c (
                            package = this_pkg,
                            month = x$month [i],
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
                             n_total_base = as.integer (deps [, 3]),
                             n_total_rmcd = as.integer (deps [, 4]),
                             n_total_ctb = as.integer (deps [, 5]),
                             n_unique_base = as.integer (deps [, 6]),
                             n_unique_rmcd = as.integer (deps [, 7]),
                             n_unique_ctb = as.integer (deps [, 8]))

    deps <- deps_full |>
        dplyr::mutate (year = lubridate::year (date),
                       n_total = n_total_base + n_total_rmcd + n_total_ctb,
                       n_unique = n_unique_base + n_unique_rmcd + n_unique_ctb) |>
        dplyr::group_by (year) |>
        dplyr::summarise (base_total = sum (n_total_base) / sum (n_total),
                   recommended_total = sum (n_total_rmcd) / sum (n_total),
                   contributed_total = sum (n_total_ctb) / sum (n_total),
                   base_unique = sum (n_unique_base) / sum (n_unique),
                   recommended_unique = sum (n_unique_rmcd) / sum (n_unique),
                   contributed_unique = sum (n_unique_ctb) / sum (n_unique))

    deps <- tidyr::pivot_longer (deps,
                                 cols = c (base_total, recommended_total, contributed_total,
                                           base_unique, recommended_unique, contributed_unique),
                                 names_to = c ("total", "unique"),
                                 names_sep = "\\_") |>
        dplyr::rename (proportion = value,
                       type = total,
                       category = unique)

    return (deps)
}
