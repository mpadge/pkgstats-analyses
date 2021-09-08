
#' Return list of all pypi packages
#' @return Character vector
#' @noRd
list_all_pypi_pkgs <- function () {

    u <- "https://pypi.org/simple/"
    all_pkgs <- rvest::read_html (u) |>
        rvest::html_text () |>
        strsplit ("\\n")
    all_pkgs <- gsub ("^\\s+", "", all_pkgs [[1]])

    return (all_pkgs)
}

#' Extract details for one pypi package
#' @param pkg Name of package
#' @return A data frame with columns of package name, all package versions, and
#' associated release dates.
#' @examples
#' pkg <- "street-address"
#' x <- one_pypi (pkg)
#' @noRd
one_pypi <- function (pkg) {

    u <- paste0 ("https://pypi.python.org/pypi/", pkg, "/json")
    x <- tryCatch (jsonlite::fromJSON (u),
                   error = function (e) NULL)
    len <- vapply (x$releases, length, integer (1))
    x$releases <- x$releases [which (len > 0L)]
    # releases can have multiple uploads!
    dates <- vapply (x$releases, function (i) i$upload_time [1],
                     character (1))

    ret <- NULL
    if (length (dates) > 0L) {

        #ret <- data.frame (name = pkg,
        #                   version = names (dates),
        #                   date = strptime (dates, "%Y-%m-%dT%H:%M:%S"))
        ret <- cbind (name = pkg,
                      version = names (dates),
                      date = dates)
    }
    return (ret)
}

#' Extract all releases and dates for all pypi packages ever
#'
#' @param chunk_size Size of chunks into which parallel job is to be broken.
#' Intermediate results are saved for each chunk.
#' @param results_file Name of file (potentially including path) where results
#' are to be saved.
#' @param data_dir Directory in which temporary results for each chunk are to be
#' saved prior to final aggregation.
#' @return Nothing (data are depositoed in 'data_dir`).
#' @export
all_pypi <- function (chunk_size = 1001,
                      results_file = "pypi.Rds",
                      data_dir = "./data-temp") {

    all_pkgs <- list_all_pypi_pkgs ()

    data_dir <- normalizePath (data_dir)
    if (!dir.exists (data_dir))
        dir.create (data_dir, recursive = TRUE)

    ntot <- length (all_pkgs)

    if (file.exists (results_file)) {

        x <- readRDS (results_file)
        all_pkgs <- all_pkgs [which (!all_pkgs %in% x [, "name"])]
    }

    index <- 1L

    nthis <- length (all_pkgs)
    nprev <- ntot - nthis

    n <- ceiling (nthis / chunk_size)
    n <- factor (rep (seq (n), each = chunk_size)) [seq (nthis)]
    all_pkgs <- split (all_pkgs, f = n)

    old_plan <- future::plan (
                    future::multisession (
                        workers = ceiling (parallelly::availableCores () / 2)))

    pt0 <- proc.time ()

    for (p in all_pkgs) {

        x <- future.apply::future_lapply (p, one_pypi)
        x <- do.call (rbind, x)
        ftmp <- file.path (data_dir,
                           paste0 ("pypi-temp-", index, ".Rds"))
        saveRDS (x, ftmp)

        prog <- (nprev + index * chunk_size) / ntot
        prog_fmt <- format (100 * prog, digits = 2)
        pt1 <- as.integer ((proc.time () - pt0) [3])
        t_per_file <- pt1 / (index * chunk_size)
        t_total <- t_per_file * nthis
        t_rem <- hms::hms (t_total - pt1)

        message ("[", nprev + index * chunk_size, " / ", ntot,
                 "]  = ", prog_fmt, "%; (elapsed, remaining) = (",
                 pt1, ", ", t_rem, ")")

        index <- index + 1
    }

    future::plan (old_plan)
}
