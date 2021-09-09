
# These functions presume local data to exist, and will fail otherwise

#' Load one lot of either R or python data
#'
#' @param datafile Name of local file containing data to load
#' @param raw If `FALSE`, return tabulated counts of packages per month,
#' otherwise return raw data.
#' @return The data file, with all dates appropriately converted, and an
#' additional "month" column added.
#' @export
load_pkgstats_data <- function (datafile = "pkgstats-results.Rds",
                                raw = TRUE) {

    if (!file.exists (datafile))
        stop ("datafile [", datafile, "] does not exist")

    x <- m_load_pkgstats_data (datafile)

    if (!raw) {

        x <- m_convert_data (x)

    }

    return (x)
}

load_pkgstats_data_internal <- function (datafile) {

    x <- readRDS (datafile)

    is_r <- "package" %in% names (x)

    if (is_r) {

        index <- which (!(is.na (x$package) |
                          grepl ("^Error\\s", x$package)))
        x <- x [index, ]

        # There are lots of experiments from 2003-2005 that also need to be
        # removed, all identifiable by names with spaces and dates
        x <- x [which (!grepl ("\\s", x$package)), ]
    } else {

        x <- data.frame (readRDS (datafile))
    }

    x <- tibble::tibble (x)

    x$date <- lubridate::ymd (strptime (x$date, format = "%Y-%m-%d"))
    x$month <- lubridate::ceiling_date (x$date, unit = "month")

    if (!is_r) # remove lastest month of python data
        x <- x [x$month < max (x$month), ]

    return (x)
}

m_load_pkgstats_data <- memoise::memoise (load_pkgstats_data_internal)

convert_data <- function (x) {

    is_r <- "package" %in% names (x)

    tab <- table (x$month)
    tibble::tibble (language = ifelse (is_r, "R", "python"),
                    count = as.integer (tab),
                    n = as.numeric (tab / sum (tab)),
                    date = lubridate::ymd (names (tab)))
}

m_convert_data <- memoise::memoise (convert_data)
