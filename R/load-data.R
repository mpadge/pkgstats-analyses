
# These functions presume local data to exist, and will fail otherwise

#' Load one lot of either R or python data
#'
#' @param datafile Name of local file containing data to load
#' @return The data file, with all dates appropriately converted, and an
#' additional "month" column added.
#' @export
load_pkgstats_data <- function (datafile = "pkgstats-results.Rds") {

    if (!file.exists (datafile))
        stop ("datafile [", datafile, "] does not exist")

    m_load_pkgstats_data (datafile)
}

load_pkgstats_data_internal <- function (datafile) {

    x <- readRDS (datafile)

    is_r <- "package" %in% names (x)

    if (is_r) {

        index <- which (!(is.na (x$package) |
                          grepl ("^Error\\s", x$package))) # only 2 pkgs
        x <- x [index, ]
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
