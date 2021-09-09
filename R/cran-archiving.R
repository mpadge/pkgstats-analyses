
#' Plot rates of CRAN archiving
#'
#' @param datafile Name of local file containing CRAN data to load
#' @return A \pkg{ggplot2} object (invisibly)
#' @export
cran_archiving <- function (datafile = "pkgstats-results.Rds") {

    # suppress no visible binding notes:
    package <- month <- n <- count <- NULL

    cran_current <- m_cran_current_pkgs ()
    #cran_archive <- m_cran_archive_pkgs ()

    all_cran <- load_pkgstats_data (datafile)
    pkgs <- unique (all_cran$package)

    archived <- pkgs [which (!pkgs %in% cran_current$package)]
    pkgs <- all_cran [all_cran$package %in% archived, ]

    pkgs |>
        dplyr::group_by (package) |>
        dplyr::summarise (date = max (date)) -> pkgs
    pkgs$month <- lubridate::ceiling_date (pkgs$date, unit = "month")
    pkgs <- dplyr::group_by (pkgs, month) |>
        dplyr::summarise (n = length (month))

    ggplot2::ggplot (pkgs, ggplot2::aes (x = month, y = n)) +
        ggplot2::geom_line () +
        ggplot2::xlim (c (lubridate::ymd ("2010-01-01"), max (pkgs$month)))
}

cran_current_pkgs <- function () {

    u <- "https://cran.r-project.org/src/contrib/"

    cran <- rvest::read_html (u) |>
        rvest::html_text ()

    # Cut everything prior to first .tar.gz:
    i1 <- gregexpr ("\\.tar\\.gz", cran) [[1]] [1]
    sp <- gregexpr ("\\s", cran) [[1]]
    sp1 <- max (sp [which (sp < i1)])
    cran <- substring (cran, sp1, nchar (cran))

    cran <- strsplit (cran, "\\s") [[1]]
    cran <- grep ("\\.tar\\.gz", cran, value = TRUE)
    # They also have ascii characters that can't be grepped, so
    cran <- stringi::stri_escape_unicode (cran)
    cran <- unlist (strsplit (cran, "\\\\u00a0"))
    cran <- grep ("\\.tar\\.gz", cran, value = TRUE)
    cran <- gsub ("\\.tar\\.gz.*$", "", cran)

    package <- gsub ("\\_.*$", "", cran)
    version <- gsub ("^.*\\_", "", cran)

    tibble::tibble (package = package,
                    version = version)
}

m_cran_current_pkgs <- memoise::memoise (cran_current_pkgs)

cran_archive_pkgs <- function () {

    u <- "https://cran.r-project.org/src/contrib/Archive/"

    cran <- rvest::read_html (u) |>
        rvest::html_text ()

    # Cut everything prior to first " - "
    i1 <- gregexpr ("\\s\\-\\s", cran) [[1]] [1]
    cran <- substring (cran, i1 - 1, nchar (cran))
    cran <- strsplit (cran, "\\s\\-") [[1]]
    cran <- cran [which (!grepl ("^\\s+$", cran))]
    cran <- cran [which (!grepl ("Apache Server", cran))]

    cran <- gsub ("^\\s+|\\s+$", "", cran)
    # They also have ascii characters that can't be grepped, so
    cran <- stringi::stri_escape_unicode (cran)
    cran <- gsub ("\\\\u00a0", "", cran)
    
    # format is then <package>/Y-M-D hh:mm.
    cran <- gsub ("\\s.*$", "", cran) # rm hh:mm
    package <- gsub ("\\/.*$", "", cran)
    date <- gsub ("^.*\\/", "", cran)

    out <- tibble::tibble (package = package,
                           date = lubridate::ymd (date))

    return (out [which (!is.na (out$date)), ])
}

m_cran_archive_pkgs <- memoise::memoise (cran_archive_pkgs)
