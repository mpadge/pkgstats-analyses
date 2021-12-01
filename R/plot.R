
#' Time series plot for R versus python
#'
#' @param x_r CRAN data load with \link{load_pkgstats_data} using `raw = FALSE`.
#' @param x_p pypi data load with \link{load_pkgstats_data} using `raw = FALSE`.
#' @param bimonthly If `TRUE`, aggregate data first into bimonthly intervals
#' (which generally produces a nicer looking plot).
#' @param start_date First date to display, or set to `NULL` to display full
#' range.
#' @param type Either "bars" for a bar (column) graph, or "lines" for a line
#' graph.
#' @param lwd For type = "lines" only.
#' @return A \pkg{ggplot2} object (invisibly)
#' @export
plot_r_py <- function (x_r, x_p, bimonthly = FALSE,
                       start_date = "2018-01-01",
                       type = "bars",
                       lwd = 1) {

    type <- match.arg (tolower (type), c ("bars", "lines"))

    # suppress no visible binding notes:
    language <- count <- n <- NULL

    x_r <- x_r [-which.max (x_r$date), ]

    dat <- rbind (x_r, x_p)

    if (bimonthly) {

        index <- which (lubridate::month (dat$date) %% 2 == 0L)
        dat$date [index] <- dat$date [index] - months (1)

        dat |>
            dplyr::group_by (language, date) |>
            dplyr::summarise (count = sum (count), .groups = "keep") -> dat

        dat |>
            dplyr::group_by (language) |>
            dplyr::summarise (date = date,
                              count = count,
                              n = count / sum (count),
                              .groups = "keep") -> dat
    }

    if (is.null (start_date))
        start_date <- min (dat$date)
    else
        start_date <- lubridate::ymd (start_date)

    g <- ggplot2::ggplot (dat, ggplot2::aes (x = date,
                                             y = n,
                                             colour = language,
                                             fill = language))
    if (type == "bars")
        g <- g + ggplot2::geom_col (alpha = 0.5,
                                    position = ggplot2::position_identity ())
    else
        g <- g + ggplot2::geom_line (lwd = lwd)

    g + ggplot2::xlim (c (start_date, max (dat$date))) +
        ggplot2::ggtitle ("Rates of submission to pypi and CRAN",
                          subtitle  = "Including new submissions and updates") +
        ggplot2::ylab ("Proprtion of total submissions") +
        ggplot2::theme (legend.position = c (0.1, 0.9),
                        plot.title = ggplot2::element_text (hjust = 0.5),
                        plot.subtitle = ggplot2::element_text (hjust = 0.5))
}

#' Plot time series of rates of new submissions versus updated packages
#'
#' @param datafile Name of local file containing data to load
#' @inheritParams plot_r_py
#' @export
plot_new_vs_update <- function (datafile = "pkgstats-results.Rds",
                                bimonthly = FALSE,
                                start_date = "2018-01-01",
                                type = "lines",
                                lwd = 1) {

    # suppress no visible binding notes:
    package <- n <- count <- NULL

    type <- match.arg (tolower (type), c ("bars", "lines"))

    x <- pkgstatsAnalyses::load_pkgstats_data (datafile,
                                               raw = TRUE,
                                               latest = FALSE)
    is_r <- min (x$date) < "2005-01-01"

    dat <- m_new_vs_update_data (x)
    # rm latest dat:
    dat <- dat [-which (dat$date == max (dat$date)), ]

    if (bimonthly) {

        index <- which (lubridate::month (dat$date) %% 2 == 0L)
        dat$date [index] <- dat$date [index] - months (1)

        dat |>
            dplyr::group_by (type, date) |>
            dplyr::summarise (count = sum (count), .groups = "keep") -> dat

        dat |>
            dplyr::group_by (type) |>
            dplyr::summarise (date = date,
                              count = count,
                              n = count / sum (count),
                              .groups = "keep") -> dat
    }
    if (is.null (start_date))
        start_date <- min (dat$date)
    else
        start_date <- lubridate::ymd (start_date)

    g <- ggplot2::ggplot (dat,
                          ggplot2::aes (x = date,
                                        y = n,
                                        colour = type,
                                        fill = type))

    if (type == "bars")
        g <- g + ggplot2::geom_col (alpha = 0.5,
                                    position = ggplot2::position_dodge ())
    else
        g <- g + ggplot2::geom_line (lwd = lwd)

    where <- ifelse (is_r, "CRAN", "pypi")
    titl <- paste0 ("Rates of new submissions and updates to ", where)

    g +
        ggplot2::xlim (c (start_date, max (dat$date))) +
        ggplot2::ggtitle (titl) +
        ggplot2::theme (legend.position = c (0.9, 0.9),
                        plot.title = ggplot2::element_text (hjust = 0.5),
                        plot.subtitle = ggplot2::element_text (hjust = 0.5))
}

new_vs_update_data <- function (x) {

    # suppress no visible binding notes:
    package <- count <- NULL

    if (!"package" %in% names (x)) # python data
        names (x) [names (x) == "name"] <- "package"

    # create an "index" column flagging initial submissions:
    p <- x [, c ("package", "version", "date", "month")]
    p |>
        dplyr::group_by (package) |>
        dplyr::mutate (initial = (date == min (date))) -> p

    tab_new <- table (p$month [which (p$initial)])
    tab_update <- table (p$month [which (!p$initial)])

    dat_new <- data.frame (type = "new",
                           count = as.integer (tab_new),
                           #n = as.numeric (tab_new / sum (tab_new)),
                           n = as.numeric (tab_new / nrow (p)),
                           date = lubridate::ymd (names (tab_new)))
    dat_update <- data.frame (type = "update",
                              count = as.integer (tab_update),
                              #n = as.numeric (tab_update / sum (tab_update)),
                              n = as.numeric (tab_update / nrow (p)),
                              date = lubridate::ymd (names (tab_update)))

    rbind (dat_new, dat_update)
}

m_new_vs_update_data <- memoise::memoise (new_vs_update_data)
