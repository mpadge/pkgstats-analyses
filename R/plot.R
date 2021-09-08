
#' Time series plot for R versus python
#'
#' @param x_r CRAN data load with \link{load_pkgstats_data} using `raw = FALSE`.
#' @param x_p pypi data load with \link{load_pkgstats_data} using `raw = FALSE`.
#' @param bimonthly If `TRUE`, aggregate data first into bimonthly intervals
#' (which generally produces a nicer looking plot).
#' @return A \pkg{ggplot2} object (invisibly)
#' @export
plot_r_py <- function (x_r, x_p, bimonthly = FALSE) {

    # suppress no visible binding notes:
    language <- count <- n <- NULL

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


    ggplot2::ggplot (dat, ggplot2::aes (x = date,
                                        y = n,
                                        colour = language,
                                        fill = language)) +
        ggplot2::geom_col (alpha = 0.5,
                           position = ggplot2::position_identity ()) +
        ggplot2::xlim (c (lubridate::ymd ("2018-01-01"), max (dat$date))) +
        ggplot2::ggtitle ("Rates of submission to pypi and CRAN",
                          subtitle  = "Including new submissions and updates") +
        ggplot2::theme (legend.position = c (0.1, 0.9),
                        plot.title = ggplot2::element_text (hjust = 0.5),
                        plot.subtitle = ggplot2::element_text (hjust = 0.5))
}
