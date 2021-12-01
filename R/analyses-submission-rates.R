
#' Analyse submission rates of packages
#'
#' @param datafile Path to local `pkgstats` data
#' @export
pkgstats_analyse_submission_rates <- function (datafile) {

    x <- load_pkgstats_data (datafile, raw = TRUE, latest = FALSE) |>
        transform_pkgstats_data ()

    m_analyse_submission_rates (x)
}

analyse_submission_rates <- function (x) {

    x |>
        dplyr::arrange (date) |>
        dplyr::group_by (package) |>
        dplyr::summarise (n = length (package),
                          time_med = stats::median (diff (date)),
                          time_mn = mean (diff (date)),
                          time_rate = ifelse (length (date) <= 2L,
                                              NA,
            unname (stats::lm (diff (date) ~ seq_along (date) [-1])$coefficients [2])))
}
m_analyse_submission_rates <- memoise::memoise (analyse_submission_rates)
