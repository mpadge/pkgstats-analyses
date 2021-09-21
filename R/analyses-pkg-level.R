
#' Analyse individual package trajectories
#'
#' @param x Result of \link{load_pkgstats_data} and
#' \link{transform_pkgstats_data}.
#' @return A `data.frame` of annual percentage changes in each variable followed
#' by each package.
#' @export
pkgstats_analyse_packages <- function (x) {

    x_ivs <- data.frame (m_rescale_data (x)$x_ivs)
    iv_nms <- names (x_ivs)

    x_ivs$package <- x$package
    # Scale effect estimates to annual change:
    x_ivs$date <- (x$date - min (x$date)) / 365
    x_ivs$month <- x$month
    x_ivs$date_wt <- x$date_wt

    m_pkgstats_analyse_all_pkgs (x_ivs, iv_nms)
}

pkgstats_analyse_all_pkgs <- function (x, iv_nms) {

    old_plan <- future::plan (future::multisession (workers =
                        ceiling (parallelly::availableCores () - 1)))

    # Random effects:
    #effect_rand <- lme4::VarCorr (mod)
    #ranefs <- lme4::ranef (mod)$package

    res <- future.apply::future_lapply (iv_nms, function (i) {
                suppressWarnings ({
                    form <- paste0 (i, " ~ date + (date | package)")

                    mod <- lme4::lmer (as.formula (form),
                                       weights = date_wt,
                                       data = x)
                    return (lme4::fixef (mod) [2]) # "date"
                })
            })

    on.exit (future::plan (old_plan))

    res <- data.frame (var = iv_nms,
                       effect = unlist (res))

    res <- res [order (abs (res$effect), decreasing = TRUE), ]

    res$effect <- res$effect * 100 # %/year

    rownames (res) <- NULL

    return (res)
}
m_pkgstats_analyse_all_pkgs <- memoise::memoise (pkgstats_analyse_all_pkgs)

pkgstats_analyse_time_only <- function (x) {

    if (any (duplicated (x$package)))
        stop ("'x' must be result of 'load_pkgstats_data' ",
              "with 'latest = TRUE'")

    x_ivs <- data.frame (m_rescale_data (x)$x_ivs)
    iv_nms <- names (x_ivs)

    x_ivs$package <- x$package
    # Scale effect estimates to annual change:
    x_ivs$date <- (x$date - min (x$date)) / 365
    x_ivs$month <- x$month
    x_ivs$date_wt <- x$date_wt

    one_effect <- function (x, nm) {

        form <- as.formula (paste0 (nm, " ~ date"))
        mod <- lm (form,
                   weights = date_wt,
                   data = x)
        mod$coefficients [2]
    }

    vapply (iv_nms, function (i) one_effect (x, i),
            numeric (1))
}
