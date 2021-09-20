
#' Analyse individual package trajectories
#'
#' @param x Result of \link{load_pkgstats_data} and
#' \link{transform_pkgstats_data}.
#' @return A `data.frame` of annual percentage changes in each variable followed
#' by each package.
#' @export
pkgstats_analyse_packages <- function (x) {

    x_ivs <- data.frame (m_rescale_data (x)$x_ivs)
    nms <- names (x_ivs)
    x_ivs$package <- x$package
    # Scale effect estimates to annual change:
    x_ivs$date <- (x$date - min (x$date)) / 365
    x_ivs$month <- x$month
    x_ivs$date_wt <- x$date_wt

    m_pkgstats_analyse_all_pkgs (x_ivs)
}

fixef1 <- function (x, nm) {

    form <- paste0 (nm, " ~ date + (date | package)")

    mod <- lme4::lmer (as.formula (form),
                       weights = date_wt,
                       data = x)

    #effect_rand <- lme4::VarCorr (mod)
    #ranefs <- lme4::ranef (mod)$package
    effect_fixed <- lme4::fixef (mod) [2] # [2] for "date"

    return (effect_fixed)
}

pkgstats_analyse_all_pkgs <- function (x) {

    nms <- names (x)

    old_plan <- future::plan (future::multisession (workers =
                        ceiling (parallelly::availableCores () - 1)))

    res <- future.apply::future_lapply (nms, function (i)
                                        suppressWarnings (fixef1 (x, i)))

    on.exit (future::plan (old_plan))

    res <- data.frame (var = nms, effect = unlist (res))
    res <- res [order (abs (res$effect), decreasing = TRUE), ]
    res$effect <- res$effect * 100 # %/year

    return (res)
}
m_pkgstats_analyse_all_pkgs <- memoise::memoise (pkgstats_analyse_all_pkgs)
