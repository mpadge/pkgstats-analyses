
#' Analyse individual package trajectories
#'
#' @return A `data.frame` of annual percentage changes in each variable followed
#' by each package.
#' @export
pkgstats_analyse_packages <- function () {

    x <- load_pkgstats_data (datafile, raw = TRUE, latest = FALSE) |>
        transform_pkgstats_data ()

    x_ivs <- data.frame (m_rescale_data (x)$x_ivs)
    iv_nms <- names (x_ivs)

    x_ivs$package <- x$package
    # Scale effect estimates to annual change:
    x_ivs$date <- (x$date - min (x$date)) / 365.25
    x_ivs$month <- x$month
    x_ivs$date_wt <- x$date_wt

    res <- m_pkgstats_analyse_all_pkgs (x_ivs, iv_nms)

    # Then temporal trajectories from latest versions only:
    x <- load_pkgstats_data (datafile, raw = TRUE, latest = TRUE) |>
        transform_pkgstats_data ()
    z <- pkgstats_analyse_time_only (x)
    index <- match (res$var, z$var)
    res$effect_date <- z$effect_date [index]

    res <- res [order (abs (res$effect_date), decreasing = TRUE), ]
    rownames (res) <- NULL

    split (group_by_var_type (res), f = factor (res$group))
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
                       effect_pkg = unlist (res))

    res <- res [order (abs (res$effect_pkg), decreasing = TRUE), ]

    res$effect_pkg <- res$effect_pkg * 100 # %/year

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
    x_ivs$date <- (x$date - min (x$date)) / 365.25
    x_ivs$month <- x$month
    x_ivs$date_wt <- x$date_wt

    one_effect <- function (x, nm) {

        form <- as.formula (paste0 (nm, " ~ date"))
        mod <- lm (form,
                   weights = date_wt,
                   data = x)
        mod$coefficients [2]
    }

    res <- vapply (iv_nms, function (i)
                   one_effect (x, i),
                   numeric (1))

    res <- data.frame (var = iv_nms,
                       effect_date = unlist (res))

    res <- res [order (abs (res$effect_date), decreasing = TRUE), ]

    res$effect_date <- res$effect_date * 100 # %/year

    rownames (res) <- NULL

    return (res)
}

group_by_var_type <- function (x) {

    x$group <- NA
    d <- "desc\\_|nimports|nsuggests|nlanguages|nlinking"
    x$group [grep (d, x$var)] <- "description"
    x$group [grep ("files\\_", x$var)] <- "files"
    x$group [grep ("^n\\_fns", x$var)] <- "functions"
    x$group [grep ("lines\\_|^loc\\_[^per\\_]", x$var)] <- "lines"
    x$group [grep ("^npars", x$var)] <- "params"
    x$group [grep ("^loc\\_per\\_|per\\_fn\\_", x$var)] <- "lines_per_fn"
    x$group [grep ("rel\\_space", x$var)] <- "space"
    x$group [grep ("^doc", x$var)] <- "doclines_per_fn"
    x$group [grep ("^nexpr", x$var)] <- "nexpr"
    x$group [grep ("data\\_", x$var)] <- "data"
    x$group [grep ("demos|num\\_vignettes", x$var)] <- "demos_vignettes"
    x$group [grep ("^indentation$", x$var)] <- "indentation"
    x$group [grep ("centrality\\_|clusters|node|edges", x$var)] <- "network"

    return (x)
}
