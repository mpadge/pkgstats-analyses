
# These functions presume local data to exist, and will fail otherwise

#' Load one lot of either R or python data
#'
#' @param datafile Name of local file containing data to load
#' @param raw If `FALSE`, return tabulated counts of packages per month,
#' otherwise return raw data.
#' @param latest If `TRUE`, return data only on latest CRAN version of each
#' package, otherwise return data on all releases of all packages.
#' @return The data file, with all dates appropriately converted, and an
#' additional "month" column added.
#' @export
load_pkgstats_data <- function (datafile = "pkgstats-results.Rds",
                                raw = TRUE,
                                latest = TRUE) {

    if (!file.exists (datafile))
        stop ("datafile [", datafile, "] does not exist")

    x <- m_load_pkgstats_data (datafile)

    if (latest) {

        x <- m_latest_data (x)

    } else if (!raw) {

        x <- m_convert_data (x)

    }

    # Add year column:
    x$year <- lubridate::year (x$date)

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

        x <- data.frame (x)
    }

    x <- tibble::tibble (x)

    x$date <- lubridate::ymd (strptime (x$date, format = "%Y-%m-%d"))
    x$month <- lubridate::ceiling_date (x$date, unit = "month")

    if (!is_r) {
        # remove lastest month of python data
        x <- x [x$month < max (x$month), ]
        # and change 'name' to 'package'
        names (x) [names (x) == "name"] <- "package"
    }

    # Add a date_wt column to weight monthly contributions
    dw <- table (x$month)
    w <- as.numeric (unname (dw [match (as.character (x$month), names (dw))]))
    x$date_wt <- w / max (w)

    x$desc_n_aut <- as.integer (x$desc_n_aut)
    x$desc_n_ctb <- as.integer (x$desc_n_ctb)
    x$files_R <- as.integer (x$files_R)
    x$files_src <- as.integer (x$files_src)
    x$files_inst <- as.integer (x$files_inst)
    x$files_tests <- as.integer (x$files_tests)
    x$files_vignettes <- as.integer (x$files_vignettes)
    x$num_data_files <- as.integer (x$num_data_files)

    x$loc_R <- as.integer (x$loc_R)
    x$loc_src <- as.integer (x$loc_src)
    x$loc_inst <- as.integer (x$loc_inst)
    x$loc_vignettes <- as.integer (x$loc_vignettes)
    x$loc_tests <- as.integer (x$loc_tests)
    x$n_fns_r_exported <- as.integer (x$n_fns_r_exported)
    x$n_fns_r_not_exported <- as.integer (x$n_fns_r_not_exported)
    x$n_fns_src <- as.integer (x$n_fns_src)
    x$n_fns_per_file_r <- as.integer (x$n_fns_per_file_r)
    x$n_fns_per_file_src <- as.integer (x$n_fns_per_file_src)
    x$npars_exported_mn <- as.integer (x$npars_exported_mn)
    x$npars_exported_md <- as.integer (x$npars_exported_md)
    x$loc_per_fn_r_mn <- as.integer (x$loc_per_fn_r_mn)
    x$loc_per_fn_r_md <- as.integer (x$loc_per_fn_r_md)
    x$loc_per_fn_r_exp_mn <- as.integer (x$loc_per_fn_r_exp_mn)
    x$loc_per_fn_r_exp_md <- as.integer (x$loc_per_fn_r_exp_md)
    x$loc_per_fn_r_not_exp_mn <- as.integer (x$loc_per_fn_r_not_exp_mn)
    x$loc_per_fn_r_not_exp_md <- as.integer (x$loc_per_fn_r_not_exp_md)
    x$loc_per_fn_src_mn <- as.integer (x$loc_per_fn_src_mn)
    x$loc_per_fn_src_md <- as.integer (x$loc_per_fn_src_md)
    x$doclines_per_fn_exp_mn <- as.integer (x$doclines_per_fn_exp_mn)
    x$doclines_per_fn_exp_md <- as.integer (x$doclines_per_fn_exp_md)
    x$doclines_per_fn_not_exp_mn <- as.integer (x$doclines_per_fn_not_exp_mn)
    x$doclines_per_fn_not_exp_md <- as.integer (x$doclines_per_fn_not_exp_md)
    x$docchars_per_par_exp_mn <- as.integer (x$docchars_per_par_exp_mn)
    x$docchars_per_par_exp_md <- as.integer (x$docchars_per_par_exp_md)

    x$n_fns_r <- as.integer (x$n_fns_r)
    x$n_fns_src <- as.integer (x$n_fns_src)
    x$n_edges <- as.integer (x$n_edges)
    x$n_edges_r <- as.integer (x$n_edges_r)
    x$n_edges_src <- as.integer (x$n_edges_src)
    x$n_clusters <- as.integer (x$n_clusters)
    x$centrality_dir_mn <- as.integer (x$centrality_dir_mn)
    x$centrality_dir_md <- as.integer (x$centrality_dir_md)
    x$centrality_dir_mn_no0 <- as.integer (x$centrality_dir_mn_no0)
    x$centrality_dir_md_no0 <- as.integer (x$centrality_dir_md_no0)
    x$centrality_undir_mn <- as.integer (x$centrality_undir_mn)
    x$centrality_undir_md <- as.integer (x$centrality_undir_md)
    x$centrality_undir_mn_no0 <- as.integer (x$centrality_undir_mn_no0)
    x$centrality_undir_md_no0 <- as.integer (x$centrality_undir_md_no0)
    x$central_edges_dir <- as.integer (x$num_terminal_edges_dir)
    x$num_terminal_edges_dir <- as.integer (x$num_terminal_edges_dir)
    x$num_terminal_edges_undir <- as.integer (x$num_terminal_edges_undir)
    x$node_degree_mn <- as.integer (x$node_degree_mn)
    x$node_degree_md <- as.integer (x$node_degree_md)
    x$node_degree_max <- as.integer (x$node_degree_max)

    x <- x [which (nzchar (x$package)), ]

    return (x)
}

m_load_pkgstats_data <- memoise::memoise (load_pkgstats_data_internal)

convert_data <- function (x) {

    is_r <- min (x$date) < "2005-01-01"

    tab <- table (x$month)
    tibble::tibble (language = ifelse (is_r, "R", "python"),
                    count = as.integer (tab),
                    n = as.numeric (tab / sum (tab)),
                    date = lubridate::ymd (names (tab)))
}

m_convert_data <- memoise::memoise (convert_data)

latest_data <- function (x) {

    package <- NULL # suppress no visible binding note

    x <- x |>
        dplyr::group_by (package) |>
        dplyr::slice_max (date)
    # multiple versions on same date:
    x <- x |>
        dplyr::group_by (package) |>
        dplyr::slice_max (version)
    # And a few repeated versions
    dups <- which (duplicated (x [, c ("package", "version", "date")]))
    if (length (dups) > 0L) {
        x <- x [-dups, ]
    }

    return (x)
}
m_latest_data <- memoise::memoise (latest_data)
