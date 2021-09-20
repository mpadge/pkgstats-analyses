
#' Transform data to form new variables
#'
#' The raw data contain many related raw count variables. This function converts
#' many of these to relative proportions.
#'
#' @param x Result of \link{load_pkgstats_data} with `raw = TRUE`.
#' @return Transformed version of input, so some variables transformed and new
#' variables added.
#' @export
transform_pkgstats_data <- function (x) {

    transform_new_vals (x) |>
        transform_relative_vals ()
}

transform_new_vals <- function (x) {

    vals <- c ("depends", "imports", "suggests", "languages", "linking_to")
    for (v in vals) {

        x [paste0 ("n", v)] <- vapply (strsplit (x [[v]], ","),
                                       length,
                                       integer (1))
    }

    f <- grep ("^files\\_", names (x), value = TRUE)
    x [f] [is.na (x [f])] <- 0L # All 0 anyway
    x$files_total <- rowSums (x [f])

    return (x)
}

#' Transform values to relative measures
#'
#' Conversions at present are:
#' 1. blank & comment lines in each directory converted to relative amounts per
#' directory (R, src, inst)
#' 2. Add new measure of total numbers of lines of code.
#' @noRd
transform_relative_vals <- function (x) {

    # convert blank & comment lines to relative proportions:
    i1 <- grep ("^blank\\_", names (x), value = TRUE)
    i2 <- grep ("^loc\\_[^per]", names (x), value = TRUE)
    i3 <- grep ("^comment\\_", names (x), value = TRUE)
    for (i in seq_along (i1)) {
        x [which (is.na (x [i1 [i]])), i1 [i]] <- 0L
        x [which (is.na (x [i2 [i]])), i2 [i]] <- 0L
        x [which (is.na (x [i3 [i]])), i3 [i]] <- 0L

        nlines <- x [i1 [i]] + x [i2 [i]] + x [i3 [i]]
        x [paste0 (i1 [i], "_rel")] <- x [i1 [i]] / nlines
        x [paste0 (i3 [i], "_rel")] <- x [i3 [i]] / nlines
    }

    x$loc_total <- x$loc_R + x$loc_src + x$loc_inst

    types <- c ("blank_lines", "comment_lines", "loc")
    dirs <- c ("R", "src", "inst")
    combs <- apply (expand.grid (types, dirs), 1,
                    function (i) paste0 (i, collapse = "_"))
    x$lines_total <- rowSums (x [combs], na.rm = TRUE)

    x$blank_lines_all_rel <- (x$blank_lines_R +
                              x$blank_lines_src +
                              x$blank_lines_inst) / x$lines_total
    x$comment_lines_all_rel <- (x$comment_lines_R +
                                x$comment_lines_src +
                                x$comment_lines_inst) / x$lines_total

    x$loc_R [x$files_R == 0] <- NA_integer_
    x$loc_src [x$files_src == 0] <- NA_integer_
    x$loc_inst [x$files_inst == 0] <- NA_integer_
    x$loc_vignettes [x$files_vignettes == 0] <- NA_integer_
    x$loc_tests [x$files_tests == 0] <- NA_integer_

    return (x)
}
