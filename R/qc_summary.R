#' QC progress summary
#'
#' Summarize QC progress for each variable that has a corresponding flag column.
#' Assumes flags use: 1 = approved, 0 = unchecked, -2 = flagged, -1 = original NA.
#'
#' @param data A `data.frame` returned by [qc_add_flags()] (must carry
#'   attributes `qc_vars` and `qc_suffix`).
#' @param quiet If `TRUE`, return invisibly without printing.
#' @param hide_complete If `TRUE`, only return rows with `%checked < 100`.
#'
#' @return A tibble with per-variable totals and percentages:
#' \itemize{
#'   \item \code{variable}: base variable name
#'   \item \code{total}: count of nonmissing data points
#'   \item \code{\%checked}: percent of nonmissing points whose flag is not
#'         \code{0} (unchecked) and not \code{-1} (original NA marker)
#'   \item \code{\%approved(1)}: percent of all rows with flag \code{1}
#'   \item \code{\%unchecked(0)}: percent of all rows with flag \code{0}
#'   \item \code{\%missing(-1)}: percent of all rows with flag \code{-1}
#'   \item \code{\%flagged(-2)}: percent of all rows with flag \code{-2}
#' }
#'
#' @details
#' Columns whose names include a flag value (e.g., \code{\%flagged(-2)}) are
#' computed over \emph{all rows} (including missing). The denominator for
#' \code{\%checked} is the number of nonmissing values.
#'
#' This function warns when:
#' \enumerate{
#'   \item \code{sum(is.na(value))} does not match \code{sum(flag == -1)}.
#'   \item Any flag outside \code{c(-2, -1, 0, 1)} is detected.
#' }
#'
#' @template see-vignette
#' @export
qc_progress <- function(data, quiet = FALSE, hide_complete = FALSE) {
    vars <- attr(data, "qc_vars")
    suffix <- attr(data, "qc_suffix")
    if (is.null(vars) || is.null(suffix)) {
        stop("qc_progress(): data does not look like a qc_add_flags() result")
    }

    res <- lapply(vars, function(v) {
        fcol <- paste0(v, suffix)
        flag <- data[[fcol]]
        val <- data[[v]]

        # sanity checks and warnings
        n_na_val <- sum(is.na(val))
        n_flag_na <- sum(flag == -1L, na.rm = TRUE)
        if (!isTRUE(n_na_val == n_flag_na)) {
            warning(sprintf(
                "qc_progress(): %s - NA count (%d) != flags == -1 (%d).",
                v, n_na_val, n_flag_na
            ))
        }

        bad_flags <- setdiff(unique(flag[!is.na(flag)]), c(-2L, -1L, 0L, 1L))
        if (length(bad_flags)) {
            warning(sprintf(
                "qc_progress(): %s - unexpected flag values found: %s",
                v, paste(sort(bad_flags), collapse = ", ")
            ))
        }

        n_all <- length(val)
        n_valid <- n_all - n_na_val

        # percentages (guard against zero denominators)
        pct_nonmissing_checked <- if (n_valid > 0) {
            100 * sum(flag != 0L & flag != -1L &
                !is.na(val), na.rm = TRUE) / n_valid
        } else {
            NA_real_
        }

        pct_approved <- 100 * sum(flag == 1L, na.rm = TRUE) / n_all
        pct_unchecked <- 100 * sum(flag == 0L, na.rm = TRUE) / n_all
        pct_missing <- 100 * sum(flag == -1L, na.rm = TRUE) / n_all
        pct_flagged <- 100 * sum(flag == -2L, na.rm = TRUE) / n_all

        tibble::tibble(
            variable = v,
            total = n_valid,
            `%checked` = round(pct_nonmissing_checked, 2),
            `%approved(1)` = round(pct_approved, 2),
            `%unchecked(0)` = round(pct_unchecked, 2),
            `%missing(-1)` = round(pct_missing, 2),
            `%flagged(-2)` = round(pct_flagged, 2)
        )
    }) |> dplyr::bind_rows()

    if (hide_complete) {
        res <- dplyr::filter(res, .data$`%checked` < 100)
    }

    if (!quiet) print(res, n = Inf)
    invisible(res)
}
