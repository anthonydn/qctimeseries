#' qctimeseries: Window-by-window QC for large time series
#'
#' Tools to add QC flag columns, review data in an interactive Shiny/plotly app,
#' summarize progress, and apply flags to produce cleaned series.
#'
#' ## Typical workflow
#' 1. Create flag columns: [qc_add_flags()].
#' 2. Review/flag in the app: [qc_window_app()] (returns modified data).
#' 3. Check progress: [qc_progress()].
#' 4. (Optional) Visual check: [qc_check_plot()].
#' 5. Apply flags to mask bad values: [qc_apply_flags()].
#' 6. (Optional) Remove flag cols: [qc_remove_flags()].
#'
#' ## Flag encoding
#' `1 = approved`, `0 = unchecked`, `-2 = manual flag`, `-1 = original NA`.
#'
#' @section Key functions:
#' - [qc_add_flags()] — initialize *_qcflag columns.
#' - [qc_window_app()] — interactive QC by fixed-width windows.
#' - [qc_progress()] — per-variable QC completion summary.
#' - [qc_check_plot()] — quick visual (raw vs. cleaned).
#' - [qc_apply_flags()] — mask flagged values, optionally drop flag cols.
#' - [qc_remove_flags()] — drop flag columns.
#' - [qc_transfer()] — copy flags from one var to another.
#'
#' @docType package
#' @name qctimeseries
#' @keywords package
NULL
