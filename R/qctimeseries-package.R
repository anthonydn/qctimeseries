#' qctimeseries: Window-by-window QC for large time series
#'
#' Tools to add/remove QC flag columns, review data in an interactive Shiny/plotly app,
#' summarize progress, and apply flags to produce cleaned series.
#'
#' @section Vignette (strongly recommended):
#' A *vignette* is a long-form tutorial with runnable examples. Read the full workflow at:
#'
#' \preformatted{
#'   vignette("getting-started", package = "qctimeseries")
#'   # or browse all vignettes:
#'   browseVignettes("qctimeseries")
#' }
#'
#' If you installed without vignettes, reinstall with:
#'
#' \preformatted{
#'   remotes::install_github("anthonydn/qctimeseries", build_vignettes = TRUE)
#' }
#'
#' (From a local clone you can also run: \code{devtools::build_vignettes()}.)
#'
#'
#' @section Key functions:
#' - [qc_add_flags()] — initialize `*_qcflag` columns
#' - [qc_window_app()] — interactive QC by fixed-width windows
#' - [qc_progress()] — per-variable QC completion summary
#' - [qc_check_plot()] — quick visual (raw vs. cleaned)
#' - [qc_apply_flags()] — mask flagged values, optionally drop flag cols
#' - [qc_remove_flags()] — drop flag columns
#' - [qc_transfer()] — copy flags from one var to another
#'
#' @seealso vignette("getting-started", package = "qctimeseries")
#' @name qctimeseries
#' @keywords package
"_PACKAGE"
