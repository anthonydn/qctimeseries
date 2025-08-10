#' qctimeseries: Window-by-window QC for large time series
#'
#' Tools to add/remove QC flag columns, review data in an interactive Shiny/plotly app,
#' summarize progress, and apply flags to produce cleaned series.
#'
#' @section Vignette:
#' A *vignette* is a long-form tutorial with runnable examples. 
#' **This is the user manual for qctimeseries.**
#'
#' \preformatted{
#'   vignette("overall_workflow", package = "qctimeseries")
#' }
#'
#' If you installed without vignettes, reinstall with:
#'
#' \preformatted{
#'   remotes::install_github("anthonydn/qctimeseries", build_vignettes = TRUE)
#' }
#'
#' @name qctimeseries
#' @docType package
#' @seealso vignette("overall_workflow", package = "qctimeseries")
#' @importFrom stats as.formula time
"_PACKAGE"

utils::globalVariables(c("panel","time","y","flag","pct_checked",".rowid","win_id"))
