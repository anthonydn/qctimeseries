#' qctimeseries: Window-by-window QC for large time series
#'
#' Tools to add/remove QC flag columns, review data in an interactive Shiny/plotly app,
#' summarize progress, and apply flags to produce cleaned series.
#'
#' @section Vignette (strongly recommended):
#' A *vignette* is a long-form tutorial with runnable examples. Read the full workflow at:
#'
#' \preformatted{
#'   vignette("overall_workflow", package = "qctimeseries")
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
#' @section R Markdown template (click-to-run QC):
#' You can set up a simple QC environment so helpers with little to no R experience
#' can review data by opening an editable R Markdown file and pressing **▶ Run** on
#' each chunk. The only thing they change is the variable name as they move through
#' each variable that needs QC checking.
#'
#' **One-time prep by an analyst**
#' \preformatted{
#'   # 1) Import your data and add QC flags
#'   dat    <- read.csv("your_data.csv")      # or readxl::read_excel(...)
#'   dat_qc <- qc_add_flags(dat)              # adds *_qcflag columns
#'
#'   # 2) Save the working object for checkers
#'   save(dat_qc, file = "sat_site_qc.RData")
#' }
#'
#' **Create the checker document**
#' \preformatted{
#'   # RStudio: File -> New File -> R Markdown -> From Template ->
#'   #   "data_checker     {qctimeseries}" 
#'   # or to create in working directory:
#'   rmarkdown::draft("QC_Workflow.Rmd",
#'      template = "data-checker", package = "qctimeseries")
#' }
#'
#' The template loads example data set `sat_site_qc.RData` (for which you can sub in your data object), 
#' shows progress with [qc_progress()], opens the app with [qc_window_app()],
#'  and saves back with `save(...)` so work persists between sessions.
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
#' qctimeseries: window-by-window QC for large time series
#'
#' @name qctimeseries
#' @docType package
#' @seealso vignette("getting-started", package = "qctimeseries")
#' @importFrom stats as.formula time
"_PACKAGE"

utils::globalVariables(c("panel","time","y","flag","pct_checked",".rowid","win_id"))

