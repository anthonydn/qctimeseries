#' Interactive QC for Time-Series Windows
#'
#' Launch a Shiny app to visually inspect and flag bad data points in large
#' numeric time-series datasets.  Data are split into fixed-width windows
#' (e.g. 168 hours) so only one slice is rendered at a time, allowing smooth
#' interaction even with millions of rows.
#'
#' @param dat A `data.frame` or `data.table` containing at least a POSIXct
#'   time column and one numeric column to QC.
#' @param y_col `string`. Name of the numeric column to display and flag.
#' @param win_hrs Window width in hours (default 168 = one week).
#' @param qc_suffix Suffix for the flag column. A new integer column
#'   `paste0(y_col, qc_suffix)` must already exist; it will be updated in
#'   place (`1 = approved`, `0 = unchecked`, `-2 = flagged`, `-1 = original NA`).
#' @param time_col Name of the POSIXct column. Defaults to `"DateTime"`.
#' @param tz_user Time zone used for display (affects x-axis labels only).
#'
#' @return A `data.frame` identical to `dat` but with modified QC flag columns.
#' @export
#' @import data.table
#' @import shiny
#' @import plotly
#' @template see-vignette
#'
#'
#' @examples
#' if (interactive()) {
#'   dummy <- data.frame(
#'     DateTime = seq.POSIXt(Sys.time(), length.out = 5000, by = "hour"),
#'     temp     = rnorm(5000, 20, 2),
#'     temp_qcflag = integer(5000)
#'   )
#'   qc_window_app(dummy, y_col = "temp")
#' }
qc_window_app <- function(dat,
                          y_col,
                          win_hrs   = 168,
                          qc_suffix = "_qcflag",
                          time_col  = "DateTime",
                          tz_user   = "America/Denver") {

  # --- load ----------------------------------------------------------
  if (!inherits(dat[[time_col]], "POSIXct")) {
    cls <- paste(class(dat[[time_col]]), collapse = "/")
    fmt <- paste0("qc_window_app(): `%s` must be POSIXct (found: %s). ",
      "Convert before calling, e.g. dat[[\"%s\"]] <- as.POSIXct(",
      "dat[[\"%s\"]], tz = \"UTC\") or lubridate::ymd_hms(...).")
    stop(sprintf(fmt, time_col, cls, time_col, time_col))}

  fcol <- paste0(y_col, qc_suffix)
  if (!fcol %in% names(dat))
    stop("qc_window_app(): column '", fcol,
         "' not found. Create flags with qc_add_flags() first.")

  dt <- as.data.table(dat)
  if (!".rowid" %in% names(dt)) dt[, .rowid := .I]

  make_windows <- function(hrs) {
    start <- min(dt[[time_col]], na.rm = TRUE)
    dt[, win_id := as.integer(
      floor(as.numeric(difftime(get(time_col), start, "secs")) / (hrs*3600)) )]
    # initial split to list(row-ids)
    wins <- split(dt$.rowid, dt$win_id)
    # keep only windows that contain more than one non-NA y-value
    wins <- Filter(function(idx) any(!is.na(dt[[y_col]][idx])), wins)
    names(wins) <- seq_along(wins) - 1L
    wins}
  win_rows <- make_windows(win_hrs)


  # --- state ------------------------------------------------------------------
  current_win <- 0L
  x_range     <- NULL
  y_range     <- NULL

  # --- UI ---------------------------------------------------------------------
  ui <- fluidPage(
    tags$head(tags$style("
  html, body { overflow-y: scroll; }
  .btn-row{display:flex;justify-content:center;flex-wrap:wrap;gap:6px}
  .btn-row .btn,.btn-row .form-control{margin:3px 4px;height:34px;}
  .label-inline{font-weight:bold;display:flex;align-items:center;}
  #win_label { min-height: 22px; }")),
    textOutput("win_label"),
    plotlyOutput("tsplot", height = 440),
    plotlyOutput("secplot", height = 200),
    tags$hr(style="margin:4px 0;"),

    div(class="btn-row",
      actionButton("prev_win","Prev"),
      actionButton("next_win","Next"),
      numericInput("jump", NULL, 1, min = 1, width = "80px"),
      actionButton("flag_sel_next",
                   "Flag Selected & Approve Unflagged & Next ->",
                   class="btn-primary"),
      actionButton("home_zoom", "Home zoom"),
      span(class="label-inline","Secondary:"),
      selectInput("sec_var", NULL, choices = character(0), selected = NULL,
                  width="300px")),
    div(class="btn-row",
      actionButton("flag_sel",   "Flag Selected Points", class="btn-danger"),
      actionButton("unflag_sel", "Unflag Selected Points"),
      actionButton("approve_sel","Approve Selected Points", class="btn-success")),
    div(class="btn-row",
      actionButton("flag_win", "Flag ENTIRE Window", class="btn-danger"),
      actionButton("approve_unflagged", "Approve ALL Unflagged", class="btn-success"),
      actionButton("reset_win", "Reset Window -> Unchecked")),
    tags$hr(style="margin:4px 0;"),
    div(class="btn-row",
      checkboxInput("hide_bad","Hide flagged (red)", FALSE),
      span(class="label-inline","Window (hrs):"),
      numericInput("win_width",NULL,win_hrs,min=1,width="90px"),
      actionButton("reset_all","Reset ALL -> Unchecked"),
      actionButton("done", "Done / Return", class="btn-primary")))

  # --- server -----------------------------------------------------------------
  server <- function(input, output, session) {

    ## ---- dropdown choices (hide qcflag cols) ----
    sec_choices <- names(dt)
    sec_choices <- sec_choices[!grepl(paste0(qc_suffix, "$"), sec_choices)]
    sec_choices <- setdiff(sec_choices, c(time_col, y_col, fcol))
    updateSelectInput(session, "sec_var",
                      choices = c("", sec_choices), selected = "")

    rows_now <- function() win_rows[[as.character(current_win)]]

    brushed_ids <- function() {
      ev <- event_data("plotly_selected", source = "plot")
      if (is.null(ev) || !nrow(ev)) integer() else as.integer(ev$key)}

    window_xr0 <- function() {
      hrs <- input$win_width
      if (!is.finite(hrs) || hrs <= 0) hrs <- win_hrs
      rows <- rows_now()
      if (!length(rows)) return(NULL)
      wid <- unique(dt[rows, win_id])[1]
      start0 <- min(dt[[time_col]], na.rm = TRUE)
      t0 <- start0 + as.difftime(wid * hrs, units = "hours")
      t1 <- t0     + as.difftime(hrs,     units = "hours")
      span <- as.numeric(difftime(t1, t0, units = "secs"))
      pad  <- span * 0.02
      c(t0 - pad, t1 + pad)}

    build_plot <- function() {
      rows <- rows_now()
      wd   <- dt[rows]
      vals <- wd[[y_col]]

      base_rows <- if (isTRUE(input$hide_bad)) rows[dt[rows][[fcol]] >= 0L] else rows
      base_rows <- base_rows[!is.na(dt[[y_col]][base_rows]) & !is.na(dt[[time_col]][base_rows])]

      xr <- if (is.null(x_range)) window_xr0() else x_range

      if (is.null(y_range)) {
        yr0  <- range(dt[[y_col]][base_rows], na.rm = TRUE)
        ypad <- diff(yr0) * 0.02
        yr   <- yr0 + c(-ypad, ypad)
      } else yr <- y_range

      p <- plot_ly(dt[base_rows],
        x     = ~get(time_col),
        y     = ~get(y_col),
        type  = "scattergl",
        mode  = "lines+markers",
        marker = list(size = 4, color = "steelblue"),
        line   = list(width = 1, color = "gray"),
        key    = ~.rowid,
        source = "plot") %>%
      layout(dragmode = "zoom",
        xaxis = list(range = xr, title = list(text = "")),
        yaxis = list(range = yr, title = y_col),
        uirevision = current_win) %>%
      event_register("plotly_selected") %>%
      event_register("plotly_relayout") %>%
      config(modeBarButtonsToRemove = c("autoScale2d", "resetScale2d"))

      add_pts <- function(idx, col) {
        idx <- idx[!is.na(dt[[y_col]][idx]) & !is.na(dt[[time_col]][idx])]
        if (!length(idx)) return(p)
        add_trace(
          p, data = dt[idx],
          x = ~get(time_col), y = ~get(y_col),
          type = "scattergl", mode = "markers",
          marker = list(size = 6, color = col, opacity = 0.9),
          inherit = FALSE, showlegend = FALSE)}

      p <- add_pts(rows[dt[rows][[fcol]] == 1L], "green")
      if (!isTRUE(input$hide_bad))
        p <- add_pts(rows[dt[rows][[fcol]] < 0L], "red")
      p
    }

    build_sec_plot <- function(){
      var <- input$sec_var
      if (is.null(var) || var == "" || !(var %in% names(dt)))
        return(plotly_empty(type = "scattergl", mode = "lines"))

      rows <- rows_now()
      good <- rows[ dt[rows][[fcol]] >= 0L & !is.na(dt[[var]][rows]) ]
      if (!length(good)) return(plotly_empty(type = "scattergl", mode = "lines"))

      xr <- if (is.null(x_range)) window_xr0() else x_range

      plot_ly(dt[good],
              x = as.formula(paste0("~", time_col)),
              y = ~get(var),
              type="scattergl", mode="lines",
              line=list(width=1,color="orange")) %>%
        layout(dragmode="zoom",
               xaxis=list(range=xr),
               yaxis=list(title=var)) %>%
        config(modeBarButtonsToRemove = c("autoScale2d", "resetScale2d"))
      }

    redraw <- function(keep_x = TRUE, keep_y = TRUE) {
      if (!keep_x) x_range <<- NULL
      if (!keep_y) y_range <<- NULL
      updateNumericInput(session, "jump", value = current_win + 1)
      output$secplot <- renderPlotly(build_sec_plot())
      output$tsplot  <- renderPlotly(build_plot())
      rng <- range(dt[rows_now(), get(time_col)], na.rm = TRUE)
      output$win_label <- renderText(sprintf(
        "Window %d / %d   %s - %s",
        current_win + 1, length(win_rows),
        format(rng[1], "%Y-%m-%d %H:%M"),
        format(rng[2], "%Y-%m-%d %H:%M")))}

    redraw()

    # remember zoom
    observeEvent(event_data("plotly_relayout", source = "plot"), ignoreInit = TRUE, {
      ev <- event_data("plotly_relayout", source = "plot")

      x0 <- ev[["xaxis.range[0]"]]; x1 <- ev[["xaxis.range[1]"]]
      if (!is.null(x0) && !is.null(x1)) {
        x_range <<- if (is.numeric(x0)) {
          div <- ifelse(max(abs(c(x0, x1))) > 1e12, 1000, 1)
          as.POSIXct(c(x0, x1) / div, origin = "1970-01-01", tz = tz_user)
        } else as.POSIXct(c(x0, x1), tz = tz_user)}
      if (isTRUE(ev[["xaxis.autorange"]])) x_range <<- NULL

      y0 <- ev[["yaxis.range[0]"]]; y1 <- ev[["yaxis.range[1]"]]
      if (!is.null(y0) && !is.null(y1)) y_range <<- as.numeric(c(y0, y1))
      if (isTRUE(ev[["yaxis.autorange"]])) y_range <<- NULL})

    # -- helper to change flags -------------------------------------------------
    set_flag <- function(ids, value) {
      if (length(ids)) {
        dt[ids, (fcol) := value]
        redraw(TRUE)}}

    # -- navigation ------------------------------------------------------------
    observeEvent(input$prev_win, {
      if (current_win > 0) { current_win <<- current_win - 1L; redraw(FALSE) }})
    observeEvent(input$next_win, {
      if (current_win < length(win_rows) - 1L) {
        current_win <<- current_win + 1L; redraw(FALSE)}})
    observeEvent(input$jump, {
      j <- input$jump - 1L
      if (!is.na(j) && j >= 0L && j < length(win_rows) && j != current_win) {
        current_win <<- j; redraw(FALSE)}})
    observeEvent(input$home_zoom, {
      x_range <<- NULL
      y_range <<- NULL
      redraw(TRUE)})

    # -- point buttons ---------------------------------------------------------
    observeEvent(input$flag_sel,    set_flag(brushed_ids(), -2L))
    observeEvent(input$approve_sel, set_flag(brushed_ids(),  1L))
    observeEvent(input$unflag_sel,  set_flag(brushed_ids(),  0L))

    # -- window buttons --------------------------------------------------------
    observeEvent(input$flag_win, {
      dt[rows_now(), (fcol) := -2L]
      redraw(TRUE)})
    observeEvent(input$approve_unflagged, {
      r <- rows_now()
      good <- r[ dt[r, get(fcol) == 0L & !is.na(get(y_col))] ]
      if (length(good)) dt[good, (fcol) := 1L]
      redraw(TRUE)})
    observeEvent(input$reset_win, {
      dt[rows_now(), (fcol) := 0L]
      redraw(TRUE)})

    # flag selected & next
    observeEvent(input$flag_sel_next, {
      sel <- brushed_ids(); r <- rows_now()
      if (length(sel)) dt[sel, (fcol) := -2L]
      rest       <- setdiff(r, sel)
      unchecked  <- rest[ dt[rest, get(fcol) == 0L & !is.na(get(y_col))] ]
      if (length(unchecked)) dt[unchecked, (fcol) := 1L]
      if (current_win < length(win_rows) - 1L) current_win <<- current_win + 1L
      redraw(FALSE)})

    # other buttons
    observeEvent(input$hide_bad, redraw(TRUE))
    observeEvent(input$win_width, ignoreInit = TRUE, {
      hrs <- input$win_width
      if (is.finite(hrs) && hrs > 0) {
        win_rows <<- make_windows(hrs)
        current_win <<- min(current_win, length(win_rows) - 1L)
        redraw(FALSE)}})
    observeEvent(input$reset_all, {
      dt[get(fcol) != -1L, (fcol) := 0L]
      redraw(TRUE)})

    # always return dt, even on browser close
    session$onSessionEnded(function() {
      cols <- intersect(c(".rowid","win_id"), names(dt))
      if (length(cols)) dt[, (cols) := NULL]
      stopApp(as.data.frame(dt))})
    # done
    observeEvent(input$done, { session$close() })
  }
  runApp(shinyApp(ui, server))
}
