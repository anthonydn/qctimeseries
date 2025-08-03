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
#' @return A `data.frame` identical to `dat` but with modified QC flag column.
#' @export
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
#                          autosave  = NULL,
                          tz_user   = "America/Denver") {

  library(shiny); library(plotly); library(data.table)

  # ---------- load / sanity ----------
  stopifnot(is.data.frame(dat), "DateTime" %in% names(dat))
  if (!inherits(dat$DateTime, "POSIXct"))
    dat$DateTime <- as.POSIXct(dat$DateTime, tz = tz_user)

  fcol <- paste0(y_col, qc_suffix)
  if (!fcol %in% names(dat))
    stop("qc_window_app(): column '", fcol,
         "' not found.  Create flags with qc() first.")

# restore from autosave if present
#  if (!is.null(autosave) && file.exists(autosave))
#    dat <- readRDS(autosave)

  dt <- as.data.table(dat)              # mutable copy for speed
  if (!".rowid" %in% names(dt)) dt[, .rowid := .I]

  make_windows <- function(hrs) {
    start <- min(dt$DateTime, na.rm = TRUE)
    dt[, win_id := as.integer(
      floor(as.numeric(difftime(DateTime, start, "secs")) / (hrs*3600)) )]

    # initial split → list(row-ids)
    wins <- split(dt$.rowid, dt$win_id)

    # **NEW** keep only windows that contain ≥1 non-NA y-value
    wins <- Filter(function(idx) any(!is.na(dt[[y_col]][idx])), wins)

    names(wins) <- seq_along(wins) - 1L

    wins
  }

    win_rows <- make_windows(win_hrs)

  # ---------- state ----------
  current_win <- 0L
  x_range     <- NULL
  ## replace #save_now()
  # save_now <- function() {
  #   if (!is.null(autosave)) {
  #     # write only if 2 s have passed since last write
  #     if (is.null(attr(#save_now, "last")) ||
  #         difftime(Sys.time(), attr(#save_now, "last"), units = "secs") > 2) {
  #       attr(#save_now, "last") <<- Sys.time()
  #       qs::qsave(as.data.frame(dt), autosave, preset = "fast")
  #     }
  #   }
  #}


  # ---------- UI ----------
  ui <- fluidPage(
    tags$head(tags$style("
      .btn-row{display:flex;justify-content:center;flex-wrap:wrap;gap:6px}
      .btn-row .btn,.btn-row .form-control{margin:3px 4px;height:34px;}
      .label-inline{font-weight:bold;display:flex;align-items:center;}
    ")),
    h4(paste("QC:", y_col)),
    textOutput("win_label"),
    plotlyOutput("tsplot", height = 440),
    tags$hr(style="margin:4px 0;"),
    div(class="btn-row",
        actionButton("prev_win","Prev"),
        actionButton("next_win","Next"),
        numericInput("jump", NULL, 1, min = 1, width = "80px"),
        actionButton("approve_next","Approve ENTIRE & Next ▶︎",
                     class="btn-success"),
        actionButton("flag_sel_next",
                     "Flag Selected ➜ Approve others & Next",
                     class="btn-danger")
    ),
    div(class="btn-row",
        actionButton("flag_sel",   "Flag Selected Points", class="btn-danger"),
        actionButton("unflag_sel", "Unflag Selected Points"),
        actionButton("approve_sel","Approve Selected Points",
                     class="btn-success")
    ),
    div(class="btn-row",
        actionButton("flag_win",        "Flag ENTIRE Window",
                     class="btn-danger"),
        actionButton("approve_unflagged","Approve ALL Unflagged",
                     class="btn-success"),
        actionButton("reset_win",       "Reset Window → Unchecked")
    ),
    tags$hr(style="margin:4px 0;"),
    div(class="btn-row",
        checkboxInput("hide_bad","Hide flagged (red)", FALSE),
        span(class="label-inline","Window (hrs):"),
        numericInput("win_width",NULL,win_hrs,min=1,width="90px"),
        actionButton("reset_all","Reset ALL → Unchecked"),
        actionButton("done",     "Done / Return", class="btn-primary")
    )
  )

  # ---------- server ----------
  server <- function(input, output, session){

    # ensure we always return `dt` even if the user clicks the X
    session$onSessionEnded(function() {
      dt[, c(".rowid","win_id") := NULL]
      stopApp(as.data.frame(dt))
    })

    rows_now <- function() win_rows[[as.character(current_win)]]
    brushed_ids <- function(){
      ev <- event_data("plotly_selected", source = "plot")
      if (is.null(ev) || !nrow(ev)) integer() else as.integer(ev$key)
    }

    build_plot <- function() {
      rows <- rows_now()
      wd   <- dt[rows]
      vals <- wd[[y_col]]

      # x-range always okay (DateTime is non-NA in your data)
      xr <- if (is.null(x_range)) range(wd$DateTime, na.rm = TRUE) else x_range

      if (all(is.na(vals))) {
        # No data: return a dummy plot with message and skip rest
        p <- plot_ly() %>%
          add_text(x = mean(xr), y = 0, text = "All values NA in this window",
                   textposition = "middle center", inherit = FALSE, showlegend = FALSE) %>%
          layout(xaxis = list(range = xr, title = "DateTime"),
                 yaxis = list(title = y_col),
                 dragmode = "zoom")
        return(p)
      }

      # normal path
      span <- diff(range(vals, na.rm = TRUE))
      yr   <- range(vals, na.rm = TRUE) + c(-1, 1) * 0.02 * span

      base_rows <- if (isTRUE(input$hide_bad))
        rows[dt[rows][[fcol]] >= 0L] else rows

      base_rows <- base_rows[!is.na(dt[[y_col]][base_rows])]

      p <- plot_ly(dt[base_rows], x = ~DateTime, y = ~get(y_col),
                   type = "scattergl", mode = "lines+markers",
                   marker = list(size = 4, color = "steelblue"),
                   line   = list(width = 1),
                   key = ~.rowid, source = "plot") %>%
        layout(dragmode = "zoom",
               xaxis = list(range = xr),
               yaxis = list(range = yr, title = y_col)) %>%
        event_register("plotly_selected") %>%
        event_register("plotly_relayout")

      add_pts <- function(idx, col) {
        if (!length(idx)) return(p)
        add_trace(p, data = dt[idx],
                  x = ~DateTime, y = ~get(y_col),
                  type = "scattergl", mode = "markers",
                  marker = list(size = 6, color = col, opacity = 0.9),
                  inherit = FALSE, showlegend = FALSE)
      }
      p <- add_pts(rows[dt[rows][[fcol]] == 1L], "green")
      if (!isTRUE(input$hide_bad))
        p <- add_pts(rows[dt[rows][[fcol]] < 0L], "red")
      p
    }

    redraw <- function(keep_zoom = TRUE){
      if (!keep_zoom) x_range <<- NULL
      updateNumericInput(session, "jump", value = current_win + 1)
      output$tsplot <- renderPlotly(build_plot())
      rng <- range(dt[rows_now(), DateTime], na.rm = TRUE)
      output$win_label <- renderText(sprintf(
        "Window %d / %d   %s – %s",
        current_win + 1, length(win_rows),
        format(rng[1], "%Y-%m-%d %H:%M"),
        format(rng[2], "%Y-%m-%d %H:%M")))
    }
    redraw()

    # remember zoom
    observeEvent(event_data("plotly_relayout", source = "plot"), {
      ev <- event_data("plotly_relayout", source = "plot")
      x0 <- ev[["xaxis.range[0]"]]; x1 <- ev[["xaxis.range[1]"]]
      if (is.null(x0) || is.null(x1)) return()
      x_range <<- if (is.numeric(x0)) {
        div <- ifelse(max(abs(c(x0,x1))) > 1e12, 1000, 1)
        as.POSIXct(c(x0,x1)/div, origin = "1970-01-01", tz = tz_user)
      } else as.POSIXct(c(x0,x1), tz = tz_user)
    })

    # utility to change flags and autosave
    set_flag <- function(ids, value) {
      if (length(ids)) {
        dt[ids, (fcol) := value]  # update in-place
        #save_now()
        redraw(TRUE)
      }
    }

    # ---- navigation (reset zoom) ----
    observeEvent(input$prev_win,{
      if (current_win > 0) { current_win <<- current_win-1L; redraw(FALSE) }
    })
    observeEvent(input$next_win,{
      if (current_win < length(win_rows)-1L) {
        current_win <<- current_win+1L; redraw(FALSE)
      }
    })
    observeEvent(input$jump,{
      j <- input$jump - 1L
      if (!is.na(j) && j >= 0L && j < length(win_rows) && j != current_win) {
        current_win <<- j; redraw(FALSE)
      }
    })

    # ---- point buttons ----
    observeEvent(input$flag_sel,    set_flag(brushed_ids(), -2L))
    observeEvent(input$approve_sel, set_flag(brushed_ids(),  1L))
    observeEvent(input$unflag_sel,  set_flag(brushed_ids(),  0L))

    # ---- window buttons ----
    observeEvent(input$flag_win, {
      r <- rows_now(); dt[r, (fcol) := -2L]
      #save_now();
      redraw(TRUE)
    })
    observeEvent(input$approve_unflagged, {
      r <- rows_now()
      good <- r[ dt[r, get(fcol) == 0L & !is.na(get(y_col))] ]  ## <-- changed
      if (length(good)) dt[good, (fcol) := 1L]
      redraw(TRUE)
    })
    observeEvent(input$reset_win, {
      r <- rows_now(); dt[r, (fcol) := 0L]
      #save_now();
      redraw(TRUE)
    })

    # approve ENTIRE & next
    observeEvent(input$approve_next, {
      r <- rows_now()
      ok <- r[ !is.na(dt[[y_col]][r]) ]          ## skip NA
      dt[ok, (fcol) := 1L]
      if (current_win < length(win_rows)-1L) current_win <<- current_win+1L
      redraw(FALSE)
    })
    # flag selected & next
    observeEvent(input$flag_sel_next, {
      sel <- brushed_ids(); r <- rows_now()

      # 1) flag selected
      if (length(sel)) dt[sel, (fcol) := -2L]

      # 2) approve everything else in the window that is still 0
      rest       <- setdiff(r, sel)
      unchecked <- rest[ dt[rest, get(fcol) == 0L & !is.na(get(y_col))] ]
      if (length(unchecked)) dt[unchecked, (fcol) := 1L]

      # 3) advance window, save, redraw
      if (current_win < length(win_rows) - 1L)
        current_win <<- current_win + 1L

      #save_now()
      redraw(FALSE)
    })

    # hide toggle
    observeEvent(input$hide_bad, redraw(TRUE))

    # window width change
    observeEvent(input$win_width, ignoreInit = TRUE, {
      hrs <- input$win_width
      if (is.finite(hrs) && hrs > 0) {
        win_rows <<- make_windows(hrs)
        current_win <<- min(current_win, length(win_rows)-1L)
        redraw(FALSE)
      }
    })

    # reset ALL
    observeEvent(input$reset_all, {
      dt[get(fcol) != -1L, (fcol) := 0L]
      #save_now();
      redraw(TRUE)
    })

    # ---- done ----
    observeEvent(input$done, {
#      if (!is.null(autosave) && file.exists(autosave))
#        file.remove(autosave)
      # drop internal columns before returning
      dt[, c(".rowid", "win_id") := NULL]
      stopApp(as.data.frame(dt))
    })
  }

  # run the GUI; return value goes to caller
  runApp(shinyApp(ui, server))
}
