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
#' @import data.table
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
                          tz_user   = "America/Denver") {

  # --- load / sanity ----------------------------------------------------------
  stopifnot(is.data.frame(dat), "DateTime" %in% names(dat))

  if (!inherits(dat$DateTime, "POSIXct"))
    dat$DateTime <- as.POSIXct(dat$DateTime, tz = tz_user)

  fcol <- paste0(y_col, qc_suffix)
  if (!fcol %in% names(dat))
    stop("qc_window_app(): column '", fcol,
         "' not found.  Create flags with qc() first.")

  dt <- data.table::as.data.table(dat)
  if (!".rowid" %in% names(dt)) dt[, .rowid := .I]

  make_windows <- function(hrs) {
    start <- min(dt$DateTime, na.rm = TRUE)
    dt[, win_id := as.integer(
      floor(as.numeric(difftime(DateTime, start, "secs")) / (hrs * 3600))
    )]

    wins <- split(dt$.rowid, dt$win_id)
    wins <- Filter(function(idx) any(!is.na(dt[[y_col]][idx])), wins)
    names(wins) <- seq_along(wins) - 1L
    wins
  }
  win_rows <- make_windows(win_hrs)

  # --- state ------------------------------------------------------------------
  current_win <- 0L
  x_range     <- NULL

  # --- UI ---------------------------------------------------------------------
  ui <- shiny::fluidPage(
    # (UI block unchanged)
    ...
  )

  # --- server -----------------------------------------------------------------
  server <- function(input, output, session) {

    # always return dt, even on browser close
    session$onSessionEnded(function() {
      dt[, c(".rowid", "win_id") := NULL]
      shiny::stopApp(as.data.frame(dt))
    })

    rows_now <- function() win_rows[[as.character(current_win)]]

    brushed_ids <- function() {
      ev <- plotly::event_data("plotly_selected", source = "plot")
      if (is.null(ev) || !nrow(ev)) integer() else as.integer(ev$key)
    }

    build_plot <- function() {
      rows <- rows_now()
      wd   <- dt[rows]
      vals <- wd[[y_col]]

      xr <- if (is.null(x_range)) range(wd$DateTime, na.rm = TRUE) else x_range

      if (all(is.na(vals))) {
        return(
          plotly::plot_ly() %>%
            plotly::add_text(
              x = mean(xr), y = 0,
              text = "All values NA in this window",
              textposition = "middle center",
              inherit = FALSE, showlegend = FALSE
            ) %>%
            plotly::layout(
              xaxis = list(range = xr, title = "DateTime"),
              yaxis = list(title = y_col),
              dragmode = "zoom"
            )
        )
      }

      span <- diff(range(vals, na.rm = TRUE))
      yr   <- range(vals, na.rm = TRUE) + c(-1, 1) * 0.02 * span

      base_rows <- if (isTRUE(input$hide_bad))
        rows[dt[rows][[fcol]] >= 0L] else rows
      base_rows <- base_rows[!is.na(dt[[y_col]][base_rows])]

      p <- plotly::plot_ly(
        dt[base_rows],
        x     = ~DateTime,
        y     = ~get(y_col),
        type  = "scattergl",
        mode  = "lines+markers",
        marker = list(size = 4, color = "steelblue"),
        line   = list(width = 1),
        key    = ~.rowid,
        source = "plot"
      ) %>%
        plotly::layout(
          dragmode = "zoom",
          xaxis = list(range = xr),
          yaxis = list(range = yr, title = y_col)
        ) %>%
        plotly::event_register("plotly_selected") %>%
        plotly::event_register("plotly_relayout")

      add_pts <- function(idx, col) {
        if (!length(idx)) return(p)
        plotly::add_trace(
          p, data = dt[idx],
          x = ~DateTime, y = ~get(y_col),
          type = "scattergl", mode = "markers",
          marker = list(size = 6, color = col, opacity = 0.9),
          inherit = FALSE, showlegend = FALSE
        )
      }
      p <- add_pts(rows[dt[rows][[fcol]] == 1L], "green")
      if (!isTRUE(input$hide_bad))
        p <- add_pts(rows[dt[rows][[fcol]] < 0L], "red")
      p
    }

    redraw <- function(keep_zoom = TRUE) {
      if (!keep_zoom) x_range <<- NULL
      shiny::updateNumericInput(session, "jump", value = current_win + 1)
      output$tsplot <- plotly::renderPlotly(build_plot())
      rng <- range(dt[rows_now(), DateTime], na.rm = TRUE)
      output$win_label <- shiny::renderText(sprintf(
        "Window %d / %d   %s – %s",
        current_win + 1, length(win_rows),
        format(rng[1], "%Y-%m-%d %H:%M"),
        format(rng[2], "%Y-%m-%d %H:%M")
      ))
    }
    redraw()

    # remember zoom
    shiny::observeEvent(
      plotly::event_data("plotly_relayout", source = "plot"),
      {
        ev <- plotly::event_data("plotly_relayout", source = "plot")
        x0 <- ev[["xaxis.range[0]"]]; x1 <- ev[["xaxis.range[1]"]]
        if (is.null(x0) || is.null(x1)) return()
        x_range <<- if (is.numeric(x0)) {
          div <- ifelse(max(abs(c(x0, x1))) > 1e12, 1000, 1)
          as.POSIXct(c(x0, x1) / div, origin = "1970-01-01", tz = tz_user)
        } else as.POSIXct(c(x0, x1), tz = tz_user)
      }
    )

    # -- helper to change flags -------------------------------------------------
    set_flag <- function(ids, value) {
      if (length(ids)) {
        dt[ids, (fcol) := value]
        redraw(TRUE)
      }
    }

    # -- navigation ------------------------------------------------------------
    shiny::observeEvent(input$prev_win, {
      if (current_win > 0) { current_win <<- current_win - 1L; redraw(FALSE) }
    })
    shiny::observeEvent(input$next_win, {
      if (current_win < length(win_rows) - 1L) {
        current_win <<- current_win + 1L; redraw(FALSE)
      }
    })
    shiny::observeEvent(input$jump, {
      j <- input$jump - 1L
      if (!is.na(j) && j >= 0L && j < length(win_rows) && j != current_win) {
        current_win <<- j; redraw(FALSE)
      }
    })

    # -- point buttons ---------------------------------------------------------
    shiny::observeEvent(input$flag_sel,    set_flag(brushed_ids(), -2L))
    shiny::observeEvent(input$approve_sel, set_flag(brushed_ids(),  1L))
    shiny::observeEvent(input$unflag_sel,  set_flag(brushed_ids(),  0L))

    # -- window buttons --------------------------------------------------------
    shiny::observeEvent(input$flag_win, {
      dt[rows_now(), (fcol) := -2L]
      redraw(TRUE)
    })
    shiny::observeEvent(input$approve_unflagged, {
      r <- rows_now()
      good <- r[ dt[r, get(fcol) == 0L & !is.na(get(y_col))] ]
      if (length(good)) dt[good, (fcol) := 1L]
      redraw(TRUE)
    })
    shiny::observeEvent(input$reset_win, {
      dt[rows_now(), (fcol) := 0L]
      redraw(TRUE)
    })

    # approve ENTIRE & next
    shiny::observeEvent(input$approve_next, {
      r  <- rows_now()
      ok <- r[!is.na(dt[[y_col]][r])]
      dt[ok, (fcol) := 1L]
      if (current_win < length(win_rows) - 1L) current_win <<- current_win + 1L
      redraw(FALSE)
    })

    # flag selected & next
    shiny::observeEvent(input$flag_sel_next, {
      sel <- brushed_ids(); r <- rows_now()
      if (length(sel)) dt[sel, (fcol) := -2L]
      rest       <- setdiff(r, sel)
      unchecked  <- rest[ dt[rest, get(fcol) == 0L & !is.na(get(y_col))] ]
      if (length(unchecked)) dt[unchecked, (fcol) := 1L]
      if (current_win < length(win_rows) - 1L) current_win <<- current_win + 1L
      redraw(FALSE)
    })

    # misc toggles
    shiny::observeEvent(input$hide_bad, redraw(TRUE))
    shiny::observeEvent(input$win_width, ignoreInit = TRUE, {
      hrs <- input$win_width
      if (is.finite(hrs) && hrs > 0) {
        win_rows <<- make_windows(hrs)
        current_win <<- min(current_win, length(win_rows) - 1L)
        redraw(FALSE)
      }
    })
    shiny::observeEvent(input$reset_all, {
      dt[get(fcol) != -1L, (fcol) := 0L]
      redraw(TRUE)
    })

    # done
    shiny::observeEvent(input$done, {
      dt[, c(".rowid", "win_id") := NULL]
      shiny::stopApp(as.data.frame(dt))
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
}
