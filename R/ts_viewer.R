#' Lightweight Time-Series Viewer
#'
#' Launch a Shiny app to interactively browse a large numeric time-series.
#' Data are split into fixed-width windows (e.g. 168 hours) so only one slice
#' is rendered at a time, allowing smooth interaction even with large datasets.
#' This viewer is read-only - it does not require or modify any QC flag columns.
#'
#' @param dat A `data.frame` or `data.table` containing at least a POSIXct
#'   time column and one numeric column to view.
#' @param y_col `string`. Name of the numeric column to display.
#' @param win_hrs Window width in hours (default 168 = one week).
#' @param time_col Name of the POSIXct column. Defaults to `"DateTime"`.
#' @param tz_user Time zone used for display (affects x-axis labels only).
#' @param main_height Initial height (in pixels) for the main time-series plot.
#'   Sets the starting size of the top plot; can be adjusted in-app via the
#'   “Main height (px)” control. Default is `440`.
#' @param sec_height Initial height (in pixels) for the secondary plot.
#'   Sets the starting size of the bottom plot; can be adjusted in-app via the
#'   “Secondary height (px)” control. Default is `200`. Reduce down to a
#'   minimum of 10 to effectively hide the secondary plot.
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of launching
#'   the viewer.
#' @export
#' @import data.table
#' @import shiny
#' @import plotly
#'
#' @examples
#' if (interactive()) {
#'   dummy <- data.frame(
#'     DateTime = seq.POSIXt(Sys.time(), length.out = 5000, by = "hour"),
#'     temp     = rnorm(5000, 20, 2)
#'   )
#'   ts_viewer(dummy, y_col = "temp")
#' }
ts_viewer <- function(dat,
                      y_col,
                      win_hrs    = 168,
                      time_col   = "DateTime",
                      tz_user    = "America/Denver",
                      main_height = 440,
                      sec_height  = 200) {

  # --- basic checks ---------------------------------------------------------
  if (!inherits(dat[[time_col]], "POSIXct")) {
    cls <- paste(class(dat[[time_col]]), collapse = "/")
    fmt <- paste0("ts_viewer(): `%s` must be POSIXct (found: %s). ",
                  "Convert before calling, e.g. dat[[\"%s\"]] <- as.POSIXct(",
                  "dat[[\"%s\"]], tz = \"UTC\") or lubridate::ymd_hms(...).")
    stop(sprintf(fmt, time_col, cls, time_col, time_col))
  }

  if (!y_col %in% names(dat)) {
    stop("ts_viewer(): column '", y_col, "' not found in `dat`.")
  }

  dt <- as.data.table(dat)
  if (!".rowid" %in% names(dt)) dt[, .rowid := .I]

  make_windows <- function(hrs) {
    start <- min(dt[[time_col]], na.rm = TRUE)
    dt[, win_id := as.integer(floor(
      as.numeric(difftime(get(time_col), start, "secs")) / (hrs * 3600)
    ))]
    wins <- split(dt$.rowid, dt$win_id)
    # keep only windows that have any non-NA y
    wins <- Filter(function(idx) any(!is.na(dt[[y_col]][idx])), wins)
    names(wins) <- seq_along(wins) - 1L
    wins
  }

  win_rows <- make_windows(win_hrs)

  # --- state ----------------------------------------------------------------
  current_win <- 0L
  x_range     <- NULL
  y_range     <- NULL

  # --- UI -------------------------------------------------------------------
  ui <- fluidPage(
    # html layout and design
    tags$head(tags$style(HTML(
      "html, body { overflow-y: auto; }
       .btn-row { display:flex; justify-content:center; flex-wrap:wrap; gap:6px; }
       .btn-row .btn, .btn-row .form-control { margin:3px 4px; height:34px; }
       .label-inline { font-weight:600; display:flex; align-items:center;
         margin-right:8px; }
       .filters {display:flex; justify-content:center; align-items:center;
         flex-wrap:wrap; gap:10px 16px;}
       .filters .control-label { font-weight:600; margin:0 6px 0 0; }
       .filters .shiny-input-container { margin:0; }
       .filters .checkbox, .filters .checkbox-inline { margin:0; }
       .filters input[type='checkbox'] { margin-right:6px; }
       .filters .divider { padding:0 10px; opacity:0.5; }"
    ))),

    # hotkeys for navigation & dragmode
    tags$script(HTML(
      "(function () {
          function getGD(id){
            var el=document.getElementById(id);
            return el && el.querySelector('.js-plotly-plot');
          }
          function setMode(mode){
            var gd=getGD('tsplot');
            if (gd) Plotly.relayout(gd,{dragmode:mode});
            Shiny.setInputValue('hot_dragmode', mode, {priority:'event'});
          }
          function clickId(id){
            var el = document.getElementById(id); if (el) el.click();
          }
          var lastMode = null;

          document.addEventListener('keydown', function(e){
            var t=(e.target&&e.target.tagName||'').toLowerCase();
            if (['input','textarea','select'].includes(t)) return;

            // plotly temporary pan (hold Space)
            if (e.code==='Space'){
              if (lastMode===null){
                var gd=getGD('tsplot');
                lastMode = gd && gd._fullLayout && gd._fullLayout.dragmode || 'zoom';
                setMode('pan');
              }
              e.preventDefault();
              return;
            }

            // plotly dragmode hotkeys
            if (e.key==='b'||e.key==='B'){ lastMode=null; setMode('select');
              e.preventDefault(); return; }
            if (e.key==='q'||e.key==='Q'){ lastMode=null; setMode('lasso');
              e.preventDefault(); return; }
            if (e.key==='l'||e.key==='L'){ lastMode=null; setMode('lasso');
              e.preventDefault(); return; }
            if (e.key==='z'||e.key==='Z'){ lastMode=null; setMode('zoom');
              e.preventDefault(); return; }
            if (e.key==='p'||e.key==='P'){ lastMode=null; setMode('pan');
              e.preventDefault(); return; }

            // interface button hotkeys
            if (e.key==='h'||e.key==='H'){
              Shiny.setInputValue('hotkey_home', Date.now(),
                {priority:'event'});
              e.preventDefault(); return;
            }
            if (e.key==='ArrowLeft'){  clickId('prev_win');
              e.preventDefault(); return; }
            if (e.key==='ArrowRight'){ clickId('next_win');
              e.preventDefault(); return; }
          });

          document.addEventListener('keyup', function(e){
            if (e.code==='Space' && lastMode!==null){
              setMode(lastMode);
              lastMode=null;
              e.preventDefault();
            }
          });
        })()"
    )),

    textOutput("win_label"),
    uiOutput("tsplot_ui"),
    uiOutput("secplot_ui"),
    tags$hr(style="margin:4px 0;"),

    div(
      class="btn-row",
      actionButton("prev_win","Prev"),
      actionButton("next_win","Next"),
      numericInput("jump", NULL, 1, min = 1, width = "80px"),
      actionButton("home_zoom", "Home zoom"),
      span(class="label-inline","Secondary:"),
      selectInput("sec_var", NULL,
                  choices = character(0),
                  selected = NULL,
                  width="300px")
    ),

    tags$hr(style="margin:4px 0;"),

    div(
      class="btn-row",
      actionButton("done", "Done / Return", class="btn-primary"),
      numericInput("win_width","Window (hrs):" , win_hrs,
                   min = 1, width = "90px"),
      numericInput("h_main", "Main height (px):", value = main_height,
                   min = 100, max = 1600, step = 10, width = "100px"),
      numericInput("h_sec",  "Secondary height (px):", value = sec_height,
                   min = 10,  max = 800,  step = 10, width = "100px")
    )
  )

  # --- server ---------------------------------------------------------------
  server <- function(input, output, session) {

    # dragmode tracking from JS
    dragmode <- reactiveVal("zoom")
    observeEvent(input$hot_dragmode, ignoreInit = TRUE, {
      dm <- input$hot_dragmode
      if (is.character(dm) && nzchar(dm)) dragmode(dm)
    })

    # secondary variable options (exclude time and helper cols)
    sec_choices <- names(dt)
    sec_choices <- setdiff(sec_choices, c(time_col, y_col, ".rowid", "win_id"))
    updateSelectInput(
      session, "sec_var",
      choices  = c("", sec_choices),
      selected = ""
    )

    rows_now <- function() win_rows[[as.character(current_win)]]

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
      c(t0 - pad, t1 + pad)
    }

    # plot height management
    output$tsplot_ui <- renderUI({
      h <- if (is.null(input$h_main) || !is.finite(input$h_main))
        main_height else as.integer(input$h_main)
      plotlyOutput("tsplot", height = paste0(h, "px"))
    })
    output$secplot_ui <- renderUI({
      h <- if (is.null(input$h_sec) || !is.finite(input$h_sec))
        sec_height else as.integer(input$h_sec)
      plotlyOutput("secplot", height = paste0(h, "px"))
    })

    observeEvent(input$h_main, ignoreInit = TRUE, { redraw(TRUE, TRUE) })
    observeEvent(input$h_sec,  ignoreInit = TRUE, { redraw(TRUE, TRUE) })

    clamp <- function(x, lo, hi, default) {
      x <- suppressWarnings(as.numeric(x))
      if (!is.finite(x)) default else max(lo, min(hi, x))
    }
    ts_h  <- reactive({ clamp(input$h_main, 100, 1600, main_height) })
    sec_h <- reactive({ clamp(input$h_sec,   10,  800, sec_height) })

    observeEvent(ts_h(), ignoreInit = TRUE, {
      if (!identical(ts_h(), input$h_main))
        updateNumericInput(session, "h_main", value = ts_h())
    })
    observeEvent(sec_h(), ignoreInit = TRUE, {
      if (!identical(sec_h(), input$h_sec))
        updateNumericInput(session, "h_sec", value = sec_h())
    })

    build_plot <- function() {
      rows <- rows_now()
      base_rows <- rows[
        !is.na(dt[[y_col]][rows]) & !is.na(dt[[time_col]][rows])
      ]
      if (!length(base_rows)) {
        return(plotly_empty(type = "scattergl", mode = "lines+markers"))
      }

      xr <- if (is.null(x_range)) window_xr0() else x_range

      if (is.null(y_range)) {
        yr0  <- range(dt[[y_col]][base_rows], na.rm = TRUE)
        ypad <- diff(yr0) * 0.02
        yr   <- yr0 + c(-ypad, ypad)
      } else {
        yr <- y_range
      }

      plot_ly(
        dt[base_rows],
        x     = ~get(time_col),
        y     = ~get(y_col),
        type  = "scattergl",
        mode  = "lines+markers",
        marker = list(size = 4, color = "steelblue"),
        line   = list(width = 1, color = "gray"),
        key    = ~.rowid,
        source = "plot"
      ) %>%
        layout(
          dragmode = dragmode(),
          xaxis = list(range = xr, title = list(text = "")),
          yaxis = list(range = yr, title = y_col),
          uirevision = current_win
        ) %>%
        event_register("plotly_relayout") %>%
        config(
          modeBarButtonsToRemove = c("autoScale2d","resetScale2d"),
          scrollZoom = TRUE
        )
    }

    build_sec_plot <- function() {
      var <- input$sec_var
      if (is.null(var) || var == "" || !(var %in% names(dt))) {
        return(plotly_empty(type = "scattergl", mode = "lines"))
      }

      rows <- rows_now()
      good <- rows[
        !is.na(dt[[var]][rows]) & !is.na(dt[[time_col]][rows])
      ]
      if (!length(good)) {
        return(plotly_empty(type = "scattergl", mode = "lines"))
      }

      xr <- if (is.null(x_range)) window_xr0() else x_range

      plot_ly(
        dt[good],
        x = as.formula(paste0("~", time_col)),
        y = ~get(var),
        type = "scattergl",
        mode = "lines",
        line = list(width = 1, color = "orange")
      ) %>%
        layout(
          dragmode = "zoom",
          xaxis = list(range = xr),
          yaxis = list(title = var)
        ) %>%
        config(
          modeBarButtonsToRemove = c("autoScale2d", "resetScale2d")
        )
    }

    redraw <- function(keep_x = TRUE, keep_y = TRUE) {
      if (!keep_x) x_range <<- NULL
      if (!keep_y) y_range <<- NULL
      updateNumericInput(session, "jump", value = current_win + 1)
      output$secplot <- renderPlotly(build_sec_plot())
      output$tsplot  <- renderPlotly(build_plot())
      rng <- range(dt[rows_now(), get(time_col)], na.rm = TRUE)
      if (all(is.finite(rng))) {
        output$win_label <- renderText(sprintf(
          "Window %d / %d   %s - %s",
          current_win + 1,
          length(win_rows),
          format(rng[1], "%Y-%m-%d %H:%M"),
          format(rng[2], "%Y-%m-%d %H:%M")
        ))
      } else {
        output$win_label <- renderText(sprintf(
          "Window %d / %d", current_win + 1, length(win_rows)
        ))
      }
    }

    redraw()

    # remember zoom ranges
    observeEvent(
      event_data("plotly_relayout", source = "plot"),
      ignoreInit = TRUE, {
        ev <- event_data("plotly_relayout", source = "plot")
        x0 <- ev[["xaxis.range[0]"]]; x1 <- ev[["xaxis.range[1]"]]
        if (!is.null(x0) && !is.null(x1)) {
          x_range <<- if (is.numeric(x0)) {
            div <- ifelse(max(abs(c(x0, x1))) > 1e12, 1000, 1)
            as.POSIXct(c(x0, x1) / div, origin = "1970-01-01", tz = tz_user)
          } else {
            as.POSIXct(c(x0, x1), tz = tz_user)
          }
        }
        if (isTRUE(ev[["xaxis.autorange"]])) x_range <<- NULL

        y0 <- ev[["yaxis.range[0]"]]; y1 <- ev[["yaxis.range[1]"]]
        if (!is.null(y0) && !is.null(y1)) y_range <<- as.numeric(c(y0, y1))
        if (isTRUE(ev[["yaxis.autorange"]])) y_range <<- NULL
      }
    )

    # navigation
    observeEvent(input$prev_win, {
      if (current_win > 0) {
        current_win <<- current_win - 1L
        redraw(FALSE)
      }
    })
    observeEvent(input$next_win, {
      if (current_win < length(win_rows) - 1L) {
        current_win <<- current_win + 1L
        redraw(FALSE)
      }
    })
    observeEvent(input$jump, {
      j <- input$jump - 1L
      if (!is.na(j) && j >= 0L && j < length(win_rows) && j != current_win) {
        current_win <<- j
        redraw(FALSE)
      }
    })
    observeEvent(input$home_zoom, {
      x_range <<- NULL
      y_range <<- NULL
      redraw(TRUE)
    })
    observeEvent(input$hotkey_home, {
      x_range <<- NULL
      y_range <<- NULL
      redraw(TRUE)
    })

    # window width change - recompute windows
    observeEvent(input$win_width, ignoreInit = TRUE, {
      hrs <- input$win_width
      if (is.finite(hrs) && hrs > 0) {
        win_rows <<- make_windows(hrs)
        current_win <<- min(current_win, max(length(win_rows) - 1L, 0L))
        redraw(FALSE)
      }
    })

    # close behavior
    session$onSessionEnded(function() {
      cols <- intersect(c(".rowid","win_id"), names(dt))
      if (length(cols)) dt[, (cols) := NULL]
      stopApp(invisible(NULL))
    })
    observeEvent(input$done, { session$close() })
  }

  runApp(shinyApp(ui, server))
  invisible(NULL)
}
