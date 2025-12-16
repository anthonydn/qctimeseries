#' Quick visual QC check (raw vs cleaned)
#'
#' Plot the raw series colored by QC flag and the cleaned (masked) series below.
#'
#' @param data Data frame with raw variables and matching flag columns.
#' @param y_col  Character, variable name to inspect (e.g., "temp").
#' @param suffix Flag suffix used for QC columns. Default "_qcflag".
#' @param time_col Time column name. Default "DateTime".
#' @return A ggplot object with two vertical panels.
#' @seealso [qc_add_flags()], [qc_apply_flags()], [qc_progress()]
#' @examples
#' # Minimal example: data + flags + a couple of masked points
#' set.seed(1)
#' n <- 48
#' dat <- data.frame(
#'     time = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(n) * 3600,
#'     temp = sin(seq_len(n) / 6) + rnorm(n, 0, 0.1)
#' )
#' dat_qc <- qc_add_flags(dat)
#' dat_qc$temp_qcflag <- 1 # approve all
#' dat_qc$temp_qcflag[c(10, 20, 30)] <- -1 # mark a few as flagged
#' qc_check_plot(dat_qc, "temp", time_col = "time")
#' @importFrom ggplot2 ggplot geom_point geom_line scale_colour_manual
#' @importFrom ggplot2 facet_grid labeller labs theme_bw theme aes
#' @template see-vignette
#' @export
qc_check_plot <- function(data,
                          y_col,
                          suffix = "_qcflag",
                          time_col = "DateTime") {
    stopifnot(is.data.frame(data), length(y_col) == 1, is.character(y_col))
    if (!time_col %in% names(data)) {
        stop("qc_check_plot(): time column '", time_col, "' not found")
    }
    if (!y_col %in% names(data)) {
        stop("qc_check_plot(): variable '", y_col, "' not found")
    }

    fcol <- paste0(y_col, suffix)
    if (!fcol %in% names(data)) {
        stop("qc_check_plot(): flag column '", fcol, "' not found")
    }

    # tidy per-panel data
    raw_df <- data.frame(
        time = data[[time_col]],
        y = data[[y_col]],
        flag = data[[fcol]],
        panel = "raw"
    )
    clean_df <- raw_df
    clean_df$y[clean_df$flag < 1] <- NA
    clean_df$panel <- "clean"

    df <- rbind(raw_df, clean_df)
    df$panel <- factor(df$panel, levels = c("raw", "clean"))

    fl <- qc_flag_levels() # assumes you have this helper

    ggplot() +
        geom_point(
            data = subset(df, panel == "raw"),
            aes(time, y, colour = factor(flag)),
            size = 0.8, alpha = 0.9, stroke = 0, na.rm = TRUE
        ) +
        geom_line(
            data = subset(df, panel == "clean"),
            aes(time, y),
            colour = "black", na.rm = TRUE
        ) +
        scale_colour_manual(
            values = fl$colors, breaks = fl$levels,
            labels = fl$labels, drop = FALSE, name = NULL
        ) +
        facet_grid(panel ~ .,
            scales = "free_y",
            labeller = labeller(panel = c(
                raw = paste(y_col, "(raw)"),
                clean = paste(y_col, "(clean)")
            ))
        ) +
        labs(x = NULL, y = NULL, title = paste("QC check for", y_col)) +
        theme_bw(base_size = 10) +
        theme(legend.position = "bottom")
}

#' Save one tall PNG of QC plots for all flagged variables
#'
#' Stacks [qc_check_plot()] for **every** QC variable into a single tall PNG,
#' allocating a fixed height per variable. Useful for static review or archiving.
#'
#' Variables are discovered from `attr(data, "qc_vars")` when present; otherwise
#' they are inferred by stripping `attr(data, "qc_suffix")` (default `"_qcflag"`)
#' from column names that end with that suffix.
#'
#' @param data A `data.frame`/`data.table` containing time series and
#'   `*_qcflag` columns.
#' @param outfile Path to the PNG file to write (directories are created
#'   if needed).
#' @param width_in Image width in inches (default `25`).
#' @param per_var_in Image height per variable (in inches), stacked vertically
#'   (default `2.5`).
#' @param dpi Raster resolution in dots-per-inch (default `200`).
#'
#' @details Very tall images can exceed some viewers’ limits. If the computed
#' height exceeds ~30,000 px, a warning is issued; reduce `dpi` or `per_var_in`
#' (or export in chunks) if needed.
#'
#' @return Invisibly returns `outfile` (the path written).
#'
#' @seealso [qc_check_plot()], [qc_add_flags()], [qc_write_exports()]
#'
#' @export
#' @importFrom grDevices png dev.off
#' @importFrom grid grid.newpage pushViewport viewport grid.layout
#'
#' @examples
#' \dontrun{
#' # Minimal demo
#' d <- data.frame(
#'     DateTime = seq.POSIXt(Sys.time(), length.out = 1000, by = "hour"),
#'     temp = rnorm(1000)
#' )
#' d <- qc_add_flags(d, vars = "temp")
#' qc_save_all_plots_png(
#'     d,
#'     outfile = "exports/demo_checks.png",
#'     width_in = 20, per_var_in = 2, dpi = 200
#' )
#' }
qc_save_all_plots_png <- function(data,
                                  outfile = "exports/qc_checks.png",
                                  width_in = 25,
                                  per_var_in = 2.5,
                                  dpi = 200) {
    stopifnot(is.data.frame(data))

    # figure out QC vars
    suffix <- attr(data, "qc_suffix")
    if (is.null(suffix)) suffix <- "_qcflag"
    vars <- attr(data, "qc_vars")
    if (is.null(vars) || !length(vars)) {
        suffix_pat <- paste0(suffix, "$")
        vars <- sub(suffix_pat, "", grep(suffix_pat, names(data), value = TRUE))
    }
    if (!length(vars)) stop("No QC variables found (looked for '*", suffix, "').")

    plots <- lapply(vars, function(v) qc_check_plot(data, v))

    total_h_in <- max(1, length(plots)) * per_var_in
    height_px <- ceiling(total_h_in * dpi)
    if (height_px > 30000) {
        warning(
            "Output will be ", height_px,
            " px tall; some viewers may struggle. Lower dpi or split into chunks."
        )
    }

    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(outfile,
        width = width_in, height = total_h_in,
        units = "in", res = dpi, type = "cairo"
    )
    on.exit(grDevices::dev.off(), add = TRUE)

    grid::grid.newpage()
    # Layout: one column, N rows
    grid::pushViewport(grid::viewport(
        layout = grid::grid.layout(nrow = length(plots), ncol = 1)
    ))
    for (i in seq_along(plots)) {
        print(plots[[i]],
            vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1)
        )
    }
    invisible(outfile)
}

#' @noRd
#' @keywords internal
qc_flag_levels <- function() {
    list(
        levels = c("1", "0", "-1", "-2"),
        labels = c("approved", "unchecked", "auto flag", "manual flag"),
        colors = c(
            `1` = "forestgreen",
            `0` = "steelblue",
            `-1` = "orange",
            `-2` = "red"
        )
    )
}
