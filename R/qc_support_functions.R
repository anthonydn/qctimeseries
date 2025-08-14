#' Initialize QC flag columns
#'
#' Create integer flag columns for selected variables using the convention
#' 1 = approved, 0 = unchecked, -2 = manual flag, -1 = original NA.
#' Adds attributes `qc_vars` and `qc_suffix`.
#'
#' @param data A data.frame or data.table of raw values.
#' @param vars Character vector of columns to flag. If NULL, all numeric columns.
#' @param suffix Suffix to use for flag columns. Default "_qcflag".
#' @param overwrite Logical; overwrite existing flag columns if present. Default FALSE.
#' @param require_numeric Logical; if TRUE and `vars` contains non-numeric columns, error. Default TRUE.
#' @return Same class as `data` with new integer flag columns appended.
#' @examples
#' df <- data.frame(x = c(1, NA, 3), y = 4:6)
#' df_qc <- qc_add_flags(df, vars = "x")
#' @template see-vignette
#' @export
qc_add_flags <- function(data,
                     vars = NULL,
                     suffix = "_qcflag",
                     overwrite = FALSE,
                     require_numeric = TRUE) {
  stopifnot(is.data.frame(data))
  if (!nzchar(suffix)) stop("qc_add_flags(): `suffix` must be a non-empty string")

  # pick vars
  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    if (!length(vars)) stop("qc_add_flags(): no numeric columns found; supply `vars`")
  } else {
    unknown <- setdiff(vars, names(data))
    if (length(unknown))
      stop("qc_add_flags(): columns not found: ", paste(unknown, collapse = ", "))
  }

  # optional type check
  if (require_numeric) {
    non_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
    if (length(non_num))
      stop("qc_add_flags(): non-numeric vars not allowed when `require_numeric=TRUE`: ",
           paste(non_num, collapse = ", "))
  }

  flag_names <- paste0(vars, suffix)
  existing <- intersect(flag_names, names(data))
  if (length(existing) && !isTRUE(overwrite)) {
    stop("qc_add_flags(): flag columns already exist: ",
         paste(existing, collapse = ", "),
         "\nSet `overwrite=TRUE` to replace them.")
  }

  # build flags
  flags <- lapply(vars, function(v) {
    f <- integer(nrow(data))     # zeros
    f[is.na(data[[v]])] <- -1L   # auto-flag original NAs
    f
  })
  names(flags) <- flag_names

  # append while preserving class
  if (inherits(data, "data.table")) {
    out <- data.table::copy(data)
    for (j in seq_along(flag_names)) {
      data.table::set(out, j = flag_names[j], value = flags[[j]])
    }
  } else {
    out <- cbind(data, as.data.frame(flags, optional = TRUE))
  }

  # merge attrs (extend if already present)
  old_vars   <- attr(data, "qc_vars")
  out_vars   <- sort(unique(c(old_vars, vars)))
  attr(out, "qc_vars")   <- out_vars
  attr(out, "qc_suffix") <- suffix

  out
}

#' Remove QC flag columns
#'
#' Drop one or more `*_qcflag` columns and update attributes.
#'
#' @param data   data.frame/data.table from [qc_add_flags()].
#' @param vars   character; base variable names to drop flags for. `NULL` = all.
#' @param suffix flag suffix; defaults to `attr(data,"qc_suffix")` or `"_qcflag"`.
#' @param strict error if a requested flag column is missing (default FALSE).
#' @return Same class as `data`, with attributes updated.
#' @template see-vignette
#' @export
qc_remove_flags <- function(data, vars = NULL, suffix = NULL, strict = FALSE) {
  stopifnot(is.data.frame(data))
  if (is.null(suffix)) suffix <- attr(data, "qc_suffix")
  if (is.null(suffix)) suffix <- "_qcflag"

  flag_cols <- names(data)[endsWith(names(data), suffix)]
  if (!length(flag_cols)) return(data)

  target <- if (is.null(vars)) flag_cols else paste0(vars, suffix)
  missing <- setdiff(target, names(data))
  if (length(missing) && strict)
    stop("qc_remove_flags(): not found: ", paste(missing, collapse = ", "))

  rm_cols <- intersect(target, names(data))
  if (!length(rm_cols)) return(data)

  # remove columns, preserve class & order
  if (inherits(data, "data.table")) {
    out <- data.table::copy(data)
    out[, (rm_cols) := NULL]
  } else {
    out <- data[, setdiff(names(data), rm_cols), drop = FALSE]
  }

  # refresh attrs from what's left
  left_flags <- names(out)[endsWith(names(out), suffix)]
  if (length(left_flags)) {
    attr(out, "qc_vars")   <- sub(paste0(suffix, "$"), "", left_flags)
    attr(out, "qc_suffix") <- suffix
  } else {
    attr(out, "qc_vars") <- attr(out, "qc_suffix") <- NULL
  }
  out
}



#' QC progress summary
#'
#' @param data A data.frame returned by [qc_add_flags()].
#' @param quiet If `TRUE`, return invisibly without printing.
#' @param hide_complete If `TRUE`, only return rows with `pct_checked < 100`.
#' @return A tibble/data.frame with per-variable totals and percentages.
#' @details
#' Percentages are computed over **all rows** (including missing) by default.
#' @template see-vignette
#' @export
qc_progress <- function(data, quiet = FALSE, hide_complete = FALSE) {
  vars   <- attr(data, "qc_vars")
  suffix <- attr(data, "qc_suffix")
  if (is.null(vars) || is.null(suffix))
    stop("qc_progress(): data does not look like a qc_add_flags() result")

  res <- lapply(vars, function(v) {
    fcol  <- paste0(v, suffix)
    flag  <- data[[fcol]]
    val   <- data[[v]]

    n_all   <- length(val)
    n_miss  <- sum(is.na(val))
    n_valid <- n_all - n_miss

    n_app <- sum(flag ==  1L, na.rm = TRUE)
    n_rej <- sum(flag <   0L, na.rm = TRUE) - n_miss
    n_chk <- sum(flag !=   0L, na.rm = TRUE)

    tibble::tibble(
      variable     = v,
      total        = n_valid,
      pct_checked  = round(100 * n_chk / n_all, 2),
      pct_approved = round(100 * n_app / n_all, 2),
      pct_flagged  = round(100 * n_rej / n_all, 2),
      pct_missing  = round(100 * n_miss / n_all, 2)
    )
  }) |>
    dplyr::bind_rows()

  if (hide_complete) {
    res <- dplyr::filter(res, pct_checked < 100)
  }

  if (!quiet) print(res, n = Inf)
  invisible(res)
}


#' Copy QC flags from one variable to another
#'
#' @param df Data frame/data.table with flag columns (from [qc_add_flags()]).
#' @param from Source variable name (without suffix).
#' @param to Destination variable name (without suffix).
#' @param suffix Flag suffix, default "_qcflag".
#' @return The modified data frame (invisibly).
#' @examples
#' df <- qc_add_flags(data.frame(a=1:3, b=1:3), vars=c("a","b"))
#' df <- qc_transfer(df, from="a", to="b")
#' @template see-vignette
#' @export
qc_transfer <- function(df, from, to, suffix = "_qcflag") {
  stopifnot(is.data.frame(df), length(from) == 1, length(to) == 1)

  f_from <- paste0(from, suffix)
  f_to   <- paste0(to,   suffix)
  if (!f_from %in% names(df))
    stop("qc_transfer(): flag column not found: ", f_from)

  val <- df[[f_from]]

  if (inherits(df, "data.table")) {
    data.table::set(df, j = f_to, value = val)  # by reference, no copy
  } else {
    df[[f_to]] <- val
  }

  # keep QC attrs in sync
  vars <- unique(c(attr(df, "qc_vars"), to))
  attr(df, "qc_vars")   <- vars[!is.na(vars)]
  if (is.null(attr(df, "qc_suffix"))) attr(df, "qc_suffix") <- suffix

  invisible(df)
}


#' Apply QC flags to data (mask bad values)
#'
#' @param data Data frame produced by [qc_add_flags()] or [qc_window_app()].
#' @param suffix Flag suffix.
#' @param drop_flags Logical; drop `*_qcflag` columns after masking (default TRUE).
#' @return Cleaned data with `flag < 0` values set to NA.
#' @template see-vignette
#' @export
qc_apply_flags <- function(data, suffix = "_qcflag", drop_flags = TRUE) {
  stopifnot(is.data.frame(data))

  flag_cols <- names(data)[endsWith(names(data), suffix)]
  if (!length(flag_cols))
    stop("qc_apply_flags(): no columns end with '", suffix, "'")

  # work on a copy if data.table
  out <- if (inherits(data, "data.table")) data.table::copy(data) else data

  for (fcol in flag_cols) {
    var <- sub(paste0(suffix, "$"), "", fcol)
    if (!var %in% names(out)) next
    bad <- out[[fcol]] < 0L
    if (any(bad, na.rm = TRUE)) out[[var]][bad] <- NA
  }

  if (isTRUE(drop_flags)) {
    if (inherits(out, "data.table")) {
      out[, (flag_cols) := NULL]
    } else {
      out <- out[, setdiff(names(out), flag_cols), drop = FALSE]
    }
  }
  out
}

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
#'   time = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(n) * 3600,
#'   temp = sin(seq_len(n)/6) + rnorm(n, 0, 0.1))
#' dat_qc <- qc_add_flags(dat)
#' dat_qc$temp_qcflag <- 1  # approve all
#' dat_qc$temp_qcflag[c(10, 20, 30)] <- -1  # mark a few as flagged
#' qc_check_plot(dat_qc, "temp", time_col = "time")
#' @importFrom ggplot2 ggplot geom_point geom_line scale_colour_manual
#' @importFrom ggplot2 facet_grid labeller labs theme_bw theme aes
#' @template see-vignette
#' @export
qc_check_plot <- function(data, y_col, suffix = "_qcflag", time_col = "DateTime") {
  stopifnot(is.data.frame(data), length(y_col) == 1, is.character(y_col))
  if (!time_col %in% names(data))
    stop("qc_check_plot(): time column '", time_col, "' not found")
  if (!y_col %in% names(data))
    stop("qc_check_plot(): variable '", y_col, "' not found")

  fcol <- paste0(y_col, suffix)
  if (!fcol %in% names(data))
    stop("qc_check_plot(): flag column '", fcol, "' not found")

  # tidy per-panel data
  raw_df <- data.frame(
    time = data[[time_col]],
    y    = data[[y_col]],
    flag = data[[fcol]],
    panel = "raw")
  clean_df <- raw_df
  clean_df$y[clean_df$flag < 1] <- NA
  clean_df$panel <- "clean"

  df <- rbind(raw_df, clean_df)
  df$panel <- factor(df$panel, levels = c("raw", "clean"))

  fl <- qc_flag_levels()  # assumes you have this helper

  ggplot() +
    geom_point(data = subset(df, panel == "raw"),
      aes(time, y, colour = factor(flag)),
      size = 0.8, alpha = 0.9, stroke = 0, na.rm = TRUE) +
    geom_line(data = subset(df, panel == "clean"),
      aes(time, y),
      colour = "black", na.rm = TRUE) +
    scale_colour_manual(values = fl$colors, breaks = fl$levels,
      labels = fl$labels, drop = FALSE, name = NULL) +
    facet_grid(panel ~ ., scales = "free_y",
      labeller = labeller(panel = c(raw = paste(y_col, "(raw)"),
        clean = paste(y_col, "(clean)")))) +
    labs(x = NULL, y = NULL, title = paste("QC check for", y_col)) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")
}

#' @noRd
#' @keywords internal
qc_is_flagged_df <- function(x, suffix = NULL) {
  if (!is.data.frame(x)) return(FALSE)
  has_attrs <- !is.null(attr(x, "qc_vars")) && !is.null(attr(x, "qc_suffix"))
  if (has_attrs) return(TRUE)
  if (is.null(suffix)) suffix <- attr(x, "qc_suffix")
  if (is.null(suffix)) suffix <- "_qcflag"
  any(endsWith(names(x), suffix))}

#' @noRd
#' @keywords internal
qc_flag_levels <- function() {list(
    levels = c("1","0","-1","-2"),
    labels = c("approved","unchecked","auto flag","manual flag"),
    colors = c(`1`="forestgreen", `0`="steelblue", `-1`="orange", `-2`="red"))}


#' Write cleaned + QC (with flags) exports, with smart names
#'
#' Uses the object name as the stem. If your object is `sat_site_qc`,
#' files will be `sat_site_qc.*` (QC) and `sat_site_clean.*` (clean).
#'
#' @param data data.frame/data.table with *_qcflag columns.
#' @param time_col POSIXct time column (for CSV/XLSX rendering).
#' @param out_dir output directory (created if missing).
#' @param base_name optional override for the stem; default = name of `data`
#'   with any trailing `_qc` / `_clean` removed.
#' @param qc_suffix suffix for QC (with-flags) files. Default "_qc".
#' @param clean_suffix suffix for clean files. Default "_clean".
#' @param write_csv logical; write CSV outputs (default TRUE).
#' @param csv_compress logical; if TRUE write .csv.gz else .csv (default TRUE).
#' @param na_csv CSV missing value marker (default "NA").
#' @param write_parquet logical; also write Parquet if {arrow} available (default TRUE).
#' @param parquet_compression "zstd","snappy","gzip" (default "zstd").
#' @param write_xlsx logical; also write Excel .xlsx via {writexl} (default FALSE).
#' @param write_rds logical; also write RDS (default FALSE).
#' @return Invisibly, named list of written paths.
#' @export
#' @importFrom readr write_csv
qc_write_exports <- function(
    data,
    time_col            = "DateTime",
    out_dir             = "exports",
    base_name           = NULL,
    qc_suffix           = "_qc",
    clean_suffix        = "_clean",
    write_csv           = TRUE,
    csv_compress        = TRUE,
    na_csv              = "NA",
    write_parquet       = TRUE,
    parquet_compression = c("zstd","snappy","gzip"),
    write_xlsx          = FALSE,
    write_rds           = FALSE
) {
  stopifnot(is.data.frame(data))
  parquet_compression <- match.arg(parquet_compression)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Derive stem from object name if base_name not provided
  # Derive stem from object name if base_name not provided
  obj_name <- deparse(substitute(data))
  if (is.null(base_name) || !nzchar(base_name)) {
    # strip a trailing _qc or _clean if present (uses your current suffix args)
    esc <- function(x) gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", x)
    strip_pat <- paste0("(", esc(qc_suffix), "|", esc(clean_suffix), ")$")  # e.g., (_qc|_clean)$
    base_name <- sub(strip_pat, "", obj_name, perl = TRUE)
    if (!nzchar(base_name)) base_name <- obj_name
  }
  name_qc    <- paste0(base_name, qc_suffix)
  name_clean <- paste0(base_name, clean_suffix)

  # Build variants using your helper
  df_qc    <- qctimeseries::qc_apply_flags(data, drop_flags = FALSE) # keep *_qcflag
  df_clean <- qctimeseries::qc_apply_flags(data, drop_flags = TRUE)  # drop *_qcflag

  # Render POSIXct to UTC ISO-8601 for text outputs
  to_text_time <- function(df) {
    if (time_col %in% names(df) && inherits(df[[time_col]], "POSIXct")) {
      df[[time_col]] <- format(df[[time_col]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    df
  }

  written <- list()

  # CSV / CSV.gz
  if (isTRUE(write_csv)) {
    ext <- if (csv_compress) ".csv.gz" else ".csv"
    p_qc    <- file.path(out_dir, paste0(name_qc,    ext))
    p_clean <- file.path(out_dir, paste0(name_clean, ext))
    readr::write_csv(to_text_time(df_qc),    p_qc,    na = na_csv)
    readr::write_csv(to_text_time(df_clean), p_clean, na = na_csv)
    written$csv_qc    <- p_qc
    written$csv_clean <- p_clean
  }

  # Parquet
  if (isTRUE(write_parquet) && requireNamespace("arrow", quietly = TRUE)) {
    p_qc    <- file.path(out_dir, paste0(name_qc,    ".parquet"))
    p_clean <- file.path(out_dir, paste0(name_clean, ".parquet"))
    arrow::write_parquet(df_qc,    p_qc,    compression = parquet_compression)
    arrow::write_parquet(df_clean, p_clean, compression = parquet_compression)
    written$parquet_qc    <- p_qc
    written$parquet_clean <- p_clean
  }

  # XLSX (optional; chunk if >1,048,576 rows)
  if (isTRUE(write_xlsx)) {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      warning("write_xlsx=TRUE but package 'writexl' is not installed; skipping XLSX.")
    } else {
      write_xlsx_file <- function(df, path, base_sheet) {
        df2 <- to_text_time(df)
        n <- nrow(df2); max_rows <- 1048576L
        if (n == 0L) return(writexl::write_xlsx(setNames(list(df2), base_sheet), path))
        parts  <- (seq_len(n) - 1L) %/% max_rows + 1L
        idxs   <- split(seq_len(n), parts)
        names  <- if (length(idxs) == 1L) base_sheet else paste0(base_sheet, "_", seq_along(idxs))
        sheets <- setNames(lapply(seq_along(idxs), function(i) df2[idxs[[i]], , drop = FALSE]), names)
        writexl::write_xlsx(sheets, path)
      }
      x_qc    <- file.path(out_dir, paste0(name_qc,    ".xlsx"))
      x_clean <- file.path(out_dir, paste0(name_clean, ".xlsx"))
      write_xlsx_file(df_qc,    x_qc,    "qc")
      write_xlsx_file(df_clean, x_clean, "clean")
      written$xlsx_qc    <- x_qc
      written$xlsx_clean <- x_clean
    }
  }

  # RDS (optional)
  if (isTRUE(write_rds)) {
    r_qc    <- file.path(out_dir, paste0(name_qc,    ".rds"))
    r_clean <- file.path(out_dir, paste0(name_clean, ".rds"))
    saveRDS(df_qc,    r_qc,    compress = "xz")
    saveRDS(df_clean, r_clean, compress = "xz")
    written$rds_qc    <- r_qc
    written$rds_clean <- r_clean
  }

  # Checksums
  if (length(written)) {
    files <- unlist(written, use.names = FALSE)
    md5   <- tools::md5sum(files)
    chk   <- file.path(out_dir, paste0(base_name, "_checksums.md5"))
    con <- file(chk, open = "wt", encoding = "UTF-8"); on.exit(close(con), add = TRUE)
    for (i in seq_along(md5)) cat(md5[[i]], "  ", names(md5)[i], "\n", sep = "", file = con)
    written$checksums <- chk
  }

  invisible(written)
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
#' @param data A `data.frame`/`data.table` containing time series and `*_qcflag` columns.
#' @param outfile Path to the PNG file to write (directories are created if needed).
#' @param width_in Image width in inches (default `25`).
#' @param per_var_in Image height per variable (in inches), stacked vertically (default `2.5`).
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
#'   DateTime = seq.POSIXt(Sys.time(), length.out = 1000, by = "hour"),
#'   temp = rnorm(1000)
#' )
#' d <- qc_add_flags(d, vars = "temp")
#' qc_save_all_plots_png(
#'   d,
#'   outfile = "exports/demo_checks.png",
#'   width_in = 20, per_var_in = 2, dpi = 200
#' )
#' }
qc_save_all_plots_png <- function(data,
                                  outfile = "exports/qc_checks.png",
                                  width_in  = 25,
                                  per_var_in= 2.5,
                                  dpi       = 200) {
  stopifnot(is.data.frame(data))

  # figure out QC vars
  suffix <- attr(data, "qc_suffix"); if (is.null(suffix)) suffix <- "_qcflag"
  vars <- attr(data, "qc_vars")
  if (is.null(vars) || !length(vars)) {
    vars <- sub(paste0(suffix, "$"), "", grep(paste0(suffix, "$"), names(data), value = TRUE))
  }
  if (!length(vars)) stop("No QC variables found (looked for '*", suffix, "').")

  plots <- lapply(vars, function(v) qc_check_plot(data, v))

  total_h_in <- max(1, length(plots)) * per_var_in
  height_px  <- ceiling(total_h_in * dpi)
  if (height_px > 30000)
    warning("Output will be ", height_px,
            " px tall; some viewers may struggle. Lower dpi or split into chunks.")

  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(outfile, width = width_in, height = total_h_in,
                 units = "in", res = dpi, type = "cairo")
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow = length(plots), ncol = 1)))
  for (i in seq_along(plots)) {
    print(plots[[i]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1))
  }
  invisible(outfile)
}
