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
#' @param var  Character, variable name to inspect (e.g., "temp").
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
qc_check_plot <- function(data, var, suffix = "_qcflag", time_col = "DateTime") {
  stopifnot(is.data.frame(data), length(var) == 1, is.character(var))
  if (!time_col %in% names(data))
    stop("qc_check_plot(): time column '", time_col, "' not found")
  if (!var %in% names(data))
    stop("qc_check_plot(): variable '", var, "' not found")

  fcol <- paste0(var, suffix)
  if (!fcol %in% names(data))
    stop("qc_check_plot(): flag column '", fcol, "' not found")

  # tidy per-panel data
  raw_df <- data.frame(
    time = data[[time_col]],
    y    = data[[var]],
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
      labeller = labeller(panel = c(raw = paste(var, "(raw)"),
        clean = paste(var, "(clean)")))) +
    labs(x = NULL, y = NULL, title = paste("QC check for", var)) +
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
qc_flag_levels <- function() {
  list(
    levels = c("1","0","-1","-2"),
    labels = c("approved","unchecked","auto flag","manual flag"),
    colors = c(`1`="forestgreen", `0`="steelblue", `-1`="orange", `-2`="red"))}
