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
#' df_qc <- qc_flags(df, vars = "x")
qc_flags <- function(data,
                     vars = NULL,
                     suffix = "_qcflag",
                     overwrite = FALSE,
                     require_numeric = TRUE) {
  stopifnot(is.data.frame(data))
  if (!nzchar(suffix)) stop("qc_flags(): `suffix` must be a non-empty string")

  # pick vars
  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    if (!length(vars)) stop("qc_flags(): no numeric columns found; supply `vars`")
  } else {
    unknown <- setdiff(vars, names(data))
    if (length(unknown))
      stop("qc_flags(): columns not found: ", paste(unknown, collapse = ", "))
  }

  # optional type check
  if (require_numeric) {
    non_num <- vars[!vapply(data[vars], is.numeric, logical(1))]
    if (length(non_num))
      stop("qc_flags(): non-numeric vars not allowed when `require_numeric=TRUE`: ",
           paste(non_num, collapse = ", "))
  }

  flag_names <- paste0(vars, suffix)
  existing <- intersect(flag_names, names(data))
  if (length(existing) && !isTRUE(overwrite)) {
    stop("qc_flags(): flag columns already exist: ",
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




#' QC progress summary
#'
#' @param data A data.frame returned by [qc_flags()].
#' @param quiet If `TRUE`, return invisibly without printing.
#' @return A tibble/data.frame with per-variable totals and percentages.
#' @details
#' Percentages are computed over **all rows** (including missing) by default.
#' If you prefer to compute over non-missing rows, set `denom = "valid"` (optional param).

qc_progress <- function(data, quiet = FALSE) {
  vars   <- attr(data, "qc_vars")
  suffix <- attr(data, "qc_suffix")
  if (is.null(vars) || is.null(suffix))
    stop("qc_progress(): data does not look like a qc() result")

  library(dplyr)

  res <- lapply(vars, function(v) {
    fcol  <- paste0(v, suffix)
    flag  <- data[[fcol]]
    val   <- data[[v]]

    n_all   <- length(val)
    n_miss  <- sum(is.na(val))
    n_valid <- n_all - n_miss          # “total” = non-NA rows

    n_app <- sum(flag ==  1L, na.rm = TRUE)
    n_rej <- sum(flag <   0L, na.rm = TRUE) - n_miss
    n_chk <- sum(flag !=   0L, na.rm = TRUE)

    tibble::tibble(
      variable     = v,
      total        = n_valid,
      pct_checked  = round(100 * n_chk / n_all, 2),
      pct_approved = round(100 * n_app / n_all, 2),
      pct_flagged  = round(100 * n_rej / n_all, 2),
      pct_missing  = round(100 * n_miss / n_all,  2)
    )
  }) |>
    bind_rows()

  if (!quiet) print(res, n = Inf)
  invisible(res)
}

#' Copy QC flags from one variable to another
#'
#' @param df Data frame with flag columns (from [qc_flags()]).
#' @param from Source variable name (without suffix).
#' @param to Destination variable name (without suffix).
#' @param suffix Flag suffix, default `"_qcflag"`.
#' @return The modified data frame (invisibly).
#' @examples
#' df <- qc_flags(data.frame(a=1:3, b=1:3), vars=c("a","b"))
#' df <- qc_transfer(df, from="a", to="b")
qc_transfer <- function(df, from, to, suffix = "_qcflag") {
  df[[paste0(to, suffix)]] <- df[[paste0(from, suffix)]]
  invisible(df)}



#' Apply QC flags to data (mask bad values)
#'
#' @param data Data frame produced by [qc_flags()] or [qc_window_app()].
#' @param suffix Flag suffix.
#' @param drop_flags Logical; drop `*_qcflag` columns after masking (default `TRUE`).
#' @return Cleaned data with `flag < 0` values set to `NA`.
qc_apply_flags <- function(data, suffix = "_qcflag") {
  stopifnot(is.data.frame(data))

  flag_cols <- grep(paste0(suffix, "$"), names(data), value = TRUE)
  if (!length(flag_cols))
    stop("qc_apply_flags(): no columns end with '", suffix, "'")

  out <- data                       # will mutate a copy

  for (fcol in flag_cols) {
    var <- sub(paste0(suffix, "$"), "", fcol)   # strip suffix
    if (!var %in% names(out)) next              # safety: flag w/o var
    bad <- out[[fcol]] < 0L                     # TRUE where flagged
    if (any(bad)) out[[var]][bad] <- NA
  }

  out[setdiff(names(out), flag_cols)]           # drop qcflag cols
}



#' Quick visual QC check (raw vs cleaned)
#'
#' @param data Data frame with raw vars and flag columns.
#' @param var Variable name to inspect (character).
#' @param suffix Flag suffix.
#' @param time_col Time column name. Default `"DateTime"`.
#' @return A ggplot object with two panels.
#' @examples
#' qc_check_plot(df, "temp")
qc_check_plot <- function(data, var, suffix = "_qcflag") {
  stopifnot(is.data.frame(data), var %in% names(data))
  fcol <- paste0(var, suffix)
  if (!fcol %in% names(data))
    stop("qc_check(): flag column '", fcol, "' not found")

  # ----------------------------------------------------------------------
  # build a tidy data.frame with one row per timestamp * per panel
  # ----------------------------------------------------------------------
  raw_df <- data.frame(
    DateTime = data$DateTime,
    y        = data[[var]],
    flag     = data[[fcol]],
    panel    = "raw"
  )
  clean_df <- raw_df
  clean_df$y[ clean_df$flag < 1 ] <- NA   # mask bad points
  clean_df$panel <- "clean"

  df <- rbind(raw_df, clean_df)

  fl <- qc_flag_levels()

  ggplot() +
    geom_point(data   = subset(df, panel == "raw"),
      mapping = aes(DateTime, y, colour = factor(flag)),
      size = 0.8, alpha = 0.9, stroke = 0) +
    geom_line(data   = subset(df, panel == "clean"),
      mapping = aes(DateTime, y),
      colour = "black") +
    scale_colour_manual(values = fl$colors, breaks = fl$levels,
      labels = fl$labels, drop = FALSE, name = NULL) +
    facet_grid(panel ~ ., scales = "free_y", labeller = labeller(panel =
      c(raw   = paste(var, "(raw)"), clean = paste(var, "(clean)")))) +
    labs(x = NULL, y = NULL,
         title = paste("QC check for", var)) +
    theme_bw(base_size = 10) +
    theme(legend.position = "bottom")}

qc_is_flagged_df <- function(x, suffix = "_qcflag") {
  !is.null(attr(x, "qc_vars")) && !is.null(attr(x, "qc_suffix")) ||
    any(endsWith(names(x), suffix))}

qc_flag_levels <- function() {
  list(levels = c("1","0","-1","-2"),
       labels = c("approved","unchecked","auto flag","manual flag"),
       colors = c(`1`="forestgreen",`0`="steelblue",`-1`="orange",`-2`="red"))}



