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
#' @param write_parquet logical; also write Parquet if \pkg{arrow} available
#'   (default TRUE).
#' @param parquet_compression "zstd","snappy","gzip" (default "zstd").
#' @param write_xlsx logical; also write Excel .xlsx via \pkg{writexl}
#'   (default FALSE).
#' @param write_rds logical; also write RDS (default FALSE).
#' @return Invisibly, named list of written paths.
#' @importFrom stats setNames
#' @export
qc_write_exports <- function(data,
                             time_col = "DateTime",
                             out_dir = "exports",
                             base_name = NULL,
                             qc_suffix = "_qc",
                             clean_suffix = "_clean",
                             write_csv = TRUE,
                             csv_compress = TRUE,
                             na_csv = "NA",
                             write_parquet = TRUE,
                             parquet_compression = c("zstd", "snappy", "gzip"),
                             write_xlsx = FALSE,
                             write_rds = FALSE) {
    stopifnot(is.data.frame(data))
    parquet_compression <- match.arg(parquet_compression)
    if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Derive stem from object name if base_name not provided
    obj_name <- deparse(substitute(data))
    if (is.null(base_name) || !nzchar(base_name)) {
        # strip a trailing _qc or _clean if present (uses your current suffix args)
        esc <- function(x) gsub("([.^$|()*+?{}\\[\\]\\\\])", "\\\\\\1", x)
        strip_pat <- paste0("(", esc(qc_suffix), "|", esc(clean_suffix), ")$")
        base_name <- sub(strip_pat, "", obj_name, perl = TRUE)
        if (!nzchar(base_name)) base_name <- obj_name
    }
    name_qc <- paste0(base_name, qc_suffix)
    name_clean <- paste0(base_name, clean_suffix)

    # Build variants using your helper
    df_qc <- if (inherits(data, "data.table")) data.table::copy(data) else data
    df_clean <- qctimeseries::qc_apply_flags(data, drop_flags = TRUE)

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
        p_qc <- file.path(out_dir, paste0(name_qc, ext))
        p_clean <- file.path(out_dir, paste0(name_clean, ext))
        readr::write_csv(to_text_time(df_qc), p_qc, na = na_csv)
        readr::write_csv(to_text_time(df_clean), p_clean, na = na_csv)
        written$csv_qc <- p_qc
        written$csv_clean <- p_clean
    }

    # Parquet
    if (isTRUE(write_parquet) && requireNamespace("arrow", quietly = TRUE)) {
        p_qc <- file.path(out_dir, paste0(name_qc, ".parquet"))
        p_clean <- file.path(out_dir, paste0(name_clean, ".parquet"))
        arrow::write_parquet(df_qc, p_qc, compression = parquet_compression)
        arrow::write_parquet(df_clean, p_clean, compression = parquet_compression)
        written$parquet_qc <- p_qc
        written$parquet_clean <- p_clean
    }

    # XLSX (optional; chunk if >1,048,576 rows)
    if (isTRUE(write_xlsx)) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
            warning("write_xlsx=TRUE but 'writexl' not installed; skipping XLSX.")
        } else {
            write_xlsx_file <- function(df, path, base_sheet) {
                df2 <- to_text_time(df)
                n <- nrow(df2)
                max_rows <- 1048576L
                if (n == 0L) {
                    return(writexl::write_xlsx(setNames(list(df2), base_sheet), path))
                }
                parts <- (seq_len(n) - 1L) %/% max_rows + 1L
                idxs <- split(seq_len(n), parts)
                names <- if (length(idxs) == 1L) {
                    base_sheet
                } else {
                    paste0(base_sheet, "_", seq_along(idxs))
                }
                sheets <- setNames(lapply(seq_along(idxs), function(i) {
                    df2[idxs[[i]], , drop = FALSE]
                }), names)
                writexl::write_xlsx(sheets, path)
            }
            x_qc <- file.path(out_dir, paste0(name_qc, ".xlsx"))
            x_clean <- file.path(out_dir, paste0(name_clean, ".xlsx"))
            write_xlsx_file(df_qc, x_qc, "qc")
            write_xlsx_file(df_clean, x_clean, "clean")
            written$xlsx_qc <- x_qc
            written$xlsx_clean <- x_clean
        }
    }

    # RDS (optional)
    if (isTRUE(write_rds)) {
        r_qc <- file.path(out_dir, paste0(name_qc, ".rds"))
        r_clean <- file.path(out_dir, paste0(name_clean, ".rds"))
        saveRDS(df_qc, r_qc, compress = "xz")
        saveRDS(df_clean, r_clean, compress = "xz")
        written$rds_qc <- r_qc
        written$rds_clean <- r_clean
    }

    # Checksums
    if (length(written)) {
        files <- unlist(written, use.names = FALSE)
        md5 <- tools::md5sum(files)
        chk <- file.path(out_dir, paste0(base_name, "_checksums.md5"))
        con <- file(chk, open = "wt", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        for (i in seq_along(md5)) {
            cat(md5[[i]], "  ", names(md5)[i], "\n", sep = "", file = con)
        }
        written$checksums <- chk
    }

    invisible(written)
}
