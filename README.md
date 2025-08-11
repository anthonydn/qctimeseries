# qctimeseries

Window-by-window QC for large time series. Review flags in a Shiny/plotly app, track progress, and export cleaned data.

## Install

```r
# minimal (fastest; user manual vignette still available on web)
remotes::install_github("anthonydn/qctimeseries")

# full (allows access to vignette locally)
remotes::install_github("anthonydn/qctimeseries", build_vignettes = TRUE)
```

## Quick start

```r
library(qctimeseries)

# Try the app on the included example data
src <- system.file("extdata", "sat_site_qc.RData", package = "qctimeseries")
if (!file.exists("sat_site_qc.RData")) file.copy(src, "sat_site_qc.RData")
load("sat_site_qc.RData")
qc_progress(sat_site_qc, hide_complete = TRUE)
sat_site_qc <- qc_window_app(sat_site_qc, y_col = "away_eos_co2_flux", time_col = "DateTime")
qc_progress(sat_site_qc, hide_complete = TRUE)
```

### Vignette

```r
vignette("overall_workflow", package = "qctimeseries")

https://crustnet.org/qctimeseries/articles/overall_workflow.html
```

## Features

* Fixed-width windows with x/y zoom memory and a Home reset
* "Hide flagged" filter and secondary overlay (optional)
* Plotly scattergl for speed on large data
* Helpers: `qc_add_flags()`, `qc_progress()`, `qc_apply_flags()`, `qc_check_plot()`, `qc_transfer()`, `qc_remove_flags()`
* R Markdown template so non-R users can review by clicking Run

## License

GPL-3
