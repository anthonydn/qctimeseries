# qctimeseries

Window-by-window QC for large time series. Review flags in a Shiny/plotly app, track progress, and export cleaned data.

## Install

```r
# install.packages("remotes")
remotes::install_github("anthonydn/qctimeseries", build_vignettes = TRUE)
```

> Note: Building vignettes requires rmarkdown/knitr and Pandoc (RStudio includes Pandoc). If you hit issues, omit `build_vignettes = TRUE` and read the online README for usage.

## Quick start

```r
library(qctimeseries)

# Try the app on the included example data
src <- system.file("extdata", "sat_site_qc.RData", package = "qctimeseries")
if (!file.exists("sat_site_qc.RData")) file.copy(src, "sat_site_qc.RData")
load("sat_site_qc.RData")
qc_window_app(sat_site_qc)
```

### Vignette

```r
vignette("getting-started", package = "qctimeseries")
```

## Features

* Fixed-width windows with x/y zoom memory and a Home reset
* "Hide flagged" filter and secondary overlay (optional)
* Plotly scattergl for speed on large data
* Helpers: `qc_add_flags()`, `qc_progress()`, `qc_apply_flags()`, `qc_check_plot()`, `qc_transfer()`, `qc_remove_flags()`
* R Markdown template so non-R users can review by clicking Run

## License

GPL-3
