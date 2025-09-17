# qctimeseries

Window-by-window QC for large time series. Review flags in a Shiny/plotly app, track progress, and export cleaned data.

## Install

```r

# install.packages("remotes")

# minimal (fastest; user manual vignette available online; link below)
remotes::install_github("anthonydn/qctimeseries")

# full (allows access to vignette locally)
remotes::install_github("anthonydn/qctimeseries", build_vignettes = TRUE)

# Note: You may see a warning about Rtools on Windows. This package is pure R and does
# not require compilation, so you can ignore that message.
```

## Quick start

```r
library(qctimeseries)

# Try the app on the included example data
load(system.file("extdata", "ibutton_example_qc.RData", package = "qctimeseries"))

qc_progress(ibutton_example_qc, hide_complete = FALSE)

ibutton_example_qc <- qc_window_app(ibutton_example_qc, 
  y_col = "temp", time_col = "datetime", win_hrs = 10000)
# nuke those high temps when the ibutton was taken back to the lab!
# see vignette below for how to use it 
```

### Vignette (user manual)

```r
vignette("overall_workflow", package = "qctimeseries")
```

Online here: <a href="http://anthony.darrouzet-nardi.net/qctimeseries/"
target="_blank" rel="noopener">anthony.darrouzet-nardi.net/qctimeseries</a>


## Features

* Fixed-width windows with x/y zoom memory and a Home reset
* "Hide flagged" filter and secondary overlay (optional)
* Plotly scattergl for speed on large data
* Helpers: `qc_add_flags()`, `qc_progress()`, `qc_apply_flags()`, `qc_check_plot()`, `qc_transfer()`, `qc_remove_flags()`
* R Markdown template so non-R users can review by clicking Run

## Known limitations

* Time zones have not gotten enough dedicated attention so be careful with them.  
* If you are using it now, you are an alpha tester, so let me know what
 bugs or inconveniences you find!

## License

GPL-3

## Acknowledgements

* Thanks to Kai Schmitt for early testing.
