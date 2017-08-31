# R package to download data from the WID.world database

This package downloads data from the online World Wealth and
Income Database (WID.world) directly into R. The World Wealth and Income
Database is an extensive source on the historical evolution of the
distribution of income and wealth both within and between countries.
It relies on the combined effort of an international network of over a
hundred researchers covering more than seventy countries from all continents.

## Installation

The package is not yet available on CRAN. But you can install the
development version by typing:
```{r}
install.packages("devtools")
devtools::install_github("WIDworld/wid-r-tool")
```

## Usage

The package exports a single function `download_wid(...)`. See `?download_wid` for help.

## Demo

See `vignette("wid-demo")`, or [click here](https://github.com/WIDworld/wid-r-tool/raw/master/inst/doc/wid-demo.pdf) for a demonstration
of the package.
