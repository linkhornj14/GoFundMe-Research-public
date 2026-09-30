# setup.R — One-time package installation for the GoFundMe-Research project
#
# Run this ONCE in a fresh R session before sourcing any analysis script:
#   source("setup.R")
#
# After packages are installed, scripts in Code/ load them via library() calls.
# No analysis script should call install.packages() directly.

required_packages <- c(
  # Core data wrangling
  "tidyverse",       # dplyr, ggplot2, tidyr, readr, stringr, etc.
  "data.table",      # large-CSV reads
  "janitor",         # clean_names()
  "fastDummies",     # dummy_cols()
  "zoo",             # date utilities
  "lubridate",       # date parsing (part of tidyverse but explicit)
  "here",            # project-relative paths (replaces hardcoded absolute paths)

  # Econometrics
  "fixest",          # feols() TWFE, sunab() event study
  "ggfixest",        # ggplot integration for fixest
  "sandwich",        # robust SEs
  "doBy",            # group-wise summaries

  # APIs / external data
  "tidycensus",      # Census ACS via API (requires CENSUS_API_KEY in .Renviron)
  "httr",            # HTTP requests
  "jsonlite",        # JSON parsing

  # Tables and reporting
  "xtable",          # legacy table output (tables.R)
  "stargazer",       # alternative regression table output

  # Visualization
  "plotly",          # interactive plots
  "geomtextpath",    # curve-following text labels (figuresCARES.R)

  # Text mining / classification
  "tm",              # corpus + DTM
  "tidytext",        # tidy text analysis
  "textTinyR",       # fast text utilities
  "randomForest",    # classifier
  "caret",           # ML harness

  # Demographic imputation
  "predictrace",     # race/ethnicity inference from names

  # Additional robustness (added 2026-05-14 from econometric review)
  "fwildclusterboot", # wild cluster bootstrap for few-cluster inference
  "HonestDiD",        # Rambachan-Roth (2023) sensitivity to pre-trends
  "did",              # Callaway-Sant'Anna (2021) group-time ATT
  "didimputation",    # Borusyak-Jaravel-Spiess imputation estimator
  "clubSandwich"      # CR3 jackknife cluster-robust SE
)

# Set a default CRAN mirror if none is configured (needed for non-interactive
# Rscript runs; interactive RStudio sessions usually have one set already).
if (is.null(getOption("repos")) ||
    identical(getOption("repos")[["CRAN"]], "@CRAN@") ||
    length(getOption("repos")) == 0) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing) > 0) {
  message("Installing ", length(missing), " missing package(s): ",
          paste(missing, collapse = ", "))
  install.packages(missing)
} else {
  message("All ", length(required_packages), " required packages already installed.")
}

invisible(NULL)
