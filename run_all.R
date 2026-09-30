# run_all.R — Master orchestrator for the GoFundMe-Research pipeline
#
# Run from the project root (or anywhere — here::here() finds the .Rproj):
#   Rscript run_all.R
#
# Prerequisites:
#   1. source("setup.R") at least once to install required packages.
#   2. Data/ must contain the small reference files tracked in git plus the
#      large gitignored ones: laus_state_monthly.csv, laus_county_monthly.csv,
#      Data/raw files/* (raw GoFundMe scraped data, 11+ GB).
#   3. "State Weekly Claims for Unemplyment Insurance Data Not Seasonally
#      Adjusted.csv" — the raw DOL ETA-539 download has coded columns; the
#      pipeline expects human-readable headers. See Data/ TODO comments.
#
# What this does:
#   1. Load shared helpers (functions.R).
#   2. Run cleanData.R → builds state/county quarterly + label panels;
#      writes Data/raw files/{countyQuarter,countyLabel,stateQuarter,stateLabel}.csv
#   3. Run regressions.R → main TWFE regression results to console.
#   4. Run figures.R → exploratory plots to paper/figures/.
#   5. Run tables.R → summary stats table to paper/tables/summary_stats.tex.
#   6. Run text_mining.R → builds test_data with predicted categories.
#   7. Run fin emergency regressions.R → financial emergency category models.
#   8. Run benefit-optouts-TWFE.R → state benefit-cut weekly DiD.
#   9. Run CARES pipeline: cleanCARES → regressionsCARES → run_all_CARES.R
#      (event studies, Sun-Abraham, Poisson PPML, writes paper/tables/*.tex
#      and paper/figures/*.pdf).
#  10. Run Code/inline_stats.R → emit paper/inline_stats.tex \newcommand macros.
#
# Each step is wrapped in tryCatch so a single failure doesn't kill the run;
# the orchestrator reports per-step status at the end.

suppressPackageStartupMessages({
  library(here)
})

# ----------------------------------------------------------------------------
# Helper: run a script with timing + status reporting.
# ----------------------------------------------------------------------------
run_step <- function(label, path) {
  cat(sprintf("\n[%s] %s\n", format(Sys.time(), "%H:%M:%S"), label))
  cat(strrep("-", 72), "\n", sep = "")
  t0 <- Sys.time()
  status <- tryCatch({
    source(path, echo = FALSE, local = FALSE)
    "OK"
  }, error = function(e) {
    cat("  !! FAILED:", conditionMessage(e), "\n")
    paste0("FAIL: ", conditionMessage(e))
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  cat(sprintf("  -> %s (%.1fs)\n", status, dt))
  list(label = label, status = status, seconds = dt)
}

# ----------------------------------------------------------------------------
# Pipeline
# ----------------------------------------------------------------------------
results <- list()

results[[length(results) + 1L]] <- run_step("[1/9] Shared helpers",
  here::here("Code", "functions.R"))

results[[length(results) + 1L]] <- run_step("[2/9] cleanData (build panels)",
  here::here("Code", "cleanData.R"))

results[[length(results) + 1L]] <- run_step("[3/9] Main TWFE regressions",
  here::here("Code", "regressions.R"))

results[[length(results) + 1L]] <- run_step("[4/9] Exploratory figures",
  here::here("Code", "figures.R"))

results[[length(results) + 1L]] <- run_step("[5/9] Summary stats tables",
  here::here("Code", "tables.R"))

results[[length(results) + 1L]] <- run_step("[6/9] Text mining (random forest)",
  here::here("Code", "text_mining.R"))

results[[length(results) + 1L]] <- run_step("[7/9] Financial emergency regressions",
  here::here("Code", "fin emergency regressions.R"))

# CARES pipeline. cleanCARES depends on State Weekly Claims file; if missing,
# this step will fail and downstream CARES steps will too — that's fine,
# tryCatch keeps the overall run alive and the summary at the end will show
# what failed.
results[[length(results) + 1L]] <- run_step("[8/9] CARES pipeline orchestrator",
  here::here("Code", "CARES", "run_all_CARES.R"))

results[[length(results) + 1L]] <- run_step("[9/9] Emit inline stats macros",
  here::here("Code", "inline_stats.R"))

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
cat("\n", strrep("=", 72), "\n", sep = "")
cat("Pipeline summary\n")
cat(strrep("=", 72), "\n", sep = "")
for (r in results) {
  status_short <- if (r$status == "OK") "OK  " else "FAIL"
  cat(sprintf("  %s  %5.1fs  %s\n", status_short, r$seconds, r$label))
}
total_time <- sum(vapply(results, `[[`, numeric(1), "seconds"))
cat(strrep("=", 72), "\n", sep = "")
cat(sprintf("Total: %.1fs across %d steps\n", total_time, length(results)))

# Next step for the user: re-compile the paper to pick up new figures/tables/
# inline_stats:
#   cd paper && pdflatex -interaction=nonstopmode "GFM UI Paper.tex"
#   bibtex "GFM UI Paper" && pdflatex "GFM UI Paper.tex" && pdflatex "GFM UI Paper.tex"
