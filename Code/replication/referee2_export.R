## referee2_export.R  — REFEREE 2 INDEPENDENT REPLICATION (READ-ONLY)
## This is the referee's own script. It does NOT modify any author file.
## Purpose:
##   1. Load the author's saved .RData workspace.
##   2. Export the analytic panels weekCamp / weekLabel to CSV so the
##      cross-language (Python / Stata) replication runs on the EXACT
##      same data the author's regressions use.
##   3. Re-run the author's main specifications (diff1/diff2/diff3) in R
##      and dump a tidy reference table of the `treat` coefficient,
##      cluster-robust SE, p-value, and N for each outcome.
##
## Run from repo root:
##   Rscript Code/replication/referee2_export.R

suppressPackageStartupMessages({
  library(fixest)
  library(tidyverse)
  library(here)
})

out_dir <- here::here("Code", "replication", "exported")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("Loading author workspace .RData ...\n")
load(here::here(".RData"))

weekCamp  <- ungroup(weekCamp)
weekLabel <- ungroup(weekLabel)

cat("weekCamp :", nrow(weekCamp),  "x", ncol(weekCamp),  "\n")
cat("weekLabel:", nrow(weekLabel), "x", ncol(weekLabel), "\n")

## Export the panels (these feed the Python + Stata replications) -------------
readr::write_csv(weekCamp,  file.path(out_dir, "weekCamp.csv"))
readr::write_csv(weekLabel, file.path(out_dir, "weekLabel.csv"))
cat("Wrote weekCamp.csv and weekLabel.csv to", out_dir, "\n")

## Helper: pull the treat row out of a single fixest model -------------------
treat_row <- function(model, dv_label) {
  ct <- as.data.frame(coeftable(model))
  if (!("treat" %in% rownames(ct))) {
    return(data.frame(outcome = dv_label, coef = NA, se = NA,
                      tval = NA, pval = NA, n = model$nobs))
  }
  r <- ct["treat", ]
  data.frame(
    outcome = dv_label,
    coef = unname(r[[1]]),
    se   = unname(r[[2]]),
    tval = unname(r[[3]]),
    pval = unname(r[[4]]),
    n    = model$nobs
  )
}

## ---- diff1: overall outcomes (full controls, as author wrote it) ----------
diff1 <- feols(
  c(log(campaigns), log(donationsTotal), log(amountTotal)) ~
    treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster = ~state, weights = ~total_population, data = weekCamp
)

## ---- diff2: by category, log(x+1) -----------------------------------------
diff2 <- feols(
  c(log(num_emergency + 1), log(num_medical + 1), log(num_memorial + 1),
    log(num_financial_emergency + 1), log(num_family + 1),
    log(num_volunteer + 1), log(num_community + 1),
    log(num_business + 1), log(num_education + 1)) ~
    treat + median_income +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster = ~state, weights = ~total_population, data = weekLabel
)

## ---- diff3: fin emergency, post-launch period -----------------------------
official <- weekLabel %>% filter((week >= 40 & year == "2020") | year == "2021")
diff3 <- feols(
  log(num_financial_emergency + 1) ~
    treat + median_income +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster = ~state, weights = ~total_population, data = official
)

ref <- bind_rows(
  treat_row(diff1[[1]], "diff1_log_campaigns"),
  treat_row(diff1[[2]], "diff1_log_donationsTotal"),
  treat_row(diff1[[3]], "diff1_log_amountTotal"),
  treat_row(diff2[[1]], "diff2_log_num_emergency_p1"),
  treat_row(diff2[[2]], "diff2_log_num_medical_p1"),
  treat_row(diff2[[3]], "diff2_log_num_memorial_p1"),
  treat_row(diff2[[4]], "diff2_log_num_financial_emergency_p1"),
  treat_row(diff2[[5]], "diff2_log_num_family_p1"),
  treat_row(diff2[[6]], "diff2_log_num_volunteer_p1"),
  treat_row(diff2[[7]], "diff2_log_num_community_p1"),
  treat_row(diff2[[8]], "diff2_log_num_business_p1"),
  treat_row(diff2[[9]], "diff2_log_num_education_p1"),
  treat_row(diff3,      "diff3_log_num_financial_emergency_p1_postlaunch")
)

print(ref, digits = 8)
readr::write_csv(ref, file.path(out_dir, "referee_R_reference.csv"))
cat("Wrote referee_R_reference.csv\n")

## Also dump the exact column types / a few diagnostics for the report -------
diag <- tibble(
  metric = c("weekCamp_rows", "weekLabel_rows",
             "weekCamp_treated_share", "weekLabel_treated_share",
             "weekCamp_states", "weekLabel_states",
             "official_rows"),
  value = c(nrow(weekCamp), nrow(weekLabel),
            mean(weekCamp$treat), mean(weekLabel$treat),
            dplyr::n_distinct(weekCamp$state),
            dplyr::n_distinct(weekLabel$state),
            nrow(official))
)
print(diag)
readr::write_csv(diag, file.path(out_dir, "referee_R_diagnostics.csv"))
cat("Done.\n")
