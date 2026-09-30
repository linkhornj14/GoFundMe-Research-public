## build_fpuc300_termination.R
## Builds the 2021 FPUC-$300 staggered-termination analysis panel for the
## "benefits-off" difference-in-differences robustness design (Tier 1, 1a).
##
## Why this design: the headline financial-emergency outcome did not exist
## during the FPUC-$600 onset window (it launched Oct 2020), so the staggered
## ONSET cannot identify it. The staggered EARLY TERMINATION of FPUC-$300 in
## summer 2021 is a clean "benefits turn off" event during which the
## financial-emergency category exists throughout. Expected sign: removing
## benefits should INCREASE financial-emergency campaigns (mirror image of the
## main result).
##
## The termination week is already encoded in the panel as fWeek2End:
##   early terminators -> ISO weeks ~24-31 (Jun-Jul 2021)
##   non-early states  -> week 36 (= federal expiration, 9/4/2021)
## This script derives a clean state -> termination crosswalk, cross-checks the
## early-terminator set against the public record, and writes the analysis panel.
##
## Run:  Rscript Code/CARES/build_fpuc300_termination.R

suppressPackageStartupMessages({ library(tidyverse); library(here) })

load(here::here(".RData"))
weekLabel <- ungroup(weekLabel)
weekCamp  <- ungroup(weekCamp)
out_dir <- here::here("Code", "replication", "exported")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## --- Publicly documented states that ended FPUC-$300 early (Jun-Jul 2021) ---
## Source: Coombs et al. (2022) and Holzer et al. (2021); 26 states.
public_early <- c("AL","AK","AZ","AR","FL","GA","ID","IN","IA","LA","MD","MS",
                  "MO","MT","NE","NH","ND","OH","OK","SC","SD","TN","TX","UT",
                  "WV","WY")

## --- Derive termination crosswalk from the panel (fWeek2End) ----------------
xwalk <- weekLabel %>%
  distinct(state, fWeek2End) %>%
  filter(!is.na(fWeek2End)) %>%
  mutate(
    term_week = as.integer(fWeek2End),
    early = term_week < 36L,                 # 36 = 9/4/2021 federal expiration
    in_public_list = state %in% public_early
  ) %>%
  arrange(term_week, state)

cat("== Termination week (fWeek2End) distribution ==\n")
print(table(xwalk$term_week, useNA = "ifany"))
cat("\nEarly terminators (panel):", sum(xwalk$early),
    " | Non-early:", sum(!xwalk$early), "\n")

## --- Cross-check panel-derived early set vs public record -------------------
panel_early <- sort(xwalk$state[xwalk$early])
disc_panel_not_public <- setdiff(panel_early, public_early)
disc_public_not_panel <- setdiff(public_early, panel_early)
cat("\n== Cross-check vs public early-terminator list ==\n")
cat("Panel-early not in public list:",
    if (length(disc_panel_not_public)) paste(disc_panel_not_public, collapse=", ") else "(none)", "\n")
cat("Public list not panel-early:   ",
    if (length(disc_public_not_panel)) paste(disc_public_not_panel, collapse=", ") else "(none)", "\n")
cat("Agreement:", length(intersect(panel_early, public_early)),
    "of", length(public_early), "public states matched.\n")

write_csv(xwalk, file.path(out_dir, "fpuc300_termination_xwalk.csv"))
cat("\nWrote fpuc300_termination_xwalk.csv\n")

## --- Build the 2021 analysis panel ------------------------------------------
## Observation window: 2021 weeks up to just before the federal expiration so
## the never-early states serve as clean not-yet-treated / never-treated
## controls. Treatment time (gname) = termination week for early terminators,
## 0 for non-early (never-treated within window).
WIN_START <- 6L     # ~early Feb 2021 (category stable, pre any termination)
WIN_END   <- 35L    # just before 9/4 federal expiration (week 36)

## weekLabel holds per-category counts + covariates; total campaign volume
## (campaigns/donationsTotal/amountTotal) lives in weekCamp -> join it in.
panel <- weekLabel %>%
  filter(year == "2021") %>%
  left_join(
    weekCamp %>% select(state, year, week, campaigns, donationsTotal, amountTotal),
    by = c("state", "year", "week")
  ) %>%
  left_join(xwalk %>% select(state, term_week, early), by = "state") %>%
  filter(!is.na(term_week)) %>%
  mutate(
    week = as.integer(week),
    gname = if_else(early, term_week, 0L),          # CS cohort; 0 = never (early) treated
    state_id = as.integer(factor(state))
  ) %>%
  filter(week >= WIN_START, week <= WIN_END) %>%
  ## outcomes + covariates needed downstream
  transmute(
    state, state_id, week, gname, early, term_week,
    num_financial_emergency, num_business, campaigns,
    donationsTotal, amountTotal,
    median_income, percent_poverty, total_population,
    insured_unemployment_rate, initial_claims, continued_claims, cases, puaIC
  )

cat("\n== Analysis panel ==\n")
cat("Rows:", nrow(panel), " States:", n_distinct(panel$state),
    " Weeks:", paste(range(panel$week), collapse="-"), "\n")
cat("Treated (early) states:", n_distinct(panel$state[panel$early]),
    " Control (never-early) states:", n_distinct(panel$state[!panel$early]), "\n")
cat("Mean num_financial_emergency:", round(mean(panel$num_financial_emergency, na.rm=TRUE), 3), "\n")

write_csv(panel, file.path(out_dir, "panel_termination_2021.csv"))
cat("Wrote panel_termination_2021.csv\n")
