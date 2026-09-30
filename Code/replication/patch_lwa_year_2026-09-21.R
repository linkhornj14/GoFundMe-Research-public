## patch_lwa_year_2026-09-21.R
## One-time patch of the saved .RData workspace for the LWA year bug.
##
## Bug 1: the LWA clause of `treat` in cleanCARES.R had no year restriction, so
## LWA (a 2020-only program) re-activated in fall 2021 for the same ISO weeks.
## Bug 2: for SD (no LWA dates) the NA LWA clause propagated through the nested
## ifelse and replace_na() set SD's 2021 FPUC-300 weeks to 0.
## cleanCARES.R is now fixed at the source. Rebuilding the workspace from raw
## data requires the Census API and the 1 GB raw files, so this script applies
## the identical corrected rule to the saved weekCamp / weekLabel panels.
##
## Safety: it first checks that the OLD rule reproduces the stored `treat`
## exactly, so the patch changes nothing except the bug. Back up .RData first.
##
## Inputs : .RData
## Outputs: .RData (weekCamp, weekLabel patched);
##          Code/replication/exported/weekCamp.csv, weekLabel.csv (re-exported)
## Run    : Rscript Code/replication/patch_lwa_year_2026-09-21.R          (dry run)
##          Rscript Code/replication/patch_lwa_year_2026-09-21.R --write  (apply)

suppressPackageStartupMessages({ library(tidyverse); library(here) })
write_out <- "--write" %in% commandArgs(trailingOnly = TRUE)

ws <- new.env()
load(here::here(".RData"), envir = ws)

treat_rule <- function(df, lwa_2020_only) {
  lwa <- ((df$week >= df$lWeekStart & df$week <= df$lWeekEnd) |
            (df$week >= df$lWeekStart & df$lWeekEnd == 1))
  if (lwa_2020_only) lwa <- !is.na(df$lWeekStart) & lwa & (df$year == 2020)
  out <- ifelse((df$week >= df$fWeek & df$week <= df$fWeek1End) & (df$year == 2020), 1,
                ifelse(lwa, 1,
                       ifelse((df$week >= df$fWeek2 & df$week <= df$fWeek2End) &
                                (df$year == 2021), 1, 0)))
  replace_na(out, 0)
}

for (nm in c("weekCamp", "weekLabel")) {
  df <- ungroup(get(nm, envir = ws))
  old <- treat_rule(df, lwa_2020_only = FALSE)
  stopifnot("old rule must reproduce stored treat" = all(old == df$treat))
  new <- treat_rule(df, lwa_2020_only = TRUE)
  off <- df[old == 1 & new == 0, ]   # bug 1: spurious 2021 LWA weeks
  on  <- df[old == 0 & new == 1, ]   # bug 2: SD's 2021 FPUC-300 weeks
  message(nm, ": ", nrow(off), " state-weeks 1->0 (", n_distinct(off$state),
          " states, 2021 wk ", paste(range(off$week), collapse = "-"), "); ",
          nrow(on), " state-weeks 0->1 (", paste(unique(on$state), collapse = ","),
          ", 2021 wk ", paste(range(on$week), collapse = "-"), ")")
  stopifnot(all(off$year == 2021), all(on$year == 2021), all(is.na(on$lWeekStart)),
            all(on$week >= on$fWeek2 & on$week <= on$fWeek2End))
  df$treat <- new
  assign(nm, df, envir = ws)
  if (write_out) readr::write_csv(df, here::here("Code", "replication", "exported",
                                                  paste0(nm, ".csv")))
}

## Flag any other saved object that carries a `treat` column (not patched here)
others <- setdiff(ls(ws, all.names = TRUE), c("weekCamp", "weekLabel"))
has_treat <- others[vapply(others, function(o) {
  x <- get(o, envir = ws); is.data.frame(x) && "treat" %in% names(x) }, logical(1))]
if (length(has_treat)) message("Other objects with `treat` (rebuilt downstream?): ",
                               paste(has_treat, collapse = ", "))

if (write_out) {
  save(list = ls(ws, all.names = TRUE), envir = ws, file = here::here(".RData"))
  message("Saved patched .RData")
} else message("Dry run: nothing written. Re-run with --write to apply.")
