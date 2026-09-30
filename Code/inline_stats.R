# inline_stats.R — Emit LaTeX \newcommand macros for prose numbers in the paper
#
# Run AFTER regressionsCARES.R has fit diff1, diff2, diff3, biz1, etc.
# Output: paper/inline_stats.tex with one \newcommand per macro.
# The paper does \input{inline_stats.tex} in its preamble, then uses
# macros like \effFinEmergPct in the body.
#
# This is the reproducibility backbone: re-running the pipeline auto-updates
# every prose number in the manuscript — eliminates copy-paste errors and
# version drift like the existing 11% vs 12% inconsistency between the
# intro (line 66) and the results section (line 142) of GFM UI Paper.tex.

library(here)
library(fixest)

# ---- Helpers --------------------------------------------------------------

# Format a coefficient (which is a log-percent change) as a positive
# percent value with consistent precision. Sign info is embedded in the
# surrounding prose ("decrease of X%"), so we always emit the magnitude.
format_pct <- function(coef, digits = 1) {
  formatC(abs(as.numeric(coef)) * 100, format = "f", digits = digits)
}

format_num <- function(x, digits = 0, big.mark = ",") {
  formatC(as.numeric(x), format = "f", digits = digits, big.mark = big.mark)
}

format_dollar <- function(x, digits = 0) {
  paste0("\\$", format_num(x, digits = digits))
}

make_macro <- function(name, value) {
  sprintf("\\newcommand{\\%s}{%s}", name, value)
}

# Safely extract a named coefficient from a fixest model or a list of models
# (multi-outcome fits return a list). Returns NA_real_ if not found, so the
# script keeps running even if a model isn't loaded.
get_coef <- function(model, term = "treat") {
  if (is.null(model)) return(NA_real_)
  if (inherits(model, "fixest_multi")) {
    # Multi-outcome: caller should index into [[i]] before passing in
    stop("get_coef: pass a single model, not a fixest_multi list. ",
         "Use model[[i]] to pick an outcome.")
  }
  tryCatch(unname(coef(model)[term]), error = function(e) NA_real_)
}

# ---- Regression coefficients ----------------------------------------------
# These pull from fitted models in the parent environment. Run after
# regressionsCARES.R so the objects exist.

# diff2 is a multi-outcome fit by category in regressionsCARES.R, ordered:
#   1: num_emergency, 2: num_medical, 3: num_memorial, 4: num_financial_emergency,
#   5: num_family, 6: num_volunteer, 7: num_community, 8: num_business, 9: num_education
fin_emerg_coef <- if (exists("diff2")) get_coef(diff2[[4]]) else NA_real_
business_coef  <- if (exists("diff2")) get_coef(diff2[[8]]) else NA_real_

# diff1 is a multi-outcome fit on overall outcomes:
#   1: log(campaigns), 2: log(donationsTotal), 3: log(amountTotal)
log_amount_coef <- if (exists("diff1")) get_coef(diff1[[3]]) else NA_real_

# biz1 is split by wordFlag (0 = no wage/employee mention, 1 = mentions them).
# Index [[1]] is wordFlag==0; the paper claims a ~11% decrease there.
biz_no_wage_coef <- if (exists("biz1")) get_coef(biz1[[1]]) else NA_real_

# diff3 is fin emerg post-launch (Oct 2020+).
fin_emerg_official_coef <- if (exists("diff3")) get_coef(diff3) else NA_real_

# Bad-control-fixed specs (drop initial_claims/IUR/continued_claims/puaIC
# controls which are post-treatment mediators of FPUC).
fin_emerg_coef_clean <- if (exists("diff2_clean")) get_coef(diff2_clean[[4]]) else NA_real_
business_coef_clean  <- if (exists("diff2_clean")) get_coef(diff2_clean[[8]]) else NA_real_
log_amount_coef_clean <- if (exists("diff1_clean")) get_coef(diff1_clean[[3]]) else NA_real_
campaigns_coef_clean  <- if (exists("diff1_clean")) get_coef(diff1_clean[[1]]) else NA_real_

# Poisson PPML coefficients (semi-elasticities); slide Robustness 2 frame.
poisson_finemerg_coef <- if (exists("pois_fin_emerg")) get_coef(pois_fin_emerg) else NA_real_
poisson_business_coef <- if (exists("pois_business")) get_coef(pois_business) else NA_real_

# Sun-Abraham aggregated ATT (single number); slide Robustness 3 frame.
sunab_att <- if (exists("sunab_total")) {
  tryCatch({
    s <- summary(sunab_total, agg = "ATT")
    if (!is.null(s$coeftable)) unname(s$coeftable[, "Estimate"])[1] else NA_real_
  }, error = function(e) NA_real_)
} else NA_real_

# ---- Back-of-envelope numbers (line 144) ----------------------------------
# These are derivable from the data; paper currently states them as fixed
# numbers. Make them computable when the upstream data exists.

n_treated_weeks <- if (exists("weekLabel")) {
  nrow(dplyr::filter(weekLabel,
                     treat == 1 &
                     ((week >= 40 & year == "2020") | year == "2021")))
} else NA_real_

avg_finemerg_per_week <- if (exists("weekLabel")) {
  mean(dplyr::filter(weekLabel,
                     (week >= 40 & year == "2020") | year == "2021")$num_financial_emergency,
       na.rm = TRUE)
} else NA_real_

# Reduction in campaigns per state-week = |coef| * avg
camp_reduction_per_week <- if (!is.na(fin_emerg_coef) && !is.na(avg_finemerg_per_week)) {
  abs(fin_emerg_coef) * avg_finemerg_per_week
} else NA_real_

total_camp_reduction <- if (!is.na(camp_reduction_per_week) && !is.na(n_treated_weeks)) {
  round(camp_reduction_per_week * n_treated_weeks)
} else NA_real_

# Average raised per fin emerg campaign in window (line 144 says ~$6,215)
avg_raised_finemerg <- if (exists("df")) {
  tmp <- dplyr::filter(df,
                       category == "financial emergency" &
                       ((lubridate::month(lubridate::ymd(lubridate::date(created_at))) >= 10 &
                         year == 2020) | year == 2021))
  if (nrow(tmp) > 0) mean(tmp$current_amount, na.rm = TRUE) else NA_real_
} else NA_real_

total_dollar_reduction <- if (!is.na(avg_raised_finemerg) && !is.na(total_camp_reduction)) {
  total_camp_reduction * avg_raised_finemerg
} else NA_real_

# ---- Emit macros ----------------------------------------------------------

# ---- Robustness statistics (2026-05-14 from econometric review) -----------

# Pre-trends joint F-test p-values (from wald() calls in run_all_CARES.R)
pretrend_p_total    <- if (exists("es_total_lead_test"))    es_total_lead_test$p    else NA_real_
pretrend_p_finemerg <- if (exists("es_finemerg_lead_test")) es_finemerg_lead_test$p else NA_real_

# Placebo treatment coefficient (should be near zero)
placebo_coef <- if (exists("placebo") && !is.null(placebo)) {
  tryCatch(unname(coef(placebo)["treat_placebo"]), error = function(e) NA_real_)
} else NA_real_
placebo_se <- if (exists("placebo") && !is.null(placebo)) {
  tryCatch(unname(sqrt(diag(vcov(placebo)))["treat_placebo"]), error = function(e) NA_real_)
} else NA_real_

# Honest DiD bounds at Mbar = 1 (a 1× the max pre-period violation)
honest_lower_m1 <- if (exists("honest_results") && !is.null(honest_results)) {
  idx <- which(honest_results$Mbar == 1)
  if (length(idx) > 0) honest_results$lb[idx] else NA_real_
} else NA_real_
honest_upper_m1 <- if (exists("honest_results") && !is.null(honest_results)) {
  idx <- which(honest_results$Mbar == 1)
  if (length(idx) > 0) honest_results$ub[idx] else NA_real_
} else NA_real_

# Wild cluster bootstrap p-values
wild_p_finemerg <- if (exists("boot_results") && !is.null(boot_results)) {
  tryCatch(boot_results$diff2_fe$p_val, error = function(e) NA_real_)
} else NA_real_
wild_p_business <- if (exists("boot_results") && !is.null(boot_results)) {
  tryCatch(boot_results$diff2_biz$p_val, error = function(e) NA_real_)
} else NA_real_

# Replacement-rate interaction coefficient (slope of effect by rep rate)
het_rep_coef <- if (exists("het_finemerg") && !is.null(het_finemerg)) {
  tryCatch(unname(coef(het_finemerg)["treat_x_rep"]), error = function(e) NA_real_)
} else NA_real_

format_p <- function(p, digits = 3) {
  if (is.na(p)) return("??")
  if (p < 0.001) return("$<$0.001")
  formatC(p, format = "f", digits = digits)
}

lines <- c(
  "% Auto-generated by Code/inline_stats.R. Do not edit by hand —",
  "% re-run via run_all.R to regenerate after any regression change.",
  "",
  "% --- Regression coefficients (semi-elasticities × 100) ---",
  make_macro("effFinEmergPct",       if (is.na(fin_emerg_coef))      "??" else format_pct(fin_emerg_coef)),
  make_macro("effBusinessPct",       if (is.na(business_coef))       "??" else format_pct(business_coef)),
  make_macro("effLogAmountPct",      if (is.na(log_amount_coef))     "??" else format_pct(log_amount_coef)),
  make_macro("effBizNoWagePct",      if (is.na(biz_no_wage_coef))    "??" else format_pct(biz_no_wage_coef)),
  make_macro("effFinEmergPostPct",   if (is.na(fin_emerg_official_coef)) "??" else format_pct(fin_emerg_official_coef)),
  "",
  "% --- Back-of-envelope aggregates ---",
  make_macro("nTreatedWeeks",        if (is.na(n_treated_weeks))      "??" else format_num(n_treated_weeks)),
  make_macro("avgFinEmergPerWeek",   if (is.na(avg_finemerg_per_week)) "??" else format_num(avg_finemerg_per_week, digits = 2)),
  make_macro("campReductionPerWeek", if (is.na(camp_reduction_per_week)) "??" else format_num(camp_reduction_per_week, digits = 3)),
  make_macro("totalCampReduction",   if (is.na(total_camp_reduction)) "??" else format_num(total_camp_reduction)),
  make_macro("avgRaisedFinEmerg",    if (is.na(avg_raised_finemerg))  "\\$??" else format_dollar(avg_raised_finemerg)),
  make_macro("totalDollarReduction", if (is.na(total_dollar_reduction)) "\\$??" else format_dollar(total_dollar_reduction)),
  "",
  "% --- Robustness statistics (from econometric review additions) ---",
  make_macro("preTrendPvalueTotal",    format_p(pretrend_p_total)),
  make_macro("preTrendPvalueFinEmerg", format_p(pretrend_p_finemerg)),
  make_macro("placeboCoef",            if (is.na(placebo_coef))      "??" else format_pct(placebo_coef)),
  make_macro("placeboSE",              if (is.na(placebo_se))        "??" else format_pct(placebo_se)),
  make_macro("honestDidLowerMone",     if (is.na(honest_lower_m1))   "??" else formatC(honest_lower_m1, format = "f", digits = 4)),
  make_macro("honestDidUpperMone",     if (is.na(honest_upper_m1))   "??" else formatC(honest_upper_m1, format = "f", digits = 4)),
  make_macro("wildPFinEmerg",          format_p(wild_p_finemerg)),
  make_macro("wildPBusiness",          format_p(wild_p_business)),
  make_macro("hetRepCoef",             if (is.na(het_rep_coef))      "??" else formatC(het_rep_coef, format = "f", digits = 3)),
  "",
  "% --- Slide-only macros (Poisson + Sun-Abraham) ---",
  make_macro("poissonFinEmergPct",     if (is.na(poisson_finemerg_coef)) "??" else format_pct(poisson_finemerg_coef)),
  make_macro("poissonBusinessPct",     if (is.na(poisson_business_coef)) "??" else format_pct(poisson_business_coef)),
  make_macro("sunabATT",               if (is.na(sunab_att))         "??" else formatC(sunab_att, format = "f", digits = 3)),
  "",
  "% --- Bad-control-fix coefficients (no claims-side mediator controls) ---",
  make_macro("effCampaignsPctClean",   if (is.na(campaigns_coef_clean))  "??" else format_pct(campaigns_coef_clean)),
  make_macro("effLogAmountPctClean",   if (is.na(log_amount_coef_clean)) "??" else format_pct(log_amount_coef_clean)),
  make_macro("effFinEmergPctClean",    if (is.na(fin_emerg_coef_clean))  "??" else format_pct(fin_emerg_coef_clean)),
  make_macro("effBusinessPctClean",    if (is.na(business_coef_clean))   "??" else format_pct(business_coef_clean))
)

out_path <- here::here("paper", "inline_stats.tex")
writeLines(lines, out_path)
cat("Wrote", sum(!grepl("^(%|$)", lines)), "macros to", out_path, "\n")
