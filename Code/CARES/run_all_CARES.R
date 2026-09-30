## run_all_CARES.R
## Full CARES pipeline runner: loads saved workspace, runs all regressions
## and robustness checks, saves publication-quality tables and figures.
##
## Run from repo root:
##   Rscript Code/CARES/run_all_CARES.R

suppressPackageStartupMessages({
  library(fixest)
  library(tidyverse)
  library(ggfixest)
  library(janitor)
  library(here)
})

## ── Paths ────────────────────────────────────────────────────────────────────
## Use here::here() so the script works from any cwd as long as it's run
## from inside the GoFundMe-Research project (here finds the .Rproj root).

fig_dir <- here::here("paper", "figures")
tab_dir <- here::here("paper", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

## ── Load saved workspace ─────────────────────────────────────────────────────
cat("[1/7] Loading .RData workspace...\n")
load(here::here(".RData"))
cat("      weekCamp : ", nrow(weekCamp),  "rows x", ncol(weekCamp),  "cols\n")
cat("      weekLabel:", nrow(weekLabel), "rows x", ncol(weekLabel), "cols\n")

## Tidy up grouped_df so fixest doesn't complain
weekCamp  <- ungroup(weekCamp)
weekLabel <- ungroup(weekLabel)

## ── etable style ─────────────────────────────────────────────────────────────
## etable() uses NSE — call it directly rather than wrapping in a helper.
## All calls use: style.tex = style.tex("aer"), fitstat = ~r2 + n

## ── [2/7] MAIN TWFE REGRESSIONS ──────────────────────────────────────────────
cat("[2/7] Running main TWFE specifications...\n")

## diff1 — overall outcomes (campaigns, donations, amounts)
diff1 <- feols(
  c(log(campaigns), log(donationsTotal), log(amountTotal)) ~
    treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp
)

etable(
  diff1,
  headers   = c("Log Campaigns", "Log Donations", "Log Amount"),
  title     = "Effect of Supplemental UI Benefits on GoFundMe Activity",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "main_results.tex")
)
cat("      Saved: paper/tables/main_results.tex\n")

## diff2 — by campaign category
diff2 <- feols(
  c(log(num_emergency + 1), log(num_medical + 1), log(num_memorial + 1),
    log(num_financial_emergency + 1), log(num_family + 1),
    log(num_volunteer + 1), log(num_community + 1),
    log(num_business + 1), log(num_education + 1)) ~
    treat + median_income +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel
)

etable(
  diff2,
  headers   = c("Emergency", "Medical", "Memorial",
                "Fin. Emerg.", "Family",
                "Volunteer", "Community", "Business", "Education"),
  title     = "Effect of Supplemental UI Benefits by Campaign Category",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "category_results.tex")
)
cat("      Saved: paper/tables/category_results.tex\n")

## diff3 — financial emergency, post-launch period only (Oct 2020 onwards)
official <- weekLabel %>%
  filter((week >= 40 & year == "2020") | year == "2021")

diff3 <- feols(
  log(num_financial_emergency + 1) ~
    treat + median_income +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = official
)

etable(
  diff3,
  title     = "Effect on Financial Emergency Campaigns (Post-Launch Period)",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "fin_emerg_official.tex")
)
cat("      Saved: paper/tables/fin_emerg_official.tex\n")


## ── [3/7] EVENT STUDY (R1) ──────────────────────────────────────────────────
cat("[3/7] Running event studies (R1 pre-trends)...\n")

weekCamp_es <- weekCamp %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(
    time2treat    = as.integer(week) - as.integer(fWeek),
    post_boundary = as.integer(fWeek1End) - as.integer(fWeek)
  ) %>%
  filter(time2treat >= -12, time2treat <= post_boundary)

weekLabel_es <- weekLabel %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(
    time2treat    = as.integer(week) - as.integer(fWeek),
    post_boundary = as.integer(fWeek1End) - as.integer(fWeek)
  ) %>%
  filter(time2treat >= -12, time2treat <= post_boundary)

cat("      weekCamp_es:", nrow(weekCamp_es), "rows\n")
cat("      weekLabel_es:", nrow(weekLabel_es), "rows\n")

# NOTE: two reference periods are dropped, not one. Every state in this
# sample is eventually treated (no never-treated units), so with state and
# calendar-week fixed effects the full set of relative-time dummies is
# collinear. Dropping only tau = -1 leaves the design near-singular
# (condition number ~9e15) and inflates the event-study standard errors by
# ~6 orders of magnitude. Dropping the most distant lead as well identifies
# the remaining coefficients relative to those two periods.
es_total <- feols(
  log(campaigns) ~ i(time2treat, ref = c(-12, -1)) +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp_es
)

es_fin_emerg <- feols(
  log(num_financial_emergency + 1) ~ i(time2treat, ref = c(-12, -1)) +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel_es
)

p_es_total <- ggiplot(
  es_total,
  xlab = "Weeks Since First FPUC-$600 Payment",
  ylab = "Estimated Effect on Log Campaigns",
  main = "Pre-Trends Test: Total GoFundMe Campaigns"
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(fig_dir, "event_study_total.pdf"),
  p_es_total, width = 12, height = 5, bg = "white"
)
cat("      Saved: paper/figures/event_study_total.pdf\n")

p_es_fin <- ggiplot(
  es_fin_emerg,
  xlab = "Weeks Since First FPUC-$600 Payment",
  ylab = "Estimated Effect on Log(1 + Financial Emergency Campaigns)",
  main = "Pre-Trends Test: Financial Emergency Campaigns"
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(fig_dir, "event_study_fin_emerg.pdf"),
  p_es_fin, width = 12, height = 5, bg = "white"
)
cat("      Saved: paper/figures/event_study_fin_emerg.pdf\n")


## ── [4/7] SUN-ABRAHAM (R2) ──────────────────────────────────────────────────
cat("[4/7] Running Sun-Abraham heterogeneity-robust DiD (R2)...\n")

weekCamp_sa <- weekCamp %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(
    cohort        = as.integer(fWeek),
    period        = as.integer(week),
    time2treat    = period - cohort,
    post_boundary = as.integer(fWeek1End) - as.integer(fWeek)
  ) %>%
  filter(time2treat >= -12, time2treat <= post_boundary)

sunab_total <- feols(
  log(campaigns) ~ sunab(cohort, period, ref.p = -1) +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + period,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp_sa
)

etable(
  sunab_total,
  agg       = "ATT",
  title     = "Sun-Abraham (2021) Robustness: Log Total Campaigns",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "sunab_robustness.tex")
)
cat("      Saved: paper/tables/sunab_robustness.tex\n")

p_sunab <- ggiplot(
  sunab_total,
  xlab = "Weeks Since First FPUC-$600 Payment",
  ylab = "Heterogeneity-Robust ATT (Log Campaigns)",
  main = "Sun-Abraham (2021): Dynamic Treatment Effects"
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(fig_dir, "sunab_event_study.pdf"),
  p_sunab, width = 12, height = 5, bg = "white"
)
cat("      Saved: paper/figures/sunab_event_study.pdf\n")


## ── [5/7] POISSON PPML (R3) ─────────────────────────────────────────────────
cat("[5/7] Running Poisson PPML robustness (R3)...\n")

pois_overall <- fepois(
  campaigns ~
    treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp
)

pois_fin_emerg <- fepois(
  num_financial_emergency ~
    treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel
)

pois_business <- fepois(
  num_business ~
    treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate +
    continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel
)

etable(
  list(pois_overall, pois_fin_emerg, pois_business),
  headers   = c("Overall", "Fin. Emergency", "Business"),
  title     = "Poisson PPML Robustness: Campaign Counts",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "poisson_robustness.tex")
)
cat("      Saved: paper/tables/poisson_robustness.tex\n")


## ── [5.4/7] BAD-CONTROL SENSITIVITY (2026-05-14) ────────────────────────────
## The main diff1/diff2 specs control for initial_claims, continued_claims,
## insured_unemployment_rate, and puaIC. These are post-treatment mediators
## of FPUC: when FPUC becomes available, claims rise (people file for the
## supplement); conditioning on those claims absorbs the very channel
## through which FPUC affects crowdfunding. The "clean" specs below drop
## those controls and keep only pre-determined state characteristics
## (median_income, percent_poverty) so the treatment coefficient captures
## the full reduced-form effect of FPUC availability.
cat("[5.4/7] Running bad-control-fixed specifications...\n")

diff1_clean <- tryCatch(
  feols(
    c(log(campaigns), log(donationsTotal), log(amountTotal)) ~
      treat + median_income + percent_poverty
    | state + year^week,
    cluster  = ~state,
    weights  = ~total_population,
    data     = weekCamp
  ),
  error = function(e) { cat("      diff1_clean failed:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(diff1_clean)) {
  etable(
    diff1_clean,
    headers   = c("Log Campaigns", "Log Donations", "Log Amount"),
    title     = "Main Specifications without Post-Treatment Controls (Bad-Control Fix)",
    style.tex = style.tex("aer"),
    fitstat   = ~r2 + n,
    file      = file.path(tab_dir, "main_results_clean.tex")
  )
  cat("      Saved: paper/tables/main_results_clean.tex\n")
}

diff2_clean <- tryCatch(
  feols(
    c(log(num_emergency + 1), log(num_medical + 1), log(num_memorial + 1),
      log(num_financial_emergency + 1), log(num_family + 1),
      log(num_volunteer + 1), log(num_community + 1),
      log(num_business + 1), log(num_education + 1)) ~
      treat + median_income + percent_poverty
    | state + year^week,
    cluster  = ~state,
    weights  = ~total_population,
    data     = weekLabel
  ),
  error = function(e) { cat("      diff2_clean failed:", conditionMessage(e), "\n"); NULL }
)
if (!is.null(diff2_clean)) {
  etable(
    diff2_clean,
    headers   = c("Emergency", "Medical", "Memorial",
                  "Fin. Emerg.", "Family",
                  "Volunteer", "Community", "Business", "Education"),
    title     = "Category Specifications without Post-Treatment Controls (Bad-Control Fix)",
    style.tex = style.tex("aer"),
    fitstat   = ~r2 + n,
    file      = file.path(tab_dir, "category_results_clean.tex")
  )
  cat("      Saved: paper/tables/category_results_clean.tex\n")
}


## ── [5.5/7] ADDITIONAL ROBUSTNESS (2026-05-14 from econometric review) ──────
## Adds: joint F-test of leads, donut (drop +/-2wk), placebo treatment,
## replacement-rate heterogeneity, wild cluster bootstrap, Honest DiD bounds.
## All wrapped in tryCatch so a single failure doesn't kill the run.
cat("[5.5/7] Additional robustness checks (econometric review)...\n")

# --- Source business-camps regression so biz1 / wordFlag heterogeneity ---
#     is available for inline_stats.R's effBizNoWagePct macro.
#     Skipped silently if the raw description data isn't available.
if (!exists("biz1") && file.exists(here::here("Data", "raw files", "raw_description.csv"))) {
  cat("      Sourcing businessCampsText.R to build biz1...\n")
  tryCatch(
    source(here::here("Code", "CARES", "businessCampsText.R")),
    error = function(e) cat("      businessCampsText.R failed:", conditionMessage(e), "\n")
  )
}

# --- Joint F-test of leads on event-study models -------------------------
# Under parallel trends, all pre-period coefficients jointly = 0.
es_total_lead_test <- tryCatch(
  fixest::wald(es_total, "time2treat::-[0-9]+$"),
  error = function(e) { cat("      wald(es_total) failed:", conditionMessage(e), "\n"); list(p = NA_real_) }
)
# For fin emerg, the pre-period outcome is identically zero (category
# launched Oct 2020), so SOME lead coefs can be NA/singular. Detect that
# and skip cleanly rather than letting Wald blow up.
es_finemerg_lead_test <- tryCatch({
  fe_coefs <- coef(es_fin_emerg)
  fe_pre_terms <- grep("^time2treat::-[0-9]+$", names(fe_coefs), value = TRUE)
  fe_pre_vals  <- fe_coefs[fe_pre_terms]
  if (length(fe_pre_terms) == 0 ||
      any(is.na(fe_pre_vals)) || any(!is.finite(fe_pre_vals))) {
    cat("      es_fin_emerg pre-period has NA/Inf coefs (category did not exist pre-Oct 2020); skipping Wald.\n")
    list(p = NA_real_)
  } else {
    fixest::wald(es_fin_emerg, "time2treat::-[0-9]+$")
  }
}, error = function(e) { cat("      wald(es_fin_emerg) failed:", conditionMessage(e), "\n"); list(p = NA_real_) })
cat("      Joint F-test of leads (total):    p =",
    round(es_total_lead_test$p, 4), "\n")
cat("      Joint F-test of leads (fin emrg): p =",
    round(es_finemerg_lead_test$p, 4), "\n")

# --- Donut analysis: drop +/- 2 weeks around treatment -------------------
weekCamp_donut <- weekCamp %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(time2treat = as.integer(week) - as.integer(fWeek)) %>%
  filter(abs(time2treat) > 2)

diff1_donut <- tryCatch(
  feols(
    log(campaigns) ~ treat + median_income + percent_poverty +
      cases + initial_claims + insured_unemployment_rate +
      continued_claims + puaIC
    | state + year^week,
    cluster  = ~state,
    weights  = ~total_population,
    data     = weekCamp_donut
  ),
  error = function(e) { cat("      diff1_donut failed:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(diff1_donut)) {
  etable(
    list(diff1[[1]], diff1_donut),
    headers   = c("Baseline", "Donut +/- 2wk"),
    title     = "Donut Robustness: Log Total Campaigns",
    style.tex = style.tex("aer"),
    fitstat   = ~r2 + n,
    file      = file.path(tab_dir, "donut_robustness.tex")
  )
  cat("      Saved: paper/tables/donut_robustness.tex\n")
}

# --- Placebo treatment date (shift each state's fWeek 12 weeks earlier) --
weekCamp_placebo <- weekCamp %>%
  filter(year == "2020", !is.na(fWeek)) %>%
  mutate(
    fWeek_placebo = pmax(as.integer(fWeek) - 12L, 1L),
    treat_placebo = ifelse(
      week >= fWeek_placebo & week <= fWeek_placebo + 8L, 1L, 0L
    )
  )

placebo <- tryCatch(
  feols(
    log(campaigns) ~ treat_placebo + median_income + percent_poverty +
      cases + initial_claims + insured_unemployment_rate +
      continued_claims + puaIC
    | state + week,
    cluster  = ~state,
    weights  = ~total_population,
    data     = weekCamp_placebo
  ),
  error = function(e) { cat("      placebo failed:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(placebo)) {
  etable(
    list(diff1[[1]], placebo),
    headers   = c("Actual FPUC", "Placebo (-12wk)"),
    title     = "Placebo Treatment: Shifting FPUC 12 Weeks Earlier",
    style.tex = style.tex("aer"),
    fitstat   = ~r2 + n,
    file      = file.path(tab_dir, "placebo_treatment.tex")
  )
  cat("      Saved: paper/tables/placebo_treatment.tex\n")
}

# --- Replacement-rate heterogeneity --------------------------------------
repRates_state <- tryCatch(
  readr::read_csv(here::here("Data", "UI_Replacement_Rates_2010_2024.csv"),
                  show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    group_by(state) %>%
    summarize(avgRatio1 = mean(replacement_ratio_1, na.rm = TRUE),
              .groups = "drop"),
  error = function(e) { cat("      repRates load failed:", conditionMessage(e), "\n"); NULL }
)

if (!is.null(repRates_state)) {
  weekLabel_het <- weekLabel %>%
    left_join(repRates_state, by = "state") %>%
    mutate(treat_x_rep = treat * avgRatio1)

  het_finemerg <- tryCatch(
    feols(
      log(num_financial_emergency + 1) ~ treat + treat_x_rep + avgRatio1 +
        median_income + cases + initial_claims + insured_unemployment_rate +
        continued_claims + puaIC
      | state + year^week,
      cluster  = ~state,
      weights  = ~total_population,
      data     = weekLabel_het
    ),
    error = function(e) { cat("      het_finemerg failed:", conditionMessage(e), "\n"); NULL }
  )

  het_business <- tryCatch(
    feols(
      log(num_business + 1) ~ treat + treat_x_rep + avgRatio1 +
        median_income + cases + initial_claims + insured_unemployment_rate +
        continued_claims + puaIC
      | state + year^week,
      cluster  = ~state,
      weights  = ~total_population,
      data     = weekLabel_het
    ),
    error = function(e) { cat("      het_business failed:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(het_finemerg) && !is.null(het_business)) {
    etable(
      list(het_finemerg, het_business),
      headers   = c("Fin. Emergency", "Business"),
      title     = "Heterogeneity by State UI Replacement Rate",
      style.tex = style.tex("aer"),
      fitstat   = ~r2 + n,
      file      = file.path(tab_dir, "het_replacement_rate.tex")
    )
    cat("      Saved: paper/tables/het_replacement_rate.tex\n")
  }
}

# --- Wild cluster bootstrap inference -----------------------------------
# fwildclusterboot requires R >= 4.4.3 and is not available on every machine
# (e.g. the R 4.4.2 build used for this project). To keep this reproducible
# without that dependency, the wild cluster bootstrap is implemented manually:
# restricted (null-imposed) Rademacher WCR clustered on state (Cameron,
# Gelbach & Miller 2008). boot_results keeps the same $...$p_val structure
# inline_stats.R expects, so downstream macros are unaffected.
set.seed(20260514)
B_wcr <- 2999
.wcr_p <- function(data, dv_expr, controls, B = B_wcr) {
  d <- data
  d$.y <- with(d, eval(parse(text = dv_expr)))
  full_f <- as.formula(paste0(".y ~ treat + ", paste(controls, collapse = " + "),
                              " | state + year^week"))
  rest_f <- as.formula(paste0(".y ~ ", paste(controls, collapse = " + "),
                              " | state + year^week"))
  m_full <- feols(full_f, data = d, weights = ~total_population, cluster = ~state)
  t_obs  <- coef(m_full)["treat"] / sqrt(diag(vcov(m_full)))["treat"]
  m_rest <- feols(rest_f, data = d, weights = ~total_population)
  fit0 <- predict(m_rest); res0 <- resid(m_rest)
  cl <- as.character(d$state); g <- unique(cl)
  tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- sample(c(-1, 1), length(g), replace = TRUE); names(w) <- g
    d$.y <- fit0 + w[cl] * res0
    mb <- feols(full_f, data = d, weights = ~total_population, cluster = ~state)
    tstar[b] <- coef(mb)["treat"] / sqrt(diag(vcov(mb)))["treat"]
  }
  list(p_val = unname((1 + sum(abs(tstar) >= abs(t_obs))) / (B + 1)))
}

C1_wcr <- c("median_income","percent_poverty","cases","initial_claims",
            "insured_unemployment_rate","continued_claims","puaIC")
C2_wcr <- c("median_income","cases","initial_claims",
            "insured_unemployment_rate","continued_claims","puaIC")

boot_results <- tryCatch(
  list(
    diff1_main = .wcr_p(weekCamp,  "log(campaigns)",                   C1_wcr),
    diff2_fe   = .wcr_p(weekLabel, "log(num_financial_emergency + 1)", C2_wcr),
    diff2_biz  = .wcr_p(weekLabel, "log(num_business + 1)",            C2_wcr),
    diff3      = .wcr_p(official,  "log(num_financial_emergency + 1)", C2_wcr)
  ),
  error = function(e) { cat("      manual WCR failed:", conditionMessage(e), "\n"); NULL })

if (!is.null(boot_results)) {
  boot_table <- data.frame(
    Specification = c("Log Campaigns (diff1)", "Fin. Emergency (diff2)",
                      "Business (diff2)", "Fin. Emerg. Post-Launch (diff3)"),
    `Cluster p-value`        = c(fixest::pvalue(diff1[[1]])["treat"],
                                  fixest::pvalue(diff2[[4]])["treat"],
                                  fixest::pvalue(diff2[[8]])["treat"],
                                  fixest::pvalue(diff3)["treat"]),
    `Wild bootstrap p-value` = c(boot_results$diff1_main$p_val,
                                  boot_results$diff2_fe$p_val,
                                  boot_results$diff2_biz$p_val,
                                  boot_results$diff3$p_val),
    check.names = FALSE
  )
  fmt4 <- function(x) formatC(x, format = "f", digits = 4)
  wb <- c(
    "% Generated by Code/CARES/run_all_CARES.R (manual WCR wild cluster bootstrap)",
    "\\begingroup", "\\centering", "\\begin{tabular}{lcc}", "\\toprule",
    "Specification & Cluster $p$-value & Wild bootstrap $p$-value \\\\", "\\midrule",
    paste0(boot_table$Specification, " & ", fmt4(boot_table[[2]]), " & ",
           fmt4(boot_table[[3]]), " \\\\"),
    "\\bottomrule",
    paste0("\\multicolumn{3}{l}{\\footnotesize{Restricted (null-imposed) Rademacher ",
           "wild cluster bootstrap, ", B_wcr, " draws, clustered on state.}}"),
    "\\end{tabular}", "\\par\\endgroup")
  writeLines(wb, file.path(tab_dir, "wild_bootstrap.tex"))
  cat("      Saved: paper/tables/wild_bootstrap.tex\n")
}

# --- Honest DiD bounds on event-study ATT --------------------------------
# Use the simple event-study (es_total) rather than sunab_total. fixest's
# sunab() returns per-cohort coefficients that aren't in the format
# HonestDiD's createSensitivityResults_relativeMagnitudes() expects
# (it expects one coefficient per event-time, sorted pre then post).
if (requireNamespace("HonestDiD", quietly = TRUE)) {
  honest_results <- tryCatch({
    es_terms <- grep("^time2treat::", names(coef(es_total)), value = TRUE)
    es_lags  <- as.integer(gsub("^time2treat::", "", es_terms))
    # Sort ascending so pre-period coefs (negative lags) come first
    ord          <- order(es_lags)
    es_terms_ord <- es_terms[ord]
    es_lags_ord  <- es_lags[ord]
    betahat <- unname(coef(es_total)[es_terms_ord])
    sigma   <- unname(vcov(es_total)[es_terms_ord, es_terms_ord, drop = FALSE])
    # vcov() can have tiny floating-point asymmetries from cluster-robust
    # computation; explicitly symmetrize so HonestDiD accepts it.
    sigma   <- (sigma + t(sigma)) / 2
    if (any(is.na(betahat))) stop("event-study coefficients contain NA")
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat        = betahat,
      sigma          = sigma,
      numPrePeriods  = sum(es_lags_ord <  0),
      numPostPeriods = sum(es_lags_ord >= 0),
      Mbarvec        = c(0.5, 1, 1.5, 2)
    )
  }, error = function(e) { cat("      HonestDiD failed:", conditionMessage(e), "\n"); NULL })

  if (!is.null(honest_results)) {
    honest_table <- data.frame(
      Mbar  = honest_results$Mbar,
      Lower = honest_results$lb,
      Upper = honest_results$ub
    )
    xt <- xtable::xtable(
      honest_table,
      caption = "Honest DiD (Rambachan-Roth 2023) Bounds on the Total-Campaign ATT",
      label   = "tab:honest_did",
      digits  = c(0, 1, 4, 4)
    )
    ## floating = FALSE: the paper wraps this in its own table float with the
    ## caption and label, and a nested table environment is a LaTeX error.
    print(xt,
          file              = file.path(tab_dir, "honest_did.tex"),
          include.rownames  = FALSE,
          floating          = FALSE)
    cat("      Saved: paper/tables/honest_did.tex\n")
  }
}

cat("[5.5/7] Done.\n")


## ── [6/7] PRINT KEY COEFFICIENT SUMMARIES ────────────────────────────────────
cat("\n[6/7] Key results summary:\n")
cat("─────────────────────────────────────────────────────────────────\n")

cat("\n── MAIN TWFE (diff1) — treat coefficient ──\n")
print(etable(diff1, keep = "treat"))

cat("\n── CATEGORY (diff2) — treat coefficient ──\n")
print(etable(diff2, keep = "treat"))

cat("\n── FIN. EMERGENCY POST-LAUNCH (diff3) — treat coefficient ──\n")
print(etable(diff3, keep = "treat"))

cat("\n── SUN-ABRAHAM ATT ──\n")
print(etable(sunab_total, agg = "ATT", keep = "ATT"))

cat("\n── POISSON PPML — treat coefficient ──\n")
print(etable(list(pois_overall, pois_fin_emerg, pois_business),
             headers = c("Overall", "Fin.Emerg.", "Business"),
             keep = "treat"))

cat("─────────────────────────────────────────────────────────────────\n")


## ── [7/7] SAVE COMBINED ROBUSTNESS TABLE ─────────────────────────────────────
cat("[7/7] Saving combined robustness comparison...\n")

## Side-by-side: TWFE col 1 (overall) vs Sun-Abraham ATT vs Poisson PPML
etable(
  list(diff1[[1]], pois_overall),
  headers   = c("TWFE (log)", "Poisson PPML"),
  keep      = "treat",
  title     = "Robustness Comparison: Overall Campaigns",
  style.tex = style.tex("aer"),
  fitstat   = ~r2 + n,
  file      = file.path(tab_dir, "robustness_comparison.tex")
)
cat("      Saved: paper/tables/robustness_comparison.tex\n")

cat("\nDone. All outputs written to paper/tables/ and paper/figures/\n")
