## build_text_proxy.R
## Builds a FULL-PERIOD (2020-2021) text-based "financial-emergency-like"
## outcome and re-runs the main effect on it. This is the most direct test of
## the audit's central concern: the official financial-emergency category did
## not exist until Oct 2020, so it is mechanically ~0 during the FPUC-$600
## identifying window. A description-based proxy exists throughout, letting us
## test whether the headline substitution result survives when the outcome is
## present during the identifying variation.
##
## Proxy definition: a campaign is "financial-emergency-like" if its description
## triggers any job-loss keyword flag already coded in raw_description.csv
## (fired, terminated, let go, lost job, downsized, reduced role, dismissed).
## These flags are available for all campaigns in all periods. (A full-text
## rent/food/bills scan would require loading 2.6M descriptions; the job-loss
## flags are a memory-safe, economically meaningful proxy for hardship-driven
## crowdfunding.)
##
## Run:  Rscript Code/CARES/build_text_proxy.R   (after the .RData panels exist)

suppressPackageStartupMessages({
  library(data.table); library(tidyverse); library(here); library(fixest); library(lubridate)
})

raw_desc_path <- here::here("Data", "raw files", "raw_description.csv")
feed_path     <- here::here("Data", "raw files", "feed_county_ACS.csv")
out_dir       <- here::here("Code", "replication", "exported")
tab_dir       <- here::here("paper", "tables")
fig_dir       <- here::here("paper", "figures")

jobloss <- c("fired","terminated","let go","lost job","downsized","reduced role","dismissed")

## --- read only the columns we need (avoid loading 2.6M full descriptions) ----
cat("Reading raw_description flags...\n")
rd <- fread(raw_desc_path, select = c("campaign_id", jobloss))
rd[, finlike := as.integer(rowSums(.SD, na.rm = TRUE) > 0), .SDcols = jobloss]
cat("  campaigns flagged finlike:", sum(rd$finlike), "of", nrow(rd), "\n")

cat("Reading feed (id, state, created_at)...\n")
feed <- fread(feed_path, select = c("id", "state", "created_at"))
feed <- feed[!is.na(state) & state != ""]
feed[, date := as.Date(created_at)]
feed[, `:=`(yr = year(date), wk = isoweek(date))]
feed <- feed[yr %in% c(2020, 2021)]

## --- join flag to feed by campaign id ---------------------------------------
m <- merge(feed, rd[, .(campaign_id, finlike)],
           by.x = "id", by.y = "campaign_id", all.x = TRUE)
m[is.na(finlike), finlike := 0L]
cat("  joined rows 2020-2021:", nrow(m), " finlike share:", round(mean(m$finlike), 4), "\n")

## --- aggregate to state x year x week ---------------------------------------
agg <- m[, .(num_finlike = sum(finlike)), by = .(state, yr, wk)]
setnames(agg, c("yr", "wk"), c("year", "week"))
agg[, year := as.factor(year)]

## --- merge onto the analysis panel (treat + covariates) from .RData ---------
load(here::here(".RData"))
weekLabel <- ungroup(weekLabel)
pan <- weekLabel %>%
  mutate(week = as.integer(week)) %>%
  left_join(agg %>% mutate(week = as.integer(week)),
            by = c("state", "year", "week")) %>%
  mutate(num_finlike = replace_na(num_finlike, 0L))

readr::write_csv(pan %>% select(state, year, week, num_finlike, treat),
                 file.path(out_dir, "panel_textproxy.csv"))

## ===========================================================================
## Re-run the main TWFE + program-specific decomposition on the proxy outcome
## ===========================================================================
pan <- pan %>%
  mutate(yr = as.integer(as.character(year)),
         fpuc600 = as.integer((week >= fWeek & week <= fWeek1End) & yr == 2020),
         lwa     = as.integer((week >= lWeekStart & week <= lWeekEnd) & !is.na(lWeekStart) & yr == 2020),
         fpuc300 = as.integer((week >= fWeek2 & week <= fWeek2End) & yr == 2021)) %>%
  mutate(across(c(fpuc600, lwa, fpuc300), ~ replace_na(.x, 0L)))

## (i) main composite-treat spec (matches diff2 structure)
tp_main <- feols(log(num_finlike + 1) ~ treat + median_income +
                   cases + initial_claims + insured_unemployment_rate +
                   continued_claims + puaIC | state + year^week,
                 cluster = ~state, weights = ~total_population, data = pan)
## (ii) program-specific decomposition
tp_prog <- feols(log(num_finlike + 1) ~ fpuc600 + lwa + fpuc300 + median_income +
                   cases + initial_claims + insured_unemployment_rate +
                   continued_claims + puaIC | state + year^week,
                 cluster = ~state, weights = ~total_population, data = pan)

etable(list("Composite treat" = tp_main, "By program" = tp_prog),
       keep = c("%treat", "%fpuc600", "%lwa", "%fpuc300"),
       dict = c(treat = "Treat (any supplement)", fpuc600 = "FPUC-$600",
                lwa = "LWA", fpuc300 = "FPUC-$300"),
       title = "Effect on Text-Based Financial-Emergency-Like Campaigns (Full Period)",
       style.tex = style.tex("aer"), fitstat = ~ r2 + n,
       file = file.path(tab_dir, "text_proxy.tex"), replace = TRUE)
cat("Saved: paper/tables/text_proxy.tex\n")

## (iii) event study around FPUC-600 onset, now that the outcome EXISTS in 2020
es_dat <- pan %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(t2t = as.integer(week) - as.integer(fWeek),
         pb  = as.integer(fWeek1End) - as.integer(fWeek)) %>%
  filter(t2t >= -12, t2t <= pb)
es <- feols(log(num_finlike + 1) ~ i(t2t, ref = -1) +
              cases + initial_claims + insured_unemployment_rate +
              continued_claims + puaIC | state + week,
            cluster = ~state, weights = ~total_population, data = es_dat)
tryCatch({
  library(ggfixest)
  p <- ggiplot(es, xlab = "Weeks Since First FPUC-$600 Payment",
               ylab = "Effect on Log(1 + Text-Proxy Campaigns)",
               main = "Pre-Trends Test on Full-Period Text Proxy") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::theme_minimal(base_size = 13)
  ggplot2::ggsave(file.path(fig_dir, "text_proxy_event_study.pdf"), p,
                  width = 12, height = 5, bg = "white")
  cat("Saved: paper/figures/text_proxy_event_study.pdf\n")
}, error = function(e) cat("  text-proxy figure failed:", conditionMessage(e), "\n"))

cat("\ntext-proxy treat coef:", round(coef(tp_main)["treat"], 4),
    " (SE", round(sqrt(diag(vcov(tp_main)))["treat"], 4), ")\n")
cat("text-proxy FPUC-600 coef:", round(coef(tp_prog)["fpuc600"], 4), "\n")
