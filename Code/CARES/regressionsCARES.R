library(data.table) ## For some minor data wrangling
library(fixest)   
library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(janitor)
library(ggfixest)
library(predictrace)
library(here)

#set etable default
set_rules = function(x, heavy, light){
  # x: the character vector returned by etable
  
  tex2add = ""
  if(!missing(heavy)){
    tex2add = paste0("\\setlength\\heavyrulewidth{", heavy, "}\n")
  }
  if(!missing(light)){
    tex2add = paste0(tex2add, "\\setlength\\lightrulewidth{", light, "}\n")
  }
  
  if(nchar(tex2add) > 0){
    x[x == "%start:tab\n"] = tex2add
  }
  
  x
}

# setwd removed — here::here() handles project-relative paths


#diff and diff specification
diff1 = feols(c(log(campaigns),log(donationsTotal),log(amountTotal)) ~ treat + median_income + percent_poverty + cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC  ## Other controls
               | state + year^week,  ## FEs
              cluster = ~state, ## Clustered SEs,
              weights=weekCamp$total_population,
              data = weekCamp)

etable(diff1,tex=TRUE)

#breaking out by category
diff2 = feols(c(log(num_emergency),log(num_medical),log(num_memorial),log(num_financial_emergency),log(num_family),log(num_volunteer),log(num_community),
                log(num_business),log(num_education)) ~ treat + median_income  + cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC ## Other controls
              | state + year^week,  ## FEs
              cluster = ~state, ## Clustered SEs,
              weights=weekLabel$total_population,
              data = weekLabel)

etable(diff2)

#isolating by when fin. emergency campaigns were official
official <- weekLabel %>%
  filter(((week>=40 & year==2020) | (year==2021))) 
diff3 = feols(log(num_financial_emergency)
                 ~ treat + median_income  + cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC  ## Other controls
              | state + year^week,  ## FEs
              cluster = ~state, ## Clustered SEs,
              weights=official$total_population,
              data = official)

etable(diff3)

#modeling if campaign goal amounts were affected by different benefit sizes
m4 <- df %>% mutate(week = lubridate::week(ymd(date(created_at))), year = as.factor(year)) %>%
  left_join(weeks[c('state','stateAbbr','fWeek','pWeek','fWeek1End','lWeekStart','lWeekEnd','fWeek2','fWeek2End')],join_by(state==stateAbbr)) %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  #mutate(state_fips = as.integer(state_fips)) %>%
  left_join(covid,join_by(state==state,year==year,week==week)) %>%
  left_join(claimsW[c('state_abb','year','week','initial_claims','insured_unemployment_rate','continued_claims')],join_by(state==state_abb,year==year,week==week)) %>%
  left_join(puaData,join_by(state==state,year==year,week==refWeek)) %>%
  mutate(fpuc600 = ifelse( (week>=fWeek & week<=fWeek1End) & (year==2020),1,0),
          lwa = ifelse(((week>=lWeekStart & week<=lWeekEnd) | (week>=lWeekStart & lWeekEnd==1)) & (year==2020),1,0),
          fpuc300 = ifelse( (week>=fWeek2 & week<=fWeek2End) & (year==2021),1,0),
         cases = replace_na(cases,0),
         fpuc600 = replace_na(fpuc600,0),
         lwa = replace_na(lwa,0),
         fpuc300 = replace_na(fpuc300,0),
         puaIC = replace_na(puaIC,0),
         peucCC = replace_na(peucCC,0),
         puaCC = replace_na(puaCC,0)) %>%
  group_by(state) %>%
  fill(initial_claims,insured_unemployment_rate,continued_claims) 

m5 <- m4 %>%
  distinct() %>%
  mutate(predict_race(user_last_name), predict_gender(user_first_name))
  
#by different benefit waves
diff4 = feols(log(goal_amount) ~  median_income + cases + insured_unemployment_rate + continued_claims + puaIC + fpuc600 + lwa + fpuc300
  | state + year^week + category,
  weight=m5$total_population,
  cluster=~state,
,data=m5)
etable(diff4)

#by race
diff5 = feols(log(goal_amount) ~  median_income + cases + insured_unemployment_rate + continued_claims + puaIC + fpuc600 + lwa + fpuc300
              | state + year^week + category,
              weight=m5$total_population
              ,data=m5 , split='likely_race',split.drop=c('american_indian'))

etable(diff5)

#by gender
diff6 = feols(goal_amount ~  median_income + cases + insured_unemployment_rate + continued_claims + puaIC + fpuc600 + lwa + fpuc300
              | state + year^week + category,
              weight=m5$total_population
              ,data=m5, split='likely_gender', split.drop=c('female, male'))

etable(diff6)


# ============================================================
# ROBUSTNESS CHECKS
# Addresses three referee concerns:
#   R1. Pre-trends test via event study
#   R2. Staggered DiD bias via Sun-Abraham (2021) estimator
#   R3. Log-of-zero bias via Poisson PPML
# ============================================================

dir.create("paper/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("paper/tables",  recursive = TRUE, showWarnings = FALSE)

# ---- R1. Event Study: Pre-Trends Test ----
#
# DESIGN CHOICE: We event-study only the FPUC-$600 rollout (spring–summer 2020).
# Rationale for a single-event focus:
#   - The FPUC-$600 start date varies across states (our source of variation).
#   - The FPUC-$600 end date is UNIFORM across all states (late July 2020, week
#     ≈ fWeek1End), so it is absorbed by calendar-week fixed effects and does not
#     contaminate the post-period.
#   - LWA and FPUC-$300 are separate events with their own identifying variation;
#     including them would contaminate the post-period for late-starting states.
#
# POST-PERIOD BOUNDARY: We trim the post-period at fWeek1End (each state's last
# week of FPUC-$600), not at an arbitrary +12. This avoids including observations
# where LWA has already begun for some states. The pre-period is capped at -12
# to keep the event window balanced and avoid data sparsity in the distant past.
#
# Resulting window per state:  [ week - fWeek ∈ {-12, ..., fWeek1End - fWeek} ]
# Since fWeek1End is the same for all states, differences in window length
# reflect only when the state started (earlier start → longer post-period).
# The UNIFORM end absorbs into week FEs — identification comes from variation
# in start timing only.

weekCamp_es <- weekCamp %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(
    time2treat    = as.integer(week) - as.integer(fWeek),
    post_boundary = as.integer(fWeek1End) - as.integer(fWeek)   # state-specific max τ
  ) %>%
  filter(time2treat >= -12, time2treat <= post_boundary)

weekLabel_es <- weekLabel %>%
  filter(year == "2020", !is.na(fWeek), !is.na(fWeek1End)) %>%
  mutate(
    time2treat    = as.integer(week) - as.integer(fWeek),
    post_boundary = as.integer(fWeek1End) - as.integer(fWeek)
  ) %>%
  filter(time2treat >= -12, time2treat <= post_boundary)

# Event study: total campaigns
# NOTE: two reference periods are dropped, not one. Every state in this
# sample is eventually treated (no never-treated units), so with state and
# calendar-week fixed effects the full set of relative-time dummies is
# collinear. Dropping only tau = -1 leaves the design near-singular
# (condition number ~9e15) and inflates the event-study standard errors by
# ~6 orders of magnitude. Dropping the most distant lead as well identifies
# the remaining coefficients relative to those two periods.
es_total <- feols(
  log(campaigns) ~ i(time2treat, ref = c(-12, -1)) +
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
  | state + week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp_es
)

# Event study: financial emergency campaigns
# Use log(1 + n) to retain zero-count state-weeks before the category
# was officially launched in October 2020 (week ≈ 40).
# Note: because fWeek1End is in July 2020 (week ~31) and the financial
# emergency category launched in October 2020 (week ~40), all observations
# in this window precede the category launch — so the outcome is effectively
# zero throughout. This event study mainly validates pre-trends for the
# broader set of consumption-related campaigns; interpret the financial
# emergency estimates cautiously given the category did not yet exist.
es_fin_emerg <- feols(
  log(num_financial_emergency + 1) ~ i(time2treat, ref = c(-12, -1)) +
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
  | state + week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel_es
)

# Save event study plots
p_es_total <- ggiplot(
  es_total,
  xlab = "Weeks Since First FPUC Payment",
  ylab = "Estimated Effect on Log # of Campaigns",
  main = "Pre-Trends Test: Total GoFundMe Campaigns"
) + theme_minimal(base_size = 13)
ggsave("paper/figures/event_study_total.pdf", p_es_total,
       width = 12, height = 5, bg = "white")

p_es_fin <- ggiplot(
  es_fin_emerg,
  xlab = "Weeks Since First FPUC Payment",
  ylab = "Estimated Effect on Log(1 + Financial Emergency Campaigns)",
  main = "Pre-Trends Test: Financial Emergency Campaigns"
) + theme_minimal(base_size = 13)
ggsave("paper/figures/event_study_fin_emerg.pdf", p_es_fin,
       width = 12, height = 5, bg = "white")


# ---- R2. Sun-Abraham (2021) Heterogeneity-Robust DiD ----
# Addresses potential TWFE bias from staggered treatment timing.
# cohort = fWeek (first FPUC payment week per state within 2020).
# Later-treated states serve as controls for earlier-treated states
# (not-yet-treated comparison).
#
# Same post-period boundary as R1: trim at fWeek1End so the post-period
# only includes weeks when FPUC-$600 was active, avoiding LWA contamination.

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
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
  | state + period,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp_sa
)

# Export aggregated ATT (compare to baseline TWFE in Table 2, col 1)
etable(
  sunab_total,
  agg        = "ATT",
  title      = "Sun-Abraham (2021) Robustness: Log Total Campaigns",
  style.tex  = style.tex("aer"),
  fitstat    = ~r2 + n,
  file       = "paper/tables/sunab_robustness.tex"
)

p_sunab <- ggiplot(
  sunab_total,
  xlab = "Weeks Since First FPUC Payment",
  ylab = "Heterogeneity-Robust ATT (Log # of Campaigns)",
  main = "Sun-Abraham (2021): Heterogeneity-Robust Event Study"
) + theme_minimal(base_size = 13)
ggsave("paper/figures/sunab_event_study.pdf", p_sunab,
       width = 12, height = 5, bg = "white")


# ---- R3. Poisson PPML Robustness ----
# Counts modeled directly without log transformation.
# Coefficients are semi-elasticities. Naturally handles zero counts
# (no dropped observations, no log(1+y) approximation needed).

pois_overall <- fepois(
  campaigns ~ treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekCamp
)

pois_fin_emerg <- fepois(
  num_financial_emergency ~ treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
  | state + year^week,
  cluster  = ~state,
  weights  = ~total_population,
  data     = weekLabel
)

pois_business <- fepois(
  num_business ~ treat + median_income + percent_poverty +
    cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC
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
  file      = "paper/tables/poisson_robustness.tex"
)


#trim sample for regressions
#binCampF <- weekCamp %>%
#  filter(between(time2treatF,-12,12))

#binCampP <- weekCamp %>%
#  filter(between(time2treatP,-12,12))

#binLabelF <- weekLabel %>%
#  filter(between(time2treatF, -4, 4))

#binLabelP <- weekLabel %>%
#  filter(between(time2treatP, -4, 4))

#should proxy by description including words like rent, utilities, groceries

#all campaign model -> should control for benefit level??
#mod_twfe = feols(log(campaigns) ~ i(time2treatF, treatF, ref = -1) + i(state,week) + ## Our key interaction: time × treatment status
#                   cases/total_population + initial_claims/total_population + insured_unemployment_rate ## Other controls
#                   | state,         ## FEs
#                   cluster = ~state,        ## Clustered SEs
                 #weights = binCampF$total_population,
#                 data = binCampF)

#iplot(mod_twfe, 
 #     xlab = 'Weeks Since First Payment',
#      main = 'Effect of FPUC Payment Timing on Weekly GoFundMe Campaigns',
#      ylab= 'Diff. in Log # of Campaigns')

#etable(mod_twfe)

#looking at PUA program
#mod_twfe2 = feols(log(campaigns) ~ i(time2treatP, treatP, ref = -1) + ## Our key interaction: time × treatment status
#                   cases/total_population + initial_claims/total_population + insured_unemployment_rate  ## Other controls
#                 | state,         ## FEs
#                 cluster = ~state + week,        ## Clustered SEs
                 #weights = binCampF$total_population,
#                 data = binCampP)

#iplot(mod_twfe2, 
#      xlab = 'Weeks Since First Payment',
#      main = 'Effect of PUA Payment Timing on Weekly GoFundMe Campaigns',
#      ylab= 'Diff. in Log # of Campaigns')


#sunab specification - robustness check
#sunab_twfe <- feols(log(campaigns) ~ sunab(treatF,time2treatF,ref.p=-1) + cases/total_population + initial_claims/total_population + insured_unemployment_rate
#                    | state,
#                    cluster=~state,
#                    data=binCampF)
#iplot(sunab_twfe)

