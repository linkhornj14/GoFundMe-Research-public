# Economic Shocks and Consumption Smoothing Using Online Crowdfunding

**Jesse Linkhorn**, University of Kentucky

This repository contains the paper (`paper/GFM UI Paper.pdf`) and the full analysis code for a study of how households use online crowdfunding to smooth consumption. It asks whether federal expansions of unemployment insurance (UI) during COVID-19 reduced reliance on GoFundMe. Identification comes from the staggered, state-level rollout of the CARES Act supplements. The code covers data construction, estimation, robustness checks, and the tables and figures that appear in the paper.

## Data availability

**The data are not included in this repository.** The campaign-level data were collected from GoFundMe and contain personal information on campaign organizers. They cannot be redistributed. The public data used in the analysis are listed below. They are also excluded, because the scripts expect a specific local folder layout.

| Source | Content | Level |
|---|---|---|
| GoFundMe (collected by the author) | Campaign creation date, category, goal, amount raised, donations, description text | Campaign |
| U.S. Census Bureau, ACS 5-year (via `tidycensus`) | Median income, population, poverty, education, insurance coverage | State, county |
| BLS Local Area Unemployment Statistics | Unemployment rate, labor force, employment-to-population | State, county × month |
| U.S. DOL, ETA-539 | Weekly initial claims, continued claims, insured unemployment rate | State × week |
| U.S. DOL, pandemic program claims | PUA initial and continued claims, PEUC continued claims | State × week |
| U.S. DOL | Monthly UI claims and financial data; UI replacement rates | State × month / year |
| COVID Tracking Project API | Current COVID-19 hospitalizations | State × day |
| USDA ERS | Rural-Urban Continuum Codes (2023) | County |
| Author-compiled | State FPUC, PUA, LWA, and FPUC-$300 start and end dates | State |

As a result, the scripts will not run from a fresh clone. They document every step that produced the reported estimates.

## Research design

The unit of observation for the main analysis is the **state-week** (`weekCamp` for totals, `weekLabel` for counts by campaign category), covering 2020–2021.

**Treatment.** `treat` equals 1 in weeks when a state was paying a federal UI supplement. The indicator switches on and off across three programs:

1. FPUC-$600, from each state's first disbursement week through the end of July 2020.
2. Lost Wages Assistance (LWA), across each state's LWA payment window in fall 2020.
3. FPUC-$300, from January 2021 through each state's end date. Several states terminated the program early in summer 2021.

The staggered FPUC-$600 onset, the LWA onset, and the early FPUC-$300 terminations provide the identifying variation.

**Baseline specification.** Two-way fixed effects with state and year-by-week effects, population weights, and standard errors clustered by state:

```r
feols(log(Y) ~ treat + median_income + percent_poverty + cases +
        initial_claims + insured_unemployment_rate + continued_claims + puaIC
      | state + year^week,
      cluster = ~state, weights = ~total_population, data = weekCamp)
```

Outcomes are campaign counts, donation counts, and amounts raised. Category-level models use the `weekLabel` panel. The main category of interest is **financial emergency**.

**Robustness.** The headline financial-emergency category was introduced by GoFundMe in October 2020. Therefore, it does not exist during the FPUC-$600 onset window. Several robustness checks address this problem, and others address the non-absorbing treatment:

- Event studies and Sun & Abraham (2021) interaction-weighted estimates
- Poisson (PPML) models for count outcomes
- Callaway & Sant'Anna (2021) DiD on the 2021 FPUC-$300 early terminations, with an IPW-matched 2×2 cross-check and randomization inference
- de Chaisemartin & D'Haultfœuille episode-by-episode estimates for the switching treatment (Stata)
- Program-specific treatment indicators (FPUC-$600, LWA, FPUC-$300)
- A text-based outcome built from campaign descriptions that exists over the full sample period
- Honest DiD sensitivity bounds (Rambachan & Roth, 2023), a wild cluster bootstrap, donut and placebo treatment tests, and heterogeneity by UI replacement rate

## Repository structure

```
├── run_all.R                   Master script; runs the pipeline in order
├── setup.R                     One-time package installation
├── GoFundMe-Research.Rproj     Project root for here::here()
├── Code/
│   ├── functions.R             Shared helpers
│   ├── cleanData.R             Exploratory state/county × quarter panels
│   ├── regressions.R           Exploratory TWFE, Poisson, heterogeneity models
│   ├── figures.R, tables.R     Descriptive figures and summary statistics
│   ├── text_mining.R           Random forest classifier on campaign descriptions
│   ├── predictDemo.R           Organizer race/gender imputation from names
│   ├── fin emergency regressions.R
│   ├── benefit-optouts-TWFE.R  State early opt-out of FPUC-$300
│   ├── state-reductions-TWFE.R State benefit reductions
│   ├── inline_stats.R          Writes paper/inline_stats.tex
│   ├── CARES/                  Main analysis (state × week)
│   └── replication/            Cross-language replication and late-stage robustness
└── paper/
    └── GFM UI Paper.pdf        Current draft of the paper
```

## Walkthrough of the analysis

Scripts use `here::here()` for all paths and read the Census API key from the `CENSUS_API_KEY` environment variable. Package dependencies are installed once with `source("setup.R")`.

### 1. Exploratory analysis (state/county × quarter)

`Code/cleanData.R` merges the campaign data with ACS, LAUS, UI claims, replacement rates, rural-urban codes, and social capital indices. It aggregates to state-quarter and county-quarter panels, overall and by category. `Code/regressions.R` estimates the early descriptive models: trends in campaigns and donations, Poisson models by category, and heterogeneity by education, income, and internet access. `text_mining.R` trains a random forest on post-October 2020 descriptions to predict which earlier campaigns would have been labeled financial emergencies.

### 2. Main panel construction (`Code/CARES/cleanCARES.R`)

This script builds the state-week panels `weekCamp` and `weekLabel`. It pulls ACS covariates (`getACS.R`), weekly ETA-539 claims, PUA/PEUC claims, and COVID-19 hospitalizations, then codes `treat` from the program dates. The panels are saved in the R workspace used by all later steps.

### 3. Main estimates (`Code/CARES/run_all_CARES.R`)

This script runs the full set of CARES specifications and writes the paper's tables and figures (LaTeX and PDF files, not included here):

| Step | Output |
|---|---|
| Main TWFE on totals and categories | `main_results*.tex`, `category_results*.tex`, `fin_emerg_official.tex` |
| Event studies | `event_study_total.pdf`, `event_study_fin_emerg.pdf` |
| Sun & Abraham | `sunab_robustness.tex`, `sunab_event_study.pdf` |
| Poisson PPML | `poisson_robustness.tex` |
| Donut, placebo, replacement-rate heterogeneity | `donut_robustness.tex`, `placebo_treatment.tex`, `het_replacement_rate.tex` |
| Combined comparison | `robustness_comparison.tex` |

`regressionsCARES.R`, `figuresCARES.R`, and `tablesCARES.R` are the interactive versions of the same models.

### 4. Alternative estimators

| Script | Purpose | Output |
|---|---|---|
| `CARES/build_fpuc300_termination.R` | Builds the 2021 FPUC-$300 termination panel | Intermediate panel |
| `CARES/robustness_methods.R` | Callaway & Sant'Anna, IPW 2×2 DiD, program-specific indicators, timing balance, randomization inference, Honest DiD on the termination design | `cs_termination.tex`, `matched_did.tex`, `program_specific.tex`, `balance_timing.tex`, `randinf.tex`, `honest_did_termination.tex`, `cs_event_study.pdf` |
| `CARES/build_text_proxy.R` | Full-period text-based financial-emergency outcome from job-loss keywords | `text_proxy.tex`, `text_proxy_event_study.pdf` |
| `replication/referee2_export.R` → `CARES/dcdh_episodes.do` → `CARES/dcdh_table.R` | Exports panels, estimates dCDH effects per treatment episode in Stata, formats the table | `dcdh_episodes.tex` |
| `replication/referee2_robustness_fix.R` | Wild cluster bootstrap and Honest DiD bounds for the main model | `wild_bootstrap.tex`, `honest_did.tex` |

`replication/referee2_replicate_main_results.do` and `.py` re-estimate the main specifications in Stata (`reghdfe`) and Python (`statsmodels`). They check that the R estimates reproduce across software.

### 5. Manuscript numbers

`Code/inline_stats.R` writes every number cited in the text to `paper/inline_stats.tex` as a LaTeX macro. Therefore, the prose updates automatically when the estimates change.

## Software

- R 4.4.2 with `fixest`, `did`, `HonestDiD`, `tidycensus`, `tidyverse`, and the other packages listed in `setup.R`
- Stata with `did_multiplegt_dyn` and `reghdfe` (dCDH estimates and replication)
- Python 3 with `pandas` and `statsmodels` (replication only)
