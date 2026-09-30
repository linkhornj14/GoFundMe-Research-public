library(data.table) ## For some minor data wrangling
# Packages: see setup.R
library(fixest)   
library(tidycensus)
library(fastDummies)
library(dplyr)
library(tidyr)
library(xtable)
library(here)

source(here::here("Code", "functions.R"))

rawFeedEthnic <- read_csv(here::here("Data", "raw files", "raw_feed_genEthnic.csv"))
desc          <- read_csv(here::here("Data", "raw files", "raw_description.csv"))

rawFeedEthnic$success <- if_else(rawFeedEthnic$current_amount > rawFeedEthnic$goal_amount,1,0)
options(scipen = 100, digits = 8)


#better table?
vars <- c('current_amount','goal_amount','success','total_donations','social_share_total','campaign_hearts', 'total_photos','total_comments','total_updates',
          'auto_fb_post_mode','probability_black','probability_hispanic','probability_female')

# Summary statistics table — write to paper/tables/summary_stats.tex
# so paper can \input{} it instead of hand-typing values.
sumstats_xt <- xtable(
  summarise_continuous(
    subset(rawFeedEthnic, subset = (goal_amount < quantile(goal_amount, .99))),
    vars
  ),
  type    = "latex",
  caption = "Summary Statistics for GoFundMe Campaigns",
  label   = "tab:summary_stats"
)
dir.create(here::here("paper", "tables"), recursive = TRUE, showWarnings = FALSE)
print(
  sumstats_xt,
  file               = here::here("paper", "tables", "summary_stats.tex"),
  include.rownames   = FALSE,
  caption.placement  = "top"
)
rawFeedEthnic %>%
  subset(subset=(goal_amount<quantile(goal_amount,.99))) %>%
  left_join(desc, join_by(id==campaign_id)) %>%
  summarise_continuous("word_count")

#summary of donations
donations %>%
  group_by(fund_id,amount) %>%
  summarize(count = n(), mean = mean(amount), sum = sum(amount))