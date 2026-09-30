rm(list = ls())
library(tidyverse)
library(lubridate)
library(here)


table1 <- df %>%
  filter((year==2020 | year ==2021) & (goal_amount<quantile(goal_amount,.99)) & (current_amount<quantile(current_amount,.99))) %>%
  left_join(desc[c('campaign_id','fund_name','fund_description')],join_by(id==campaign_id)) %>%
  mutate(word_count = str_count(fund_description,'\\w+'),met_goal = current_amount>goal_amount) %>%
  summarise_continuous(c('goal_amount','current_amount','total_donations','total_photos',
                       'total_updates','campaign_hearts','social_share_total','word_count',
                       'met_goal'))
table1

#fin emergency numbers compared to claims
monthlyClaims <- read.csv(here::here("Data", "monthly claims and financial data.csv"))
monthlyClaims <- monthlyClaims %>%
  mutate(year = lubridate::year(mdy(date)),month = lubridate::month(mdy(date)))

#total benefits paid out during fin emergency period
monthlyClaims %>%
  filter((month>=10 & year==2020) | year==2021) %>%
  summarize(total = sum(as.integer(benefitsPaid))) 

#average state weekly fin. emergency campaigns -- 2 campaign
weekLabel %>%
  filter(((week>=40 & year==2020) | (year==2021))) %>%
  summarize(mean = mean(num_financial_emergency))

#number of state weeks that had available benefits for fin. emergency campaigns -- 2261 treated weeks 
weekLabel %>%
  filter(treat==1 & ((week>=40 & year==2020) | (year==2021))) %>%
  summarize(count = n())

# (2.06 * .103) * 2261

#average amount raised and target for fin. emergency campaigns during time period
df %>%
  mutate(month = lubridate::month(ymd(date(created_at))),week = lubridate::week(ymd(date(created_at)))) %>%
  filter( ((month>=10 & year==2020) | year==2021) & category=='financial emergency')  %>%
  summarize(raisedMean = mean(current_amount), goalMean = mean(goal_amount), total = sum(current_amount))

  
#earliest week for october 2020
df %>%
  mutate(month = lubridate::month(ymd(date(created_at))),week = lubridate::week(ymd(date(created_at)))) %>%
  filter(month==10 & year==2020) %>%
  summarize(min = min(week))


#donation statistics
donations <- read_csv(here::here("Data", "raw files", "donations.csv"))

table2 <- donations %>%
  mutate(year = year(date(created_at))) %>%
  filter((year==2020 | year==2021) & (amount<quantile(amount,.99))) %>%
  summarise_continuous(c('amount'))
table2

table3 <- weekCamp %>%
  summarize(camps = mean(campaigns),
            donations = mean(donationsTotal),
            amount = mean(amountTotal))
table3
