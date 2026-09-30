# NOTE: rm(list = ls()) removed so this script can be sourced from
# run_all_CARES.R after cleanCARES has already built df/weekCamp/weeks/etc.
# Behavior when run standalone in RStudio is unchanged (the workspace it
# would have cleared was just empty in a fresh session anyway).
library(tidyverse)
library(janitor)
library(tidytext)
library(textTinyR)
library(caret)
library(tm)
library(here)



# setwd removed — here::here() handles project-relative paths

desc <- read_csv(here::here("Data", "raw files", "raw_description.csv"))

#Preprocess the text data: convert to lowercase, remove stopwords, and tokenize
#can't get this function to work for whatever reason
clean_text <- function(text_data) {
  text_data %>%
    mutate(text_data = removePunctuation(fund_description),
           text_data = removeNumbers(text_data),
           text_data = stripWhitespace(text_data),
           text_data = tolower(text_data)) 
}

bizCamps <- df %>%
  filter(category=='business' & (year==2020 | year == 2021)) %>%
  left_join(desc[c('campaign_id','fund_name','fund_description')],join_by(id==campaign_id)) %>%
  mutate(
    fund_description = gsub("\",\"fund_description_excerpt\":\".*","",fund_description),
    fund_description = removePunctuation(fund_description),
    fund_description = removeNumbers(fund_description),
    fund_description = stripWhitespace(fund_description),
    fund_description = tolower(fund_description),
    fund_description = gsub("nnnn.*","",fund_description),
    word_count = str_count(fund_description,'\\w+'),
    category = as.factor(category)
  ) %>%
  distinct()

words <- c('salaries','wages','employees','workers','staff')

bizCamps <- cbind(bizCamps, sapply(words, function(x) as.integer(grepl(x, bizCamps$fund_description))))
bizCamps <- bizCamps %>%
  mutate(wordFlag = as.numeric(if_any(c(salaries,wages,employees,workers,staff), ~ . == 1)))

bizCamps <- bizCamps %>% 
  filter(year == 2020 | year == 2021) %>%
  mutate(week = lubridate::week(ymd(date(created_at)))) %>%
  group_by(state,year,week,wordFlag) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount)) %>%
  mutate(year = as.factor(year)) %>%
  left_join(weeks[c('state','stateAbbr','fWeek','pWeek','fWeek1End','lWeekStart','lWeekEnd','fWeek2','fWeek2End')],join_by(state==stateAbbr)) %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  #mutate(state_fips = as.integer(state_fips)) %>%
  left_join(state_ACS[c('GEOID','year','median_income','total_population','percent_poverty')],join_by(state_fips==GEOID,year==year)) %>%
  left_join(covid,join_by(state==state,year==year,week==week)) %>%
  left_join(claimsW[c('state_abb','year','week','initial_claims','insured_unemployment_rate','continued_claims')],join_by(state==state_abb,year==year,week==week)) %>%
  left_join(puaData,join_by(state==state,year==year,week==refWeek)) %>%
  mutate(treat = ifelse( (week>=fWeek & week<=fWeek1End) & (year==2020),1,ifelse(!is.na(lWeekStart) & ((week>=lWeekStart & week<=lWeekEnd) | (week>=lWeekStart & lWeekEnd==1)) & (year==2020),1,
                                                                                 ifelse( (week>=fWeek2 & week<=fWeek2End) & (year==2021),1,0))),
         cases = replace_na(cases,0),
         treat = replace_na(treat,0),
         puaIC = replace_na(puaIC,0),
         peucCC = replace_na(peucCC,0),
         puaCC = replace_na(puaCC,0)) %>%
  fill(initial_claims,insured_unemployment_rate,continued_claims)


biz1 <- feols(c(log(campaigns)) ~ treat + median_income + cases + initial_claims + insured_unemployment_rate + continued_claims + puaIC ## Other controls
      | state + year^week,  ## FEs
      cluster = ~state, ## Clustered SEs,
      weights=bizCamps$total_population,
      data = bizCamps, split = bizCamps$wordFlag)

etable(biz1,tex=TRUE)