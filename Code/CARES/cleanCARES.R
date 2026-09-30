rm(list = ls())
library(tidyverse)
library(tidycensus)   # needed for census_api_key() / get_acs() / load_variables()
library(httr)
library(jsonlite)
library(janitor)
library(here)
options(download.file.method = "libcurl")


# setwd removed — here::here() handles project-relative paths
# The Census API key is read from the environment, never hardcoded. Set it once
# per machine in ~/.Renviron (CENSUS_API_KEY=...) or export it before running.
# NOTE: the previously hardcoded key was committed to git history and must be
# revoked/rotated at https://api.census.gov/data/key_signup.html.
census_key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(census_key)) {
  stop("CENSUS_API_KEY is not set. Add it to ~/.Renviron or call ",
       "Sys.setenv(CENSUS_API_KEY = \"<your key>\") before sourcing cleanCARES.R.")
}
census_api_key(census_key)



#read in raw campaign data + fips
df   <- read_csv(here::here("Data", "raw files", "feed_county_ACS.csv"))
fips <- read_csv(here::here("Data", "state fips.csv"))

#get ACS data for years of interest -> run getACS to create functions to grab and create table of data
years <- lst(2020,2021) #set years you want to grab data
source(here::here("Code", "CARES", "getACS.R"))
state_ACS <- stateACS(years)
state_ACS$year <- as.factor(state_ACS$year)


#get state populations
#vars <- load_variables(dataset = "acs5",year=2021)
pops <- get_acs(variables = 'B01001_001' ,geography='state',year =2021)
pops$GEOID <- as.integer(pops$GEOID)
pops <- merge(x=pops,y=fips,by.x="GEOID" ,by.y="state_fips")

#state LAUS
sLaus <- read_csv(here::here("Data", "laus_state_monthly.csv"))
sLaus <- sLaus %>%
  filter(year == 2020 | year ==2021) %>%
  mutate(month = as.numeric(substr(month,2,3)),
         year = as.factor(year)) %>%
  select(c('StateAbbr','year','month','urate','empPop'))


#get weekly state claims
# DOL ETA-539 (State Weekly Claims for UI, NSA) — raw download from
# https://oui.doleta.gov/unemploy/csv/ar539.csv has coded columns; map the
# 5 we need into the human-readable names downstream code expects.
# Mapping based on the standard DOL ETA-539 record layout:
#   st   -> state abbreviation
#   c2   -> reflecting-week-ended date
#   c3   -> NSA initial claims (this week)
#   c8   -> NSA continued claims (insured unemployment)
#   c19  -> NSA insured unemployment rate (%)
# Verify against the DOL record layout if downstream results look odd.
claimsW_raw <- read_csv(
  here::here("Data", "State Weekly Claims for Unemplyment Insurance Data Not Seasonally Adjusted.csv"),
  show_col_types = FALSE
)
claimsW <- claimsW_raw %>%
  transmute(
    state_abb                 = st,
    filed_week_ended          = c2,
    initial_claims            = c3,
    continued_claims          = c8,
    insured_unemployment_rate = c19
  ) %>%
  mutate(
    week  = lubridate::week(mdy(filed_week_ended)),
    month = lubridate::month(mdy(filed_week_ended)),
    year  = lubridate::year(mdy(filed_week_ended)),
    year  = as.factor(year)
  ) %>%
  left_join(fips[c("state_name", "state_abb")], by = "state_abb") %>%
  left_join(sLaus, join_by(state_abb == StateAbbr, year == year, month == month))


#get covid-19 hospitalizations from Atlantic
covid <- GET(url="https://api.covidtracking.com/v1/states/daily.json")
covid <- fromJSON(rawToChar(covid$content))
covid$date <- as.Date(paste(covid$date, "01", sep=""), "%Y%m%d")

covid <- covid %>%
  mutate(week = lubridate::week(as.Date(date,"%Y%m%d")),
         year = lubridate::year(as.Date(date,"%Y%m%d")),
         year = as.factor(year)) %>%
  group_by(state,year,week) %>%
  summarize(cases = sum(hospitalizedCurrently,na.rm=TRUE))

#load in PUA and PEUC data
puaData <- read_csv(here::here("Data", "weekly_pandemic_claims.csv"))
puaData <- puaData %>%
  left_join(fips[c('state_fips','state_abb')],join_by(State==state_abb)) %>%
  rename(state=State,rptWeek=Rptdate,puaIC=`PUA IC`,refDate=`Reflect Date`,puaCC=`PUA CC`,peucCC=`PEUC CC`) %>%
  select(-c(...8,...7)) %>%
  mutate(rptDate = mdy(rptWeek), refDate= mdy(refDate),
         rptWeek = week(rptDate), refWeek = week(refDate),
         year = as.factor(year(rptDate)))
  
  

#load in treatment weeks
weeks <- read_csv(here::here("Data", "treated weeks.csv"))
weeks <- weeks %>%
  mutate(fpucWeek = mdy(fpucWeek),
         puaWeek = mdy(puaWeek),
         lwaWeekStart = mdy(lwaWeekStart),
         lwaWeekEnd = mdy(lwaWeekEnd),
         reinstatedFPUC = mdy(reinstatedFPUC),
         fpucEnd2 = mdy(fpucEnd2),
         fpucEnd1 = mdy(fpucEnd1),
         fWeek = lubridate::week(fpucWeek),
         pWeek = lubridate::week(puaWeek),
         fWeek1End = lubridate::week(fpucEnd1),
         lWeekStart = lubridate::week(lwaWeekStart),
         lWeekEnd = lubridate::week(lwaWeekEnd),
         fWeek2 = lubridate::week(reinstatedFPUC),
         fWeek2End = lubridate::week(fpucEnd2)
        ) 

#weekly group by state and label
weekCamp <- df %>% 
  filter(year == 2020 | year == 2021) %>%
  mutate(week = lubridate::week(ymd(date(created_at)))) %>%
  group_by(state,year,week) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount)) %>%
  mutate(year = as.factor(year))

weekLabel <- df %>% 
  filter(year == 2020 | year ==2021) %>%
  mutate(month = lubridate::month(ymd(date(created_at))),week = lubridate::week(ymd(date(created_at)))) %>%
  group_by(state,year,week,category) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount)) %>%
  select(category,campaigns) %>%
  pivot_wider(names_from=category, values_from=campaigns, values_fill=0,names_prefix="num_") %>%
  clean_names() %>%
  mutate(year = as.factor(year))

#join treatment weeks *** NEED TO CONVERT GEOID TO CHARACTER. MAKE SURE THERE ARE NO MISSING OBSERVATIONS
#somehow is duplicating when joining -> something to pay attention to ***TABLING THIS TO DO GENERAL DIFF_IN_DIFF
#weekCamp <- weekCamp %>%
#  left_join(weeks[c('stateAbbr','year','fWeek','treated')],join_by(state==stateAbbr,year==year,week==fWeek)) %>%
#  left_join(weeks[c('stateAbbr','year','pWeek','treated')],join_by(state==stateAbbr,year==year,week==pWeek)) %>%
#  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
#  #mutate(state_fips = as.integer(state_fips)) %>%
#  left_join(state_ACS[c('GEOID','year','median_income','total_population','percent_poverty')],join_by(state_fips==GEOID,year==year)) %>%
#  left_join(covid,join_by(state==state,year==year,week==week)) %>%
#  left_join(claimsW[c('state_abb','year','week','initial_claims','insured_unemployment_rate','continued_claims')],join_by(state==state_abb,year==year,week==week)) %>%
  #distinct() %>%
#  group_by(state) %>%
#  mutate(treatF = treated.x, treatP = treated.y,
#         treatF = replace_na(treatF, 0),
#         treatP = replace_na(treatP, 0),
#         time2treatF = ifelse(treatF == 1, 0, row_number()-row_number()[treatF==1]),
#         time2treatP = ifelse(treatP == 1, 0, row_number()-row_number()[treatP==1]),
#         treatF = 1,
#         treatP = 1,
#         cases = replace_na(cases,0)) %>%
#  select(-c('treated.x','treated.y'))

weekCamp <- weekCamp %>%
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

weekLabel <- weekLabel %>%
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
  group_by(state) %>%
  fill(initial_claims,insured_unemployment_rate,continued_claims) %>%
  ungroup()



