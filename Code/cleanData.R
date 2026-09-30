rm(list = ls())
# Packages: see setup.R

library(data.table) ## For some minor data wrangling
library(janitor)
library(fixest)   
library(tidycensus)
library(fastDummies)
library(tidyverse)
library(zoo)
library(here)

# setwd removed — here::here() handles project-relative paths

#load in raw data
df <- read_csv(here::here("Data", "raw files", "feed_county_ACS.csv"))
#donations <- read_csv(here::here("Data", "raw files", "donations.csv"))
acsVars <- read_csv(here::here("Data", "acs_variables.csv"))
countyLaus <- read_csv(here::here("Data", "laus_county_monthly.csv"))
urbanRural <- read_csv(here::here("Data", "Ruralurbancontinuumcodes2023.csv"))
ec <- read_csv(here::here("Data", "raw files", "social_capital_county.csv"))
acsVars$variable_call <- substr(acsVars$variable_call,1,nchar(acsVars$variable_call)-1)
fips <- read_csv(here::here("Data", "state fips.csv"))
repRates <- read_csv(here::here("Data", "UI_Replacement_Rates_2010_2024.csv"))
claims <- read_csv(here::here("Data", "monthly claims and financial data.csv"))
stateLaus <- read_csv(here::here("Data", "laus_state_monthly.csv"))
desc <- read_csv(here::here("Data", "raw files", "raw_description.csv"))

# loop over list of years and get 5 year acs estimates
years <- lst(2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
stateACS <- map_dfr(
  years,
  ~ get_acs(
    geography = "state",
    variables = acsVars$variable_call,
    #survey='acs1',
    year = .x,
    geometry = FALSE
  ),
  .id = "year"  # when combining results, add id var (name of list item)
) %>%
  select(-moe) %>%
  left_join(acsVars[c('variable_call','variable_label')], join_by(variable==variable_call)) %>%# shhhh
  pivot_wider(names_from=variable_label,values_from=estimate,id_cols=c(GEOID,NAME,year)) %>%
  mutate( year = as.integer(year)) %>%
  arrange(NAME, year) %>%
  bind_rows(get_acs(geography='state',variables=acsVars$variable_call,year='2023',survey='acs1') %>%
              select(-moe) %>%
              left_join(acsVars[c('variable_call','variable_label')], join_by(variable==variable_call)) %>%# shhhh
              pivot_wider(names_from=variable_label,values_from=estimate,id_cols=c(GEOID,NAME)) %>%
              mutate( year = 2023)
  )



countyACS <- map_dfr(
  years,
  ~ get_acs(
    geography = "county",
    variables = acsVars$variable_call,
    year = .x,
    geometry = FALSE
  ),
  .id = "year"  # when combining results, add id var (name of list item)
) %>%
  select(-moe) %>%
  left_join(acsVars[c('variable_call','variable_label')], join_by(variable==variable_call)) %>%# shhhh
  pivot_wider(names_from=variable_label,values_from=estimate,id_cols=c(GEOID,NAME,year)) %>%
  mutate(year = as.integer(year)) %>%
  arrange(NAME, year) %>%
  bind_rows(get_acs(geography='county',variables=acsVars$variable_call,year='2023',survey='acs1') %>%
              select(-moe) %>%
              left_join(acsVars[c('variable_call','variable_label')], join_by(variable==variable_call)) %>%# shhhh
              pivot_wider(names_from=variable_label,values_from=estimate,id_cols=c(GEOID,NAME)) %>%
              mutate( year = 2023)
  )

#clean indiviudal data, turn to factors, alter variables 
urbanRural <- urbanRural %>%
  pivot_wider(names_from=Attribute,values_from=Value,id_cols=c(FIPS,State)) %>%
  mutate(metro = as.factor(if_else(as.integer(RUCC_2023)<=3,1,0)),
         FIPS = as.integer(FIPS))

countyLaus$month <- as.integer(countyLaus$month)
df$success <- as.numeric(df$current_amount>=df$goal_amount)
countyACS$GEOID <- as.integer(countyACS$GEOID) 
stateACS$GEOID <- as.integer(stateACS$GEOID)

#aggregate by state + quarter
claims <- claims %>%
  mutate(month = lubridate::month(as.Date(date,"%m/%d/%Y")),year = lubridate::year(as.Date(date,"%m/%d/%Y")),
         quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' )) %>%
  group_by(state,year,quarter) %>%
  summarize(totalInitial = sum(initialClaims),
            totalBenefits = sum(benefitsPaid),
            totalFinal = sum(finalPayments))

countyLaus <- countyLaus %>%
  filter(!month ==13) %>%
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' )) %>%
  group_by(countyFIPS,year,quarter) %>%
  summarize(avgURate = mean(urate,na.rm=TRUE),
            avglforce = mean(lforce,na.rm=TRUE))
  

#this is already at the quarter level so the averages should work as long as you maintain the time series
colnames(repRates) <- make.names(colnames(repRates))
repRates <- repRates %>%
  mutate(quarter = as.numeric(Quarter),quarter = case_when(
    quarter == 1 ~ 'Q1'
    , quarter == 2 ~ 'Q2'
    , quarter == 3 ~ 'Q3'
    , quarter == 4 ~ 'Q4' )) %>%
  group_by(State, Year ,quarter) %>%
  summarize(avgRatio1 = mean(Replacement.Ratio.1,na.rm=TRUE),
    avgRatio2 = mean(Replacement.Ratio.2,na.rm=TRUE),
    averageWBA2 = mean(as.numeric(Average.WBA),na.rm=TRUE),
    avgWeekWage = mean(averageWeeklyWageQ,na.rm=TRUE)
  )

stateLaus <- stateLaus %>%
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' )) %>%
  group_by(StateAbbr,year, quarter) %>%
  summarize(avgURate = mean(urate,na.rm=TRUE),
            avglforce = mean(lforcePR,na.rm=TRUE))


stateLabel <- df %>% 
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' ),yearQtr = paste(as.character(year),".",quarter)) %>%
  group_by(state,year,quarter,yearQtr,category) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount)) %>%
  select(category,campaigns) %>%
  pivot_wider(names_from=category, values_from=campaigns, values_fill=0,names_prefix="num_") %>%
  clean_names()

  
stateQuarter <- df %>% 
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' ),yearQtr = paste(as.character(year),".",quarter)) %>%
  group_by(state,year,quarter,yearQtr) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), 
            amountTotal= sum(current_amount))  
  


#aggregate by county + quarter
countyQuarter <- df %>% 
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' ),yearQtr = paste(as.character(year),".",quarter),
         state_county_fips = as.integer(state_county_fips)) %>%
  group_by(state,state_county_fips,year,quarter,yearQtr) %>%
  summarize(campaignNum = n(),
            donationsTotal = sum(total_donations),
            aggregateDonated = sum(current_amount),
            meanSuccess = mean(success))


#aggregate by county + quarter + label
countyLabel <- df %>% 
  mutate(month = as.numeric(month),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' ),
         state_county_fips = as.integer(state_county_fips),yearQtr = paste(as.character(year),".",quarter)) %>%
  group_by(state,state_county_fips,year,quarter,yearQtr,category) %>%
  summarize(campaignNum = n(),
            donationsTotal = sum(total_donations),
            aggregateDonated = sum(current_amount),
            meanSuccess = mean(success))


#merge in other data
countyLabel <- countyLabel %>%
  left_join(countyLaus, join_by(state_county_fips==countyFIPS,year==year,quarter==quarter)) %>%
  left_join(countyACS, join_by(state_county_fips==GEOID,year==year)) %>%
  left_join(urbanRural, join_by(state_county_fips==FIPS)) %>%
  left_join(ec[c('county','ec_county','exposure_grp_mem_county','volunteering_rate_county','civic_organizations_county')],join_by(state_county_fips==county))

countyQuarter <- countyQuarter %>%
  left_join(countyLaus, join_by(state_county_fips==countyFIPS,year==year,quarter==quarter)) %>%
  left_join(countyACS, join_by(state_county_fips==GEOID,year==year)) %>%
  left_join(urbanRural, join_by(state_county_fips==FIPS)) %>%
  left_join(ec[c('county','ec_county','exposure_grp_mem_county','volunteering_rate_county','civic_organizations_county')],join_by(state_county_fips==county))

stateQuarter <- stateQuarter %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  left_join(stateACS,join_by(state_fips==GEOID,year==year)) %>%
  left_join(claims, join_by(state==state,year==year,quarter==quarter)) %>%
  left_join(repRates, join_by(state==State,year==Year,quarter==quarter)) %>%
  left_join(stateLaus, join_by(state==StateAbbr,year==year,quarter==quarter)) 

stateLabel <- stateLabel %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  left_join(stateACS,join_by(state_fips==GEOID,year==year)) %>%
  left_join(claims, join_by(state==state,year==year,quarter==quarter)) %>%
  left_join(repRates, join_by(state==State,year==Year,quarter==quarter)) %>%
  left_join(stateLaus, join_by(state==StateAbbr,year==year,quarter==quarter))

#drop unneeded data
drops <- c("unemployment_rate","X","regionType","sa","period","Population_2020","countyurate")
countyLabel <- countyLabel[ , !(names(countyLabel) %in% drops)]
countyQyarter <- countyQuarter[ , !(names(countyQuarter) %in% drops)]

countyQuarter$donPerCamp <- countyQuarter$donationsTotal/countyQuarter$campaignNum
countyLabel$donPerCamp <-  countyLabel$donationsTotal/countyLabel$campaignNum

write_csv(countyQuarter, here::here("Data", "raw files", "countyQuarter.csv"))
write_csv(countyLabel,   here::here("Data", "raw files", "countyLabel.csv"))
write_csv(stateQuarter,  here::here("Data", "raw files", "stateQuarter.csv"))
write_csv(stateLabel,    here::here("Data", "raw files", "stateLabel.csv"))