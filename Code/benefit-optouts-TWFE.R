library(data.table) ## For some minor data wrangling
# Packages: see setup.R
library(fixest)   
library(tidyverse)
library(tidycensus)
library(httr)
library(jsonlite)
library(janitor)
library(ggfixest)
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

#census_api_key(Sys.getenv("CENSUS_API_KEY"))  # never hardcode the key
vars <- load_variables(dataset = "acs5",year=2021,)
pops <- get_acs(variables = 'B01001_001' ,geography='state',year =2021)
pops$GEOID <- as.integer(pops$GEOID)
pops <- merge(x=pops,y=fips,by.x="GEOID" ,by.y="state_fips")

#state LAUS
sLaus <- read_csv(here::here("Data", "laus_state_monthly.csv"))
sLaus <- sLaus %>%
  filter(year == 2020 | year ==2021) %>%
  mutate(month = as.numeric(month)) %>%
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
    year  = lubridate::year(mdy(filed_week_ended))
  ) %>%
  left_join(fips[c("state_name", "state_abb")], by = "state_abb") %>%
  left_join(sLaus, join_by(state_abb == StateAbbr, year == year, month == month))


#get covid-19 hospitalizations from Atlantic
covid <- GET(url="https://api.covidtracking.com/v1/states/daily.json")
covid <- fromJSON(rawToChar(covid$content))
covid$date <- as.Date(paste(covid$date, "01", sep=""), "%Y%m%d")

covid <- covid %>%
  mutate(week = lubridate::week(as.Date(date,"%Y%m%d")),
         year = lubridate::year(as.Date(date,"%Y%m%d"))) %>%
  group_by(state,year,week) %>%
  summarize(cases = sum(hospitalizedCurrently,na.rm=TRUE))

#get weekly campaign, donation, counts by state week
weekLabel <- df %>% 
  filter(year == 2020 | year ==2021) %>%
  mutate(week = lubridate::week(created_at)) %>%
  group_by(state,year,week,category) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount)) %>%
  select(category,campaigns) %>%
  pivot_wider(names_from=category, values_from=campaigns, values_fill=0,names_prefix="num_") %>%
  clean_names()

weekCamp <- df %>% 
  filter(year == 2020 | year == 2021) %>%
  mutate(week = lubridate::week(created_at)) %>%
  group_by(state,year,week) %>%
  summarize(campaigns = n(), donationsTotal = sum(total_donations), amountTotal= sum(current_amount))  

#load in treatment weeks
weeks <- read_csv(here::here("Data", "treated weeks.csv"))
weeks <- weeks %>%
  mutate(fpucWeek = mdy(fpucWeek),
         puaWeek = mdy(puaWeek),
         year = 2020,
         fWeek = lubridate::week(fpucWeek),
         pWeek = lubridate::week(puaWeek),
         treated = 1) 

#join treatment weeks *** NEED TO CONVERT GEOID TO CHARACTER. MAKE SURE THERE ARE NO MISSING OBSERVATIONS
weekCamp <- weekCamp %>%
  left_join(weeks[c('stateAbbr','year','fWeek','treated')],join_by(state==stateAbbr,year==year,week==fWeek)) %>%
  left_join(weeks[c('stateAbbr','year','pWeek','treated')],join_by(state==stateAbbr,year==year,week==pWeek)) %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  mutate(state_fips = as.integer(state_fips)) %>%
  left_join(stateACS[c('GEOID','year','median_income','total_population','percent_poverty','percent_black')],join_by(state_fips==GEOID,year==year)) %>%
  left_join(covid,join_by(state==state,year==year,week==week)) %>%
  left_join(claimsW[c('state_abb','year','week','initial_claims','insured_unemployment_rate')],join_by(state==state_abb,year==year,week==week)) %>%
  group_by(state) %>%
  mutate(treatF = treated.x, treatP = treated.y,
         treatF = replace_na(treatF, 0),
         treatP = replace_na(treatP, 0),
         time2treatF = ifelse(treatF == 1, 0, row_number()-row_number()[treatF==1]),
         time2treatP = ifelse(treatP == 1, 0, row_number()-row_number()[treatP==1]),
         treatF = 1,
         treatP = 1,
         cases = na.fill(cases,0)) %>%
  select(-c('treated.x','treated.y'))

weekLabel <- weekLabel %>%
  left_join(weeks[c('stateAbbr','year','fWeek','treated')],join_by(state==stateAbbr,year==year,week==fWeek)) %>%
  left_join(weeks[c('stateAbbr','year','pWeek','treated')],join_by(state==stateAbbr,year==year,week==pWeek)) %>%
  left_join(fips[c('state_fips','state_abb')],join_by(state==state_abb)) %>%
  left_join(stateACS[c('GEOID','year','median_income','total_population','percent_poverty','percent_black')],join_by(state_fips==GEOID,year==year)) %>%
  left_join(covid,join_by(state==state,year==year,week==week)) %>%
  left_join(claimsW[c('state_abb','year','week','initial_claims','insured_unemployment_rate')],join_by(state==state_abb,year==year,week==week)) %>%
  group_by(state) %>%
  mutate(treatF = treated.x, treatP = treated.y,
         treatF = replace_na(treatF, 0),
         treatP = replace_na(treatP, 0),
         time2treatF = ifelse(treatF == 1, 0, row_number()-row_number()[treatF==1]),
         time2treatP = ifelse(treatP == 1, 0, row_number()-row_number()[treatP==1]),
         treatF=1,
         treatP=1,
         cases = na.fill(cases,0)) %>%
  select(-c('treated.x','treated.y'))


binCampF <- weekCamp %>%
  filter(between(time2treatF,-4,4))

binCampP <- weekCamp %>%
  filter(between(time2treatP,-4,4))

binLabelF <- weekLabel %>%
  filter(between(time2treatF, -4, 4))

binLabelP <- weekLabel %>%
  filter(between(time2treatP, -4, 4))

#should proxy by description including words like rent, utilities, groceries
#all campaign model -> should control for benefit level??
mod_twfe = feols(log(campaigns) ~ i(time2treatF, treatF, ref = -1) + ## Our key interaction: time × treatment status
                   cases/total_population + log(initial_claims) + insured_unemployment_rate                    ## Other controls
                   | state + week,         ## FEs
                   cluster = ~state,        ## Clustered SEs
                 #weights = binCampF$total_population,
                 data = binCampF)

iplot(mod_twfe, 
      xlab = 'Weeks Since First Payment',
      main = 'Effect of FPUC Payment Timing on Weekly GoFundMe Campaigns',
      ylab= 'Diff. in Log # of Campaigns')

etable(mod_twfe)

binCampF %>% 
  filter_all(any_vars(is.na(.)))

sunab_twfe <- feols(log(campaigns) ~ sunab(fWeek,week,ref.p=-1) + cases + log(initial_claims) + insured_unemployment_rate
                    | state+week,
                    cluster=~state,
                    data=binCampF)
iplot(sunab_twfe)

#category model
twfeL = feols(num_emergency ~ i(time2treatF, treatF, ref = -1) + ## Our key interaction: time × treatment status
                   cases + initial_claims + insured_unemployment_rate |                    ## Other controls
                   state + week,  ## FEs
                 cluster = ~state, ## Clustered SEs,
              weights=binLabelF$total_population,
                 data = binLabelF)

ggiplot(twfeL,geom='errorbar',main='Staggered Treatment on Category',multi_style = 'facet')

etable(twfeL, style.tex = style.tex("aer"), fitstat = ~ r2 + n)