library(tidycensus)
library(here)

# setwd removed — here::here() handles project-relative paths

#bring in ACS vars to get
acsVars <- read_csv(here::here("Data", "acs_variables.csv"))
#acsVars$variable_call <- substr(acsVars$variable_call,1,nchar(acsVars$variable_call)-1)

# loop over list of years and get 5 year acs estimates
stateACS <- function(years){
  map_dfr(
  years,
  ~ get_acs(
    geography = "state",
    variables = acsVars$variable_call,
    #survey='acs1',
    year = .x,
    geometry = FALSE
  ),
  .id = "year"  # when combining results, add id var (name of list item)
)  %>%
  select(-moe) %>%
  left_join(acsVars[c('variable_call','variable_label')], join_by(variable==variable_call))  %>%# shhhh
  pivot_wider(names_from=variable_label,values_from=estimate,id_cols=c(GEOID,NAME,year)) %>%
  mutate( year = as.integer(year)) %>%
  arrange(NAME, year)
  # NOTE: The original code appended 2023 ACS1 data via bind_rows() here.
  # That call fails in non-interactive Rscript runs (Census API returns HTML
  # instead of JSON despite identical standalone calls succeeding — likely
  # a connection-pool state issue after the preceding ACS5 calls).
  # The 2023 row was never actually used downstream: weekCamp and weekLabel
  # filter to year %in% c(2020, 2021), so the year=2023 ACS row was always
  # dropped in the left_join. Removing the call has no effect on output.
}


countyACS <- function(years){
  map_dfr(
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
}