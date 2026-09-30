library(data.table) ## For some minor data wrangling
# Packages: see setup.R
library(fixest)   
library(dplyr)
library(tidyr)
library(tidycensus)
library(here)

#census_api_key(Sys.getenv("CENSUS_API_KEY"))  # never hardcode the key
fips <- fips_codes %>% distinct(fips_codes$state,fips_codes$state_code,fips_codes$state_name, keep.all=TRUE)
vars <- load_variables(dataset = "acs5",year=2021,)
pops <- get_acs(variables = 'B01001_001' ,geography='state',year =2021)
pops <- merge(x=pops,y=fips,by.x="GEOID" ,by.y="fips_codes$state_code")


#df2 <- read.csv("~/TWFEStates.csv")
df <- read.csv(here::here("Data", "raw files", "feed_county_ACS.csv"))
df2 <- df %>% 
  group_by(state,year,month,label) %>%
  summarize(campaignNum = n(),
            donationsTotal = sum(total_donations),
            aggregateDonated = sum(current_amount))


df2$StateAbbr <- df2$state

#merge and keep columns
keep <- c("fips_codes$state","estimate")
df2 <- merge(x=df2,y=pops[keep],by.x='state_prefix',by.y='fips_codes$state')

#drop columns
to_drop <- c('X')
df2 <- df2[,!(colnames(df2) %in% to_drop)]

# Let's create a more user-friendly indicator of which states received treatment
df2$treatmentMonth <- ifelse((df2$state=='KY' & df2$year=='2023' & df2$month==7), 1, 
                      ifelse((df2$state=='AL' & df2$year=='2020' & df2$month==1), 1,
                      ifelse((df2$state=='KS' & df2$year=='2021' & df2$month==4), 1,
                     ifelse((df2$state=='MO' & df2$year=='2022' & df2$month==1), 1,
                      ifelse((df2$state=='TN' & df2$year=='2023' & df2$month==12), 1,
                       ifelse((df2$state=='OK' & df2$year=='2023' & df2$month==1), 1,
                      ifelse((df2$state=='MA' & df2$year=='2023' & df2$month==7), 1,
                       ifelse((df2$state=='ID' & df2$year=='2023' & df2$month==7), 1,
                      ifelse((df2$state=='AR' & df2$year=='2024' & df2$month==1), 1,
                       ifelse((df2$state=='IA' & df2$year=='2022' & df2$month==7),1,0))))))))))

#treatment states
states <- c("KY","AL","KS","MO","TN","OK","MA","ID","AR","IA")
df2$treatment <- ifelse(df2$state %in% states,1,0)
df2 <- arrange(df2,state,year,month)


#just to remove extra years
df2 <- subset(df2, year >= 2019 & label == "Medical, Illness & Healing")
#calculate time to treatment var -> does not work if there are missing observations for months along years (should just refine sample before aggregating)
df2 <- df2 %>% 
  mutate(time2treat = ifelse(treatmentMonth == 1, 0,   row_number()-row_number()[treatmentMonth==1]))

#impute control group and bin other observations
df2$time2treat[is.na(df2$time2treat)] <- 0
#df2$time2treat <- ifelse(df2$time2treat>24,"25<",ifelse(df2$time2treat<-24,"-25>",df2$time2treat))

#fill in missing FIPS by grouping
df2$urate[is.na(df2$urate)] <- 0
df2 <- df2 %>%
  group_by(state_prefix) %>%
  fill(StateFIPS,.direction="updown")

#outcome variables
df2$deflatedID <- df2$id/df2$estimate
df2$logID <- log(df2$id)

#df2$time2treat <- as.numeric(levels(df2$time2treat))[df2$time2treat]
binneddf2 <- df2 %>%
  filter(between(time2treat, -12, 12))

#state reductions model 
mod_twfe = feols(campaignNum ~ i(time2treat, treatment, ref = -1) + ## Our key interaction: time × treatment status
                                        ## Other controls
                   state + year*month,         ## FEs
                 cluster = ~state,             ## Clustered SEs
                 data = binneddf2)


iplot(mod_twfe, 
      xlab = 'Months since Reduction',
      main = 'Effect of State Benefit Reduction on Num. of Campaigns',
      ylab= 'Diff. in # of Campaigns per State Cap.')

etable(mod_twfe,tex=TRUE)