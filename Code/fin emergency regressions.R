library(data.table) ## For some minor data wrangling
# Packages: see setup.R
library(fixest)   
library(fastDummies)
library(tidyverse)
library(sandwich)
library(zoo)
library(stringr) 
library(stargazer)
library(here)

# setwd removed — here::here() handles project-relative paths

#raw feed data is only 2017 to 2024
#feed <- read_csv(here::here("Data", "raw files", "raw_feed.csv"))

feed <- df %>%
  filter((category %in% c('emergency','financial emergency')) & date(created_at)>='2017-01-01' & date(created_at)<='2023-12-31') %>%
  filter(goal_amount <= quantile(goal_amount,.95)) %>%
  left_join(desc[c('campaign_id','fund_name','fund_description','word_count')],join_by(id==campaign_id)) %>%
  left_join(test_data[c('id','probPredictions','predictions')],join_by(id==id)) %>%
  mutate(goalMet = if_else(current_amount>=goal_amount,1,0),
    pctFunded = current_amount/goal_amount,
         anyDonations = if_else(donation_count>0,1,0),
         date = date(created_at),
    probPredictions = na.fill(probPredictions,0),
    post = if_else(date>='2020-10-01', 1, 0 ),
    fund_description = gsub("\",\"fund_description_excerpt\":\".*","",fund_description),
    word_count = str_count(fund_description,'\\w+'), 
    treat = if_else(((category=='financial emergency') | (predictions=='financial emergency' & category=='emergency' & date(created_at)<'2020-10-01')),1,0),
    descQuality = if_else(word_count>=400, 1,0),
    nonPersonal = if_else(bene_first_name!='' | bene_last_name !='',1,0),
    nonPersonal = na.fill(nonPersonal,0)
        )



#get donations for campaigns over this time period
#compDonations <- donations %>%
#  select(donation_id,amount,is_anonymous, fund_id,created_at) %>%
#  mutate(date = date(created_at)) %>%
#  filter((date>='2020-01-01') & (date<='2021-12-31')) %>%
#  left_join(feed[c('id','goal_amount','category')],join_by(fund_id==id)) %>%
#  filter(category %in% c('emergency','financial emergency')) %>%
#  arrange(fund_id,date) %>%
 # mutate(lastDonation = lag(amount))

#summary statistics
stats <- function(vars,df){
  df %>%  }

feed %>%
  filter(treat==1 & post==1) %>%
  select(goal_amount,current_amount,total_donations,total_photos,total_updates,campaign_hearts,social_share_total,word_count,goalMet) %>%
  stargazer(median=TRUE,digits=2,title='Summary Statistics')


#effect of grouping on campaign success
competition1 = feols(goalMet ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                                log(goal_amount)+ log(goal_amount)*treat + total_donations +total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                                | state+year^month ,  ## FEs
                              cluster = ~state, ## Clustered SEs,
                              data = feed)

#effect of grouping on amount raises
competition2 = feols(log(current_amount) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount) + log(goal_amount)*treat + total_donations +total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year^month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed)

#table
etable(competition1,competition2,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n)

#total donations
competition6 = feols(log(total_donations) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount)  +total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year+month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed)

etable(competition6,style.tex = style.tex("aer"), fitstat = ~ r2 + n)

#heterogeneity results - Non Personal Campaigns
competition3 = feols(c(goalMet,log(current_amount)) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount)  + total_donations + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year^month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed,split='nonPersonal')

etable(competition3,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n,keep=c('treat','post','treat:post'))

#predicted race
competition4 = feols(c(goalMet,log(current_amount)) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount) + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year^month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed,split='likely_race',split.drop=c('2races','american_indian','black, white'))

etable(competition4,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n,keep=c('treat','post','treat:post'))

#predicted gender
competition5 = feols(c(goalMet,log(current_amount)) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount) + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year^month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed,split='likely_gender',split.drop='female, male')

etable(competition5,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n,keep=c('treat','post','treat:post'))

etable(competition3,competition4,competition5,tex=TRUE,style.tex = style.tex("aer"),fitstat = ~ r2 + n,keep=c('treat','post','treat:post'))
#sub demographics
competition7 = feols(log(current_amount) ~  treat  + post + treat*post + ## Our key interaction: time × treatment status
                       log(goal_amount) + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
                     | state+year^month ,  ## FEs
                     cluster = ~state, ## Clustered SEs,
                     data = feed,split='demo')

etable(competition7,style.tex = style.tex("aer"), fitstat = ~ r2 + n)

#including donation data
feedDon <- feed %>%
  select(id,treat,post,goal_amount,current_amount,total_photos,total_updates,total_donations,campaign_hearts,social_share_total,unemployment_rate,descQuality,state,year,month) %>%
  left_join(donations[c('fund_id','amount','is_anonymous','created_at')],join_by(id==fund_id)) %>%
  group_by(id) %>%
  arrange(created_at) %>%
  mutate(firstDon = first(amount),
         lastDon = last(amount),
           topDon = max(amount),
         prevDon = lag(amount),
         lastAnon = if_else(lag(is_anonymous)==1,1,0),
         prevDon = na.fill(prevDon,0),
         lastAnon = na.fill(lastAnon,0))

#summary statistics
t <- feedDon %>%
  filter(treat==0 & post==1) 
sum(is.na(t$amount))
t[is.na(t$amount),]

#effect of last donation anonymous
m3 <- feols(log(amount) ~   prevDon + lastAnon + prevDon*lastAnon  ## Our key interaction: time × treatment status
              #+ log(goal_amount) + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
            | state+year^month,  ## FEs
            cluster = ~state, ## Clustered SEs,
            data = feedDon,split='treat')

etable(m3,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n)

m4 <-  feols(log(amount) ~   prevDon + lastAnon + prevDon*lastAnon  ## Our key interaction: time × treatment status
             #+ log(goal_amount) + total_photos + total_updates + campaign_hearts + social_share_total + unemployment_rate  + descQuality     ## Other controls
             | state+year^month+id,  ## FEs
             cluster = ~state, ## Clustered SEs,
             data = feedDon, split='treat')

etable(m3,m4,tex=TRUE,style.tex = style.tex("aer"), fitstat = ~ r2 + n)