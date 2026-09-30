# Packages: see setup.R
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)

# Common ggsave defaults for this project (paper output, NOT slides)
.fig_dim <- list(width = 12, height = 5, bg = "white")

#trend proportion
p_category_trend <- df %>%
  filter(! category %in% c('competition','faith','newlywed','other','sports','wishes','travel','animals','NA')) %>%
  group_by(year,category) %>%
  count() %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(ttl = sum(n),
         pct_ttl = n/ttl) %>%
  ggplot(aes(year, pct_ttl, color = category, group = category)) +
  geom_line() +
  scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
  #scale_x_date() +
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "proportion", x = "")
ggsave(here::here("paper", "figures", "category_trend.pdf"), p_category_trend,
       width = .fig_dim$width, height = .fig_dim$height, bg = .fig_dim$bg)

p_fin_emerg_goal_trend <- df %>%
  filter(category %in% c('financial emergency') & year >= 2021) %>%
  group_by(year) %>%
  summarize(med = median(goal_amount)) %>%
  ggplot(aes(year, med)) +
  geom_line() +
  #scale_y_continuous(label = scales::percent_format(accuracy = 1)) +
  #scale_x_date(breaks='month') +
  #theme(axis.text.x = element_text(angle = 90))+
  labs(y = "", x = "")
ggsave(here::here("paper", "figures", "fin_emerg_goal_trend.pdf"), p_fin_emerg_goal_trend,
       width = .fig_dim$width, height = .fig_dim$height, bg = .fig_dim$bg)


#labor market plots
p_campaigns_vs_urate <- ggplot(stateQuarter,aes(x=campaigns,y=avgURate)) +
  geom_point(stat='summary') +
  geom_smooth(method='lm') +
  ylab("Avg. UR") +
  xlab("State Quarterly Campaigns Published") +
  ggtitle("Quarterly State Campaigns Published by Avg. UR")
ggsave(here::here("paper", "figures", "campaigns_vs_urate.pdf"), p_campaigns_vs_urate,
       width = .fig_dim$width, height = .fig_dim$height, bg = .fig_dim$bg)

#good, show these
p_campaigns_vs_claims <- ggplot(stateQuarter,aes(x=campaigns,y=totalInitial)) +
  geom_point(stat='summary') +
  geom_smooth(method='lm') +
  ylab("Log Initial Claims") +
  xlab("State Quarterly Campaigns Published") +
  ggtitle("Quarterly State Campaigns Published by Total Initial UI Claims")
ggsave(here::here("paper", "figures", "campaigns_vs_claims.pdf"), p_campaigns_vs_claims,
       width = .fig_dim$width, height = .fig_dim$height, bg = .fig_dim$bg)


ggplot(stateQuarter,aes(x=campaigns,y=averageWBA2)) +
  geom_point(stat='summary') +
  geom_smooth(method='lm') +
  ylab("Log Average WBA") +
  xlab("Quarterly State Campaigns Published") +
  ggtitle("Quarterly State Campaigns Published by Average WBA")


#now for financial emergency/emergency
ggplot(stateLabel,aes(x=num_financial_emergency,y=avgURate)) +
  geom_point(stat='summary') +
  geom_smooth(method='lm') +
  ylab("Avg. UR") +
  xlab("State Quarterly Campaigns Published") +
  ggtitle("Quarterly State Fin. Emergency Campaigns Published by Avg. UR")
  

ggplot(stateLabel,aes(x=log(num_financial_emergency),y=log(totalInitial))) +
  geom_point(stat='summary') +
  geom_smooth() +
  ylab("Log Initial Claims") +
  xlab("Log State Quarterly Campaigns Published") +
  ggtitle("Quarterly State Fin. Emergency Campaigns Published by Total Initial UI Claims")


#share by amount raised -> idk what to make of this. slightly positive
df %>%
  subset(subset=(goal_amount<quantile(goal_amount,.99))) %>%
  ggplot(aes(x=log(social_share_total),y=log(current_amount))) +
  geom_point() +
  geom_smooth(method='lm')

#bar chart for charity organized campaigns
df %>%
  group_by(charity_organized) %>%
  summarize(count = n()) %>%
  ggplot(aes(x=as.factor(charity_organized),y=count)) +
  geom_bar(stat='identity',fill='steelblue') +
  theme_minimal() +
  xlab("Charity Organized")

#category goal amount groups -> would have to weight proportions by category overall pop.
df %>%
  #filter(category %in% c('medical','emergency','family','financial_emergency','memorial')) %>%
  mutate(goalBin = ifelse(goal_amount<=1000,"<=1000",
                          ifelse((goal_amount>1000) & (goal_amount<=2500), "1000-2500",
                          ifelse((goal_amount>2500) & (goal_amount<=5000),"2500-5000",
                          ifelse((goal_amount>5000) & (goal_amount<=7500),"5000-7500","7500<"))))) %>%
  group_by(goalBin, is_personal_charity) %>%
  summarize(count = n(), avgSuccess = mean(success)) %>%
  ggplot(aes(x=goalBin,y=count/sum(count),fill=is_personal_charity)) +
  geom_bar(stat='identity') +
  theme_minimal()

#success rates by bin groups and category
df %>%
  filter(category %in% c('medical','emergency','family','financial emergency','memorial','education')) %>%
  mutate(goalBin = ifelse(goal_amount<=1000,"<=1000",
                          ifelse((goal_amount>1000) & (goal_amount<=2500), "1000-2500",
                                 ifelse((goal_amount>2500) & (goal_amount<=5000),"2500-5000",
                                        ifelse((goal_amount>5000) & (goal_amount<=7500),"5000-7500","7500<"))))) %>%
  group_by(goalBin,category) %>%
  summarize(count = n(), avgSuccess = mean(success)) %>%
  ggplot(aes(x=goalBin,y=avgSuccess,fill=category)) +
  geom_bar(stat='identity',position='dodge') 

#histogram of recurring donors ???

#histogram of success
df %>%
  mutate(pct = current_amount/goal_amount) %>%
  filter(pct<100) %>%
  ggplot(aes(x=pct)) +
  geom_histogram() 

#donation per total raised ?? does this change when the initial gift is larger than average?
donations %>%
  #filter(fund_id == 20718460) %>%
  left_join(df[c('id','created_at','goal_amount')],join_by(fund_id==id)) %>%
  rename(fundStart = created_at.y, donationTime = created_at.x) %>%
  arrange(fund_id,donationTime) %>%
  group_by(fund_id) %>%
  mutate_at(vars(donationTime,fundStart), ~as.POSIXct(strptime(.x, format = c("%Y-%m-%dT%H:%M:%OS")))) %>%
  mutate(sum = sum(amount),giftPCT = amount/goal_amount, lastGift = as.double(difftime(donationTime,lag(donationTime),units='hours')),
         sinceStart = as.double(difftime(donationTime,fundStart)), cumsum = cumsum(amount)/goal_amount) %>%
  ggplot(aes(sinceStart,cumsum)) +
  geom_point(summary='stat',fun='mean') 

#anonymous giving
donations %>%
  mutate(date = as.Date(created_at), year = year(date)) %>%
  group_by(year,is_anonymous) %>%
  summarize(count = n()) %>%
  filter(year < 2024) %>%
  ggplot(aes(x = year, y = count, color = is_anonymous, group = is_anonymous)) +
  geom_line() +
  labs(y = "count", x = "")

#POISSON FIGURES
ggplot(test, aes(y=excessRate, x=date)) +
  geom_point(stat='summary',fun='mean') +
  ggtitle("Average Excess Campaign Rate over COVID-19 Pandemic")



