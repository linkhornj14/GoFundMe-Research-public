# Packages: see setup.R
update.packages(checkBuilt = TRUE,ask=FALSE)
library(tidyverse)
library(predictrace)
library(here)

df <- read.csv(here::here("Data", "raw files", "raw_feed.csv"))
race <- predict_race(name=df$user_last_name)
gender <- predict_gender(name = df$user_first_name)
df <- cbind.data.frame(df,race,gender)

to_drop <- c('X','state_abb','name','match_name')
df <- df[,!(colnames(df) %in% to_drop)]

df %>%
  count(df$likely_race)

write.csv(df, here::here("Data", "raw files", "raw_feed_genEthnic.csv"))

