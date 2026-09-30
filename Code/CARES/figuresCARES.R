rm(list = ls())
# Packages: see setup.R
library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)
library(geomtextpath)
library(here)


# setwd removed — here::here() handles project-relative paths

yearWeeks <- read_csv(here::here("Data", "yearWeek.csv"))

hist1 <- ggplot(weeks) +
  labs(color="Variable name",x="Date",y="Density",fill='Program')+
  geom_density(aes(x=fpucWeek, fill= "FPUC Start"), alpha = 0.2 )  +
  geom_density(aes(x=puaWeek, fill= "PUA Start"), alpha = 0.2) + 
  geom_vline(xintercept = as.numeric(ymd("2020-07-31")), linetype="dashed", 
             color = "black", size=0.5) +
  geom_textvline(label = "FPUC End", xintercept = as.numeric(ymd("2020-07-31")), vjust = 1.3,size=2.5) +
  geom_density(aes(x=lwaWeekStart, fill="LWA Start"), alpha=0.2) + 
  geom_density(aes(x=lwaWeekEnd, fill="LWA End"),alpha=0.2) +
  geom_vline(xintercept = as.numeric(ymd("2021-01-02")), linetype="dashed", 
             color = "black", size=0.5) +
  geom_textvline(label = "FPUC Reinstated", xintercept = as.numeric(ymd("2021-01-02")), vjust = 1.3,size=2.5) +
  geom_density(aes(x=fpucEnd2, fill="FPUC Second End"),alpha=0.2) +
  scale_fill_discrete(breaks=c('FPUC Start','PUA Start','LWA Start','LWA End','FPUC Second End'))#values = c("red","purple",'blue','green','yellow','orange'))
hist1

hist2 <- ggplot(weeks) +
  labs(color="Variable name",x='Week',y='Count',fill='Program') + 
  geom_density(aes(x=reinstatedFPUC),alpha=0.2) +
  geom_vline(xintercept = as.numeric(ymd("2021-01-02")), linetype="dashed", 
             color = "black", size=0.5) +
  geom_textvline(label = "FPUC Reinstated", xintercept = as.numeric(ymd("2021-01-02")), vjust = 1.3,size=2.5) +
  geom_density(aes(x=fpucEnd2, fill="FPUC Second End"),alpha=0.2) +
  scale_fill_manual(values = c('red'))
hist2





