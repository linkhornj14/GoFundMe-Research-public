library(data.table) ## For some minor data wrangling
# Packages: see setup.R
library(fixest)   
library(fastDummies)
library(tidyverse)
library(sandwich)
library(zoo)
library(here)

countyMonth  <- read_csv(here::here("Data", "raw files", "countyMonth.csv"))
countyLabel  <- read_csv(here::here("Data", "raw files", "countyLabel.csv"))
stateQuarter <- read_csv(here::here("Data", "raw files", "stateQuarter.csv"))
stateLabel   <- read_csv(here::here("Data", "raw files", "stateLabel.csv"))

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
setFixest_etable(style.tex = style.tex("aer", signif.code = NA), postprocess.tex = set_rules, 
                 fitstat = ~ r2 + n)

#estimate trend of campaigns and number of donations 
m1 <- feols(log(amountTotal) ~  i(year)  + avgURate +log(median_income) + log(total_population) +
              percent_black + public_health_insurance + avglforce + avgRatio1 +percent_poverty + log(totalInitial) + log(totalFinal)
            | state+quarter ,
            # , weights= stateQuarter[(stateQuarter$year>=2017 & stateQuarter$year<=2023),]$total_population  
            ,cluster= ~state
            , data=subset(stateQuarter, subset=(year>=2017 & year<=2023)))
summary(m1)
coefplot(m1,pt.join=TRUE,drop="Constant",keep='year', ci.lty = 0, ci.width = 0,ci.fill = TRUE,
         ci.fill.par = list(col = 'blue', alpha = 0.2), geom_style='ribbon',main='Effects on Log Amount Donated')


#estimate trends of total amount given
m2 <- feols(log(donationsTotal) ~  i(year)  + avgURate +log(median_income)  + percent_black + public_health_insurance + avglforce + log(totalInitial) + log(totalFinal) +
           avgRatio1 + percent_poverty  | state+quarter 
            # , weights= stateQuarter[(stateQuarter$year>=2017 & stateQuarter$year<=2023),]$total_population  
            ,cluster= ~state
            , data=subset(stateQuarter, subset=(year>=2017 & year<=2023)))
summary(m2)
coefplot(m2,pt.join=TRUE,drop="Constant",keep='year', ci.lty = 0, ci.width = 0,ci.fill = TRUE,
         ci.fill.par = list(col = 'blue', alpha = 0.2), geom_style='ribbon',main="Effects on Log # of Donations")

#changes in log donation per campaign over time (by anonymous)
t <- donations %>%
  mutate(year = year(created_at),month = month(created_at),quarter = case_when(
    month >= 7 & month <= 9 ~ 'Q3'
    , month >= 10 & month <= 12 ~ 'Q4'
    , month >= 1 & month <= 3 ~ 'Q1'
    , month >= 4 & month <= 6 ~ 'Q2' )) %>%
  filter(year>=2017 & year<=2023) %>%
  left_join(df[c('id','label','state','goal_amount')],join_by(fund_id==id))

m3 <- feols(log(amount) ~ i(year) + goal_amount | state+quarter+fund_id, cluster=~state, data=t,split=t$is_anonymous) 
coefplot(m3,pt.join=TRUE,drop="Constant",keep='year', ci.lty = 0, ci.width = 0,ci.fill = TRUE,
         ci.fill.par = list(col = 'blue', alpha = 0.2), geom_style='ribbon')
etable(m3)
#poisson model - fit overall and for categories

summary(s1 <- glm(campaigns ~ avgURate + totalInitial + totalFinal + averageWBA2 + 
                    median_income + total_population + percent_public_assist + percent_poverty + state,
                  family=poisson(link='log'),
                  weights = subset(stateQuarter, subset=c(year>=2017 & year<=2019))$total_population,
                  data= subset(stateQuarter, subset=c(year>=2017 & year<=2019))))

#robust standard errors
cov.s1 <- vcovHC(s1, type="HC0")
std.err <- sqrt(diag(cov.s1))
r.est <- cbind(Estimate= coef(s1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(s1)/std.err), lower.tail=FALSE),
               LL = coef(s1) - 1.96 * std.err,
               UL = coef(s1) + 1.96 * std.err)
r.est

#chi-squared test to see residual deviance
with(s1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

p1 <- predict(s1,subset(stateQuarter, subset=c(year>=2017 & year<=2021)),type="response")

test <- stateQuarter %>%
  filter(year>=2017 & year<=2021) %>%
  cbind(predCamps = p1) %>%
  mutate(date = as.Date(yq(quarter))
                    , excessRate =(campaigns/predCamps)-1)


#FE state Regressions - normally specified model

state1FE <- feols(c(log(campaigns),log(donationsTotal),log(amountTotal)) ~ avgURate + avglforce + log(totalInitial) + log(totalFinal)  + 
                    avgRatio1   + log(median_income) + percent_poverty 
                  + percent_black + log(total_population) + public_health_insurance    | state+quarter,
                  cluster=~state, 
                  #weights = stateQuarter$total_population
                  ,data =stateQuarter )
etable(state1FE, style.tex = style.tex("aer"), fitstat = ~ r2 + n,tex=TRUE)

#unfortunately there's no real effects when breaking out -> everything is concentrated for medical campaigns

label1FE <- feols(c(log(num_medical),log(num_memorial),log(num_family),log(num_emergency),log(num_financial_emergency),log(num_business),log(num_community),log(num_animals), log(num_education)) ~ avgURate + avglforce + log(totalInitial) + log(totalFinal)  + 
                    avgRatio1   + log(median_income) + percent_poverty 
                  + percent_black + log(total_population) + public_health_insurance    | state+quarter,
                  cluster=~state, 
                  #weights = stateLabel$total_population,
                  data =stateLabel)
etable(label1FE, style.tex = style.tex("aer"), fitstat = ~ r2 + n)

#heterogeneity analysis
#creates dummy columns for above median observations in data based on list
het <- map_dfc(hetero, ~stateQuarter %>%
                 group_by(quarter) %>%
                 mutate(!!paste0("med","_",.x) := median(get(.x),na.rm=TRUE),
                        !!paste0("above","_",.x) :=  if_else(get(.x) > get(!!paste0("med","_",.x)), 1,0)) %>%
                 ungroup() %>%
                 select(tail(names(.), 2))) 

het2 <- map_dfc(hetero2, ~countyMonth %>%
                  group_by(month) %>%
                  mutate(!!paste0("med","_",.x) := median(get(.x),na.rm=TRUE),
                         !!paste0("above","_",.x) :=  if_else(get(.x) > get(!!paste0("med","_",.x)), 1,0)) %>%
                  ungroup() %>%
                  select(tail(names(.), 2)))

#bind matrices together
stateQuarter <- stateQuarter %>%
  cbind(het)
countyMonth <- countyMonth %>%
  cbind(het2)


#highschool - above median highschool - significance for initial claims, final payments, average weekly wage decreases campaign nums
hschool <- feols(c(campaigns,donationsTotal,amountTotal) ~ avgURate + avglforce + totalInitial + totalFinal + 
        averageWBA2 + avgRatio1 + avgWeekWage+ total_population  
      | state+quarter,
      cluster=~state,data=stateQuarter, weights=stateQuarter$total_population, fsplit = stateQuarter$above_ed_percent_highschool)
etable(hschool)

#bachelors - INTERESTING RESULTS: below median is significant for initial and final, above no BUT higher WBA has significant decrease in campaign num andavg weekly wage has positive effect
bach <- feols(c(campaignNum) ~ avgURate + avglforce + totalInitial + totalFinal + 
                   averageWBA2 + avgRatio1 + avgWeekWage+ total_population  
                 | state+quarter,
                 cluster=~state,data=stateQuarter, weights=stateQuarter$total_population, fsplit = stateQuarter$above_ed_percent_bachelors)
etable(bach)

#internet : only effects on initial claims and final payments, nothing crazy
internet <- feols(c(campaigns,donationsTotal) ~ avgURate + avglforce + totalInitial + totalFinal + 
                   averageWBA2 + avgRatio1 + avgWeekWage+ total_population  
                 | state+quarter,
                 cluster=~state,data=stateQuarter, weights=stateQuarter$total_population, fsplit = stateQuarter$above_has_internet)
etable(internet)

#income
income <- feols(c(campaigns,donationsTotal) ~ avgURate + avglforce + totalInitial + totalFinal + 
                   averageWBA2 + avgRatio1 + avgWeekWage+ total_population  
                 | state+quarter,
                 cluster=~state,data=stateQuarter, weights=stateQuarter$total_population, fsplit = stateQuarter$above_median_income)
etable(income)

#county analysis
#no health insurance decreases campaigns and donations
cl1 <- feols(c(campaignNum, meanSuccess) ~ urate + no_health_insurance + percent_poverty + total_population + 
                  percent_black + volunteering_rate_county + total_population + RUCC_2023 + ec_county 
                | State+month+label,
                cluster=~State,data=countyLabel, weights=countyLabel$total_population)
etable(cl1)

#figure out economic significance on campaign num. a one s.d increase * coefficient is how much of mean dependent?
#mean(stateQuarter$campaignNum,na.rm=TRUE)
#sd(stateQuarter$totalFinal,na.rm=TRUE)


#data.table way
#a[urate>median(urate,na.rm=T)] works as well - could probably find a way to loop through 
#all factor vars to run heterogeneity analysis pretty easy , with all outcomes


# Look at the results
#model.matrix(~-1+label, data=df) -- this create a dummy variable matrix , maybe not use?
