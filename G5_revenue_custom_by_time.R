# By: James Tan

# Date: 7/23/2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)

rm(list = ls())

source('~/.Rprofile')
setwd(datapath)

monnb <- function(d){
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
}

default_objects <- ls()

cohortdata <- readRDS('revenue_modeling_cohortday.RData')
cohort_noplatform <- readRDS('revenue_modeling_cohortday_noplatform.RData')

cohortdata_weeks <- readRDS('revenue_modeling_cohortday_weeks.RData')
cohort_noplatform_weeks <- readRDS('revenue_modeling_cohortday_noplatform_weeks.RData')

cohortdata_months <- readRDS('revenue_modeling_cohortday_months.RData')
cohort_noplatform_months <- readRDS('revenue_modeling_cohortday_noplatform_months.RData')

spenddata <- readRDS('revenue_modeling_spend_daily_pull.RData')

spenddata <- spenddata %>% filter(install_date >= as.Date('2017-7-1'), install_date < as.Date("2018-7-1"))



# USER INPUTS

# choose user performance of FFXV to be aggregated weekly, monthly, or over the entire first 6 months
# only choose one of weeks, months, or overall
aggregation_scale <- 'weeks'

if(aggregation_scale == 'weeks'){
  # weekly aggregation scale
  cohortdata <- cohortdata_weeks
  cohort_noplatform <- cohort_noplatform_weeks
  time_variable <- quo(weeks_since_launch)

}else if(aggregation_scale == 'months'){
  # monthly aggregation scale
  cohortdata <- cohortdata_months
  cohort_noplatform <- cohort_noplatform_months
  time_variable <- quo(months_since_launch)

}else{
  # default to overall aggregation
  time_variable <- quo()
}

# install platform
# include install platform in granularity or not
install_platform_granularity <- FALSE

if(install_platform_granularity){
  cohort_simulation <- cohortdata
  group_variables <- quos(campaign_type, install_platform, dup_account)
}else{
  cohort_simulation <- cohort_noplatform
  group_variables <- quos(campaign_type, dup_account)
}

rm(list = ls()[!ls() %in% c(default_objects, 'default_objects', 'spenddata', 'cohort_simulation', 'group_variables', 'time_variable', 'aggregation_scale', 'install_platform_granularity')])

# ARPU AS A FUNCTION OF ARPDAU AND RETENTION

arpu_as_ret_arpdau <- TRUE
if(arpu_as_ret_arpdau){
  cohort_simulation$arpu <- cohort_simulation$arpdau * cohort_simulation$retention
}

# MANUAL SPEND, TRAFFIC BLEND, AND CPI

# user input
# true if using user provided cpi, false if using log model to predict cpi
customcpi <- TRUE

filename <- 'burhan_assumptions_preload_as_incent'

g5_assumptions <- read.csv(paste0(filename, '.csv'))
summary(g5_assumptions)

# g5_assumptions <- g5_assumptions %>%
#   mutate(
#     spend = spend * 1e6
#   )

dates <- seq(as.Date('2018-11-1'), by=1, length.out=length(unique(spenddata$install_date)))

if(!customcpi){
  incent_log_model <- readRDS('cpi_incent_spend_log_model.RData')
  notincent_log_model <- readRDS('cpi_notincent_spend_log_model.RData')

  notincent_spend <- data.frame(monthly_spend = filter(g5_assumptions, campaign_type == 'not-incent') %>% pull(spend) / 1e6)
  incent_spend <- data.frame(monthly_spend = filter(g5_assumptions, campaign_type == 'incent') %>% pull(spend) / 1e6)

  incent_cpi <- predict(incent_log_model, incent_spend)
  notincent_cpi <- predict(notincent_log_model, notincent_spend)

  g5_assumptions <- g5_assumptions %>%
    mutate(
      cpi = ifelse(campaign_type == 'incent', incent_cpi, cpi),
      cpi = ifelse(campaign_type == 'not-incent', notincent_cpi, cpi)
    )
}

newdata <- tibble(install_date = dates) %>%
  mutate(
    month = as.factor(format(install_date, format='%Y-%m'))
  ) %>%
  group_by(month) %>%
  mutate(
    n = n()
  ) %>%
  left_join(g5_assumptions, by='month') %>%
  filter(!is.na(campaign_type)) %>%
  mutate(
    spend = spend / n,
    installs = if(customcpi){
      ifelse(spend == 0, installs / n, spend / cpi)
    }else{
      installs / n
    }
  ) %>%
  rename(
    total_spend_new = spend,
    installs_new = installs
  ) %>%
  ungroup() %>%
  mutate(
    days_since_launch = as.numeric(install_date - min(install_date)),
    weeks_since_launch = as.numeric(floor(difftime(install_date, min(install_date), units='weeks'))),
    months_since_launch = monnb(install_date) - monnb(min(install_date))
  ) %>%
  select(install_date, month, !! time_variable, !!! group_variables, installs_new, total_spend_new, cpi)

name <- paste(filename, 'weekly', sep='_')

# Run Model

join_vars <- quos(cohortday, !!! group_variables, !! time_variable)
dates <- data.frame(date = unique(newdata$install_date))

alldata <- merge(newdata, dates) %>% filter(date >= install_date) %>%
  mutate(
    cohortday = as.numeric(date - install_date)
  ) %>%
  select(install_date, date, cohortday, month, !!! group_variables, !! time_variable, installs_new) %>%
  left_join(cohort_simulation, by=sapply(join_vars, quo_name)) %>%
  select(-c(installs, dau, rpdau, payers, revenue)) %>%
  arrange(install_date, cohortday)

installdata <- newdata %>%
  group_by(install_date) %>%
  summarise(
    installs = sum(installs_new),
    total_spend = sum(total_spend_new),
    cpi = total_spend / installs
  ) %>%
  rename(date = install_date)

# daily predictions

predictdata <- alldata %>%
  mutate(
    revenue = arpu * installs_new,
    revenue_rpdau = revenue * rpdau_revshare,
    rpdau = rpdau_ret * installs_new,
    rpdau_20 = rpdau_20_ret_installs * installs_new,
    rpdau_100 = rpdau_100_ret_installs * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date) %>%
  summarise(
    revenue = sum(revenue),
    revenue_rpdau = sum(revenue_rpdau),
    rpdau = sum(rpdau),
    rpdau_20 = sum(rpdau_20),
    rpdau_100 = sum(rpdau_100),
    dau = sum(dau),
    payers = sum(payers),
    arprpdau = revenue_rpdau / rpdau
  ) %>%
  left_join(installdata, by='date') %>%
  mutate(
    month = as.factor(format(date, '%Y-%m'))
  )

predictdata_month <- alldata %>%
  mutate(
    revenue = arpu * installs_new,
    revenue_rpdau = revenue * rpdau_revshare,
    rpdau = rpdau_ret * installs_new,
    rpdau_20 = rpdau_20_ret_installs * installs_new,
    rpdau_100 = rpdau_100_ret_installs * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date, month) %>%
  summarise(
    revenue = sum(revenue),
    revenue_rpdau = sum(revenue_rpdau),
    rpdau = sum(rpdau),
    rpdau_20 = sum(rpdau_20),
    rpdau_100 = sum(rpdau_100),
    dau = sum(dau),
    payers = sum(payers),
    arprpdau = revenue_rpdau / rpdau
  ) %>%
  left_join(installdata, by='date')

rpdau_delta <- predictdata_month %>%
  ungroup() %>%
  dplyr::mutate(
    date_month = as.factor(format(date, '%Y-%m'))
  ) %>%
  group_by(date_month) %>%
  filter(date == max(date)) %>%
  summarise(
    rpdau_total = sum(rpdau),
    rpdau_gain = rpdau[which(date_month == month)],
    rpdau_existing = rpdau_total - rpdau_gain,
    rpdau_20_total = sum(rpdau_20),
    rpdau_20_gain = rpdau_20[which(date_month == month)],
    rpdau_20_existing = rpdau_20_total - rpdau_20_gain,
    rpdau_100_total = sum(rpdau_100),
    rpdau_100_gain = rpdau_100[which(date_month == month)],
    rpdau_100_existing = rpdau_100_total - rpdau_100_gain
  ) %>%
  ungroup() %>%
  mutate(
    rpdau_net_churn = lag(rpdau_total) - rpdau_existing,
    rpdau_20_net_churn = lag(rpdau_20_total) - rpdau_20_existing,
    rpdau_100_net_churn = lag(rpdau_100_total) - rpdau_100_existing
  ) %>%
  rename(month = date_month)

# monthly predictions

month_output <- predictdata %>%
  group_by(month) %>%
  summarise(
    revenue = sum(revenue),
    cpi = weighted.mean(cpi, installs),
    installs = sum(installs),
    arpdau = revenue / sum(dau),
    arprpdau = sum(revenue_rpdau) / sum(rpdau),
    rpdau_end = rpdau[which(date == max(date))],
    rpdau_mid = rpdau[which(format(date, '%d') == '15')],
    rpdau_20_end = rpdau_20[which(date == max(date))],
    rpdau_20_mid = rpdau_20[which(format(date, '%d') == '15')],
    rpdau_100_end = rpdau_100[which(date == max(date))],
    rpdau_100_mid = rpdau_100[which(format(date, '%d') == '15')],
    dau = dau[which(date == max(date))],
    spend = installs * cpi
  ) %>%
  mutate(
    rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01')))
  ) %>%
  left_join(rpdau_delta, by= 'month') %>%
  select(
    month, spend, revenue, cpi, installs, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_net_churn, rpdau_20_mid, rpdau_20_end, rpdau_20_gain, rpdau_20_net_churn, rpdau_100_mid, rpdau_100_end, rpdau_100_gain, rpdau_100_net_churn, arpdau, arprpdau
  )

write.csv(predictdata, paste0('g5_daily_predictions_', name, '.csv'), row.names=FALSE)
write.csv(month_output, paste0('g5_monthly_predictions_', name, '.csv'), row.names=FALSE)

# By Type

installdata <- newdata %>%
  group_by(install_date, !!! group_variables) %>%
  summarise(
    installs = sum(installs_new),
    total_spend = sum(total_spend_new),
    cpi = total_spend / installs
  ) %>%
  rename(date = install_date)

join_vars <- quos(date, !!! group_variables)
predictdata <- alldata %>%
  mutate(
    revenue = arpu * installs_new,
    rpdau = rpdau_ret * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date, !!! group_variables) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
  ) %>%
  left_join(installdata, by=sapply(join_vars, quo_name)) %>%
  ungroup() %>%
  mutate(
    month = as.factor(format(date, '%Y-%m'))
  )

predictdata_month <- alldata %>%
  mutate(
    revenue = arpu * installs_new,
    rpdau = rpdau_ret * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date, month, !!! group_variables) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
  ) %>%
  left_join(installdata, by=sapply(join_vars, quo_name))

rpdau_delta <- predictdata_month %>%
  ungroup() %>%
  dplyr::mutate(
    date_month = as.factor(format(date, '%Y-%m'))
  ) %>%
  group_by(!!! group_variables, date_month) %>%
  filter(date == max(date)) %>%
  summarise(
    rpdau_total = sum(rpdau),
    rpdau_gain = rpdau[which(date_month == month)],
    rpdau_existing = rpdau_total - rpdau_gain
  ) %>%
  group_by(!!! group_variables) %>%
  mutate(
    rpdau_net_churn = lag(rpdau_total) - rpdau_existing
  ) %>%
  rename(month = date_month)

# monthly predictions

join_vars <- quos(month, !!! group_variables)
month_output <- predictdata %>%
  group_by(!!! group_variables, month) %>%
  summarise(
    revenue = sum(revenue),
    cpi = weighted.mean(cpi, installs),
    installs = sum(installs),
    arpdau = revenue / sum(dau),
    arprpdau = revenue / sum(rpdau),
    rpdau_end = rpdau[which(date == max(date))],
    rpdau_mid = rpdau[which(format(date, '%d') == '15')],
    dau = dau[which(date == max(date))],
    spend = installs * cpi
  ) %>%
  mutate(
    rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01')))
  ) %>%
  left_join(rpdau_delta, by= sapply(join_vars, quo_name)) %>%
  select(
    !!! group_variables, month, spend, revenue, cpi, installs, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_net_churn, arpdau, arprpdau
  )

write.csv(predictdata, paste0('g5_by_type_daily_predictions_', name, '.csv'), row.names=FALSE)
write.csv(month_output, paste0('g5_by_type_monthly_predictions_', name, '.csv'), row.names=FALSE)
