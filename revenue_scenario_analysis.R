# By: James Tan

# Date: 6/29/2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)

source('~/.Rprofile')
setwd(datapath)

cohortdata <- readRDS('revenue_modeling_cohortday.RData')
cohort_noplatform <- readRDS('revenue_modeling_cohortday_noplatform.RData')
spenddata <- readRDS('revenue_modeling_spend_daily_pull.RData')

group_variables <- quos(campaign_type, dup_account)

spenddata <- spenddata %>% filter(install_date >= as.Date('2017-7-1'), install_date < as.Date("2018-7-1"))

# User Inputs

# use old cpis or new cpis
useOldCPI <- FALSE

# g5 budget per month
g5spend_static <- c(19500, 19500, 9500, 9500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500) * 1000
g5spend_static <- c(9.5, 9.5, 9.5, 6.5, 6.5, 6.5, 6.5, 4.5, 4.5, 4.5, 4.5, 4.5) * 1e6

# new cpis by month
# newcpi_static <- c(4.06, 3.98, 2.39, 2.39, 1.57, 1.58, 1.60, 1.61, 1.63, 1.65, 1.66, 1.68)

log_model <- readRDS('cpi_spend_log_model.RData')
loess_model <- readRDS('cpi_spend_loess_model.RData')
spend_predict <- data.frame(monthly_spend =  g5spend_static / 1e6)
log_cpi <- predict(log_model, spend_predict)
loess_cpi <- predict(loess_model, spend_predict)
cpi <- ifelse(is.na(loess_cpi), log_cpi, loess_cpi)
newcpi_static <- cpi

# traffic and cpi blend

# manual blend

g5_assumptions <- read.csv('g5_assumptions.csv')
summary(g5_assumptions)

g5_assumptions <- g5_assumptions %>%
  mutate(
    spend = spend * 1e6
  )

dates <- seq(as.Date('2018-11-1'), by=1, length.out=length(unique(spenddata$install_date)))

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
    installs = installs / n,
    spend = spend / n
  ) %>%
  rename(
    total_spend_new = spend,
    installs_new = installs
  ) %>%
  select(install_date, month, campaign_type, dup_account, installs_new, total_spend_new, cpi)

# predicted cpis

incent_cpi <- c(1.77012085111802, 1.77012085111802, 1.77012085111802, 1.78140990726239,
                1.78185362026448, 1.78317302239132, 1.78490547308897, 1.74789276233169,
                1.74974321933756, 1.75155992751899, 1.75334409588783, 1.75681933452236)

notincent_cpi <- c(7.99266826459223, 7.99266826459223, 7.99266826459223, 7.28130767046531,
                   7.27835200661655, 7.26944310710371, 7.25746554620041, 6.79767377283929,
                   6.78459600103935, 6.77138080705498, 6.75802527210347, 6.73088103014196)

g5_assumptions <- g5_assumptions %>%
  mutate(
    cpi = ifelse(campaign_type == 'incent', incent_cpi, cpi),
    cpi = ifelse(campaign_type == 'not-incent', notincent_cpi, cpi)
  )

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
    installs = ifelse(spend == 0, installs / n, spend / cpi)
  ) %>%
  rename(
    total_spend_new = spend,
    installs_new = installs
  ) %>%
  select(install_date, month, campaign_type, dup_account, installs_new, total_spend_new, cpi)


# %>%
#   group_by(month, campaign_type, dup_account) %>%
#   summarise(
#     installs = sum(installs),
#     total_spend = sum(total_spend),
#     cpi = total_spend / installs
#   )


# plot of rpdau and revenue curve assumptions from FFXV
meltdata <- melt(cohortdata, id.vars=c('cohortday', 'campaign_type', 'install_platform', 'dup_account', 'type'))
plotdata <- filter(meltdata, cohortday <= 300)
ggplot(plotdata, aes(cohortday, value, group=type, color=type)) +
  geom_line() +
  facet_wrap(~variable, scales='free_y')

# These can also be changed but this is harder to do manually since they are on a by cohortday, by campaign type/platform/dup basis

# Run Model

installdata <- spenddata %>%
  group_by(install_date) %>%
  summarise(
    installs = sum(installs),
    total_spend = sum(total_spend),
    cpi = total_spend / installs
  )

newmonth_date <- seq(from=as.Date('2018-11-1'), by='month', length.out = length(unique(spenddata$month)))
datediff <- as.numeric(as.Date('2018-11-1') - min(installdata$install_date))

monthdata <- spenddata %>%
  group_by(month) %>%
  summarise(
    monthly_spend = sum(total_spend),
    monthly_cpi = monthly_spend / sum(installs),
    installs = sum(installs),
    install_date = min(install_date)
  ) %>%
  mutate(
    g5spend = g5spend_static,
    newcpi = newcpi_static,
    newmonth_date = newmonth_date,
    newmonth = as.factor(format(newmonth_date, '%Y-%m')),
    newcpi_newinstalls = g5spend / newcpi,
    oldcpi_newinstalls = g5spend / monthly_cpi
  )

newdata <- spenddata %>%
  mutate(
    install_date = install_date + datediff,
    newmonth = as.factor(format(install_date, '%Y-%m'))
  ) %>%
  group_by(newmonth) %>%
  mutate(
    install_percent = installs / sum(installs),
    total_spend_percent = total_spend / sum(total_spend)
  ) %>%
  left_join(select(monthdata, newmonth, g5spend, newcpi_newinstalls, oldcpi_newinstalls), by=c('newmonth')) %>%
  mutate(
    total_spend_new = total_spend_percent * g5spend,
    installs_new = install_percent * ifelse(useOldCPI, oldcpi_newinstalls, newcpi_newinstalls),
    month = newmonth
  )

join_vars <- quos(cohortday, !!! group_variables)
dates <- data.frame(date = unique(newdata$install_date))

alldata <- merge(newdata, dates) %>% filter(date >= install_date) %>%
  mutate(
    cohortday = as.numeric(date - install_date)
  ) %>%
  select(install_date, date, cohortday, month, !!! group_variables, installs_new) %>%
  left_join(cohort_noplatform, by=sapply(join_vars, quo_name)) %>%
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
    rpdau = rpdau_ret * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
  ) %>%
  left_join(installdata, by='date') %>%
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
  group_by(date, month) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
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
    rpdau_existing = rpdau_total - rpdau_gain
  ) %>%
  ungroup() %>%
  mutate(
    rpdau_churn = lag(rpdau_total) - rpdau_existing
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
    arprpdau = revenue / sum(rpdau),
    rpdau_end = rpdau[which(date == max(date))],
    rpdau_mid = rpdau[which(format(date, '%d') == '15')],
    dau = dau[which(date == max(date))],
    spend = installs * cpi
  ) %>%
  mutate(
    rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01')))
  ) %>%
  left_join(rpdau_delta, by= 'month') %>%
  select(
    month, spend, revenue, rev_rpdau_mid_arprpdau, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_churn, arpdau, arprpdau, installs, cpi
  )

write.csv(predictdata, 'g5_logcpi_daily_predictions.csv', row.names=FALSE)
write.csv(month_output, 'g5_logcpi_monthly_predictions.csv', row.names=FALSE)

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
    rpdau_churn = lag(rpdau_total) - rpdau_existing
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
    !!! group_variables, month, spend, revenue, rev_rpdau_mid_arprpdau, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_churn, arpdau, arprpdau, installs, cpi
  )

write.csv(predictdata, 'g5_by_type_logcpi_daily_predictions.csv', row.names=FALSE)
write.csv(month_output, 'g5_by_type_logcpi_monthly_predictions.csv', row.names=FALSE)
