# By: James Tan

# Date: 6/27/2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)
library(vtreat)
library(gbm)
library(ICEbox)

source('~/.Rprofile')
setwd(datapath)

# gamedata <- read.csv('revenue_modeling_pull.csv')
# spenddata <- read.csv('revenue_modeling_spend_pull.csv')
#
# sapply(gamedata, class)
# gamedata <- gamedata %>%
#   mutate(
#     game_id = as.factor(game_id),
#     session_daily_ts = as.POSIXct(session_daily_ts),
#     dup_account = ifelse(dup_account == 1, TRUE, FALSE)
#   )
#
# sapply(spenddata, class)
# summary(spenddata)
# spenddata <- spenddata %>%
#   mutate(
#     game_id = as.factor(game_id),
#     month_ts = as.POSIXct(mon),
#     month = as.factor(format(month_ts, format='%Y-%m'))
#   ) %>%
#   select(-c(game_id, mon))
#
# names(gamedata) <- tolower(names(gamedata))
#
# gamedata <- filter(gamedata, session_daily_ts >= as.POSIXct('2017-06-28 00:00:00'))
# spenddata <- filter(spenddata, month_ts >= as.POSIXct('2017-06-01 00:00:00'))
#
# names(gamedata) <- tolower(names(gamedata))
#
# saveRDS(gamedata, 'revenue_modeling_game.RData')
# saveRDS(spenddata, 'revenue_modeling_spend.RData')

gamedata <- readRDS('revenue_modeling_game.RData')
spenddata <- readRDS('revenue_modeling_spend.RData')


# cohortdata <- read.csv('revenue_modeling_cohortday_pull_country.csv')
# cohortdata <- readRDS('revenue_modeling_cohortday_all_dates.RData')

cohortdata <- filter(mydata, install_event_date >= as.Date("2018-6-1"), install_event_date <= as.Date('2017-10-28'))

type_variables <- quos(campaign_type)
time_variable <- quo(weeks_since_launch)

group_variables <- c(type_variables, time_variable)

# summary(cohortdata)

# cohortdata <- cohortdata %>%
#   filter(install_platform == 1 | install_platform == 2) %>%
#   mutate(
#     install_platform = as.factor(ifelse(install_platform == 1, 'iOS', ifelse(install_platform == 2, 'android', 'Other'))),
#     dup_account = as.factor(ifelse(dup_account == 1, 'dup', 'non-dup')),
#     country_tier = as.factor(country_tier),
#     type = droplevels(interaction(!!! group_variables))
#   )

# CUMULATIVE INSTALLS

# cohortdata <- cohortdata %>%
#   group_by(!!! group_variables, cohortday) %>%
#   arrange(install_event_date) %>%
#   mutate(
#     cum_installs = cumsum(installs),
#     install_group = floor(cum_installs / 1e6)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     install_group = as.factor(install_group)
#   )
#
# type_variables <- quos(campaign_type, install_platform, dup_account)
# group_variables <- c(type_variables, quo(install_group))

# AGGREGATION

cohortdata <- cohort_noplatform_weeks %>%
  mutate(
    game_id = as.factor(23),
    campaign_type = as.factor(ifelse(campaign_type == 'not-incent', 'nonincent', as.character(campaign_type))),
    type = droplevels(interaction(!!! type_variables))
  ) %>%
  group_by(game_id, !!! group_variables, type, cohortday) %>%
  summarise(
    installs = sum(installs),
    dau = sum(dau),
    rpdau = sum(rpdau),
    rpdau_5 = sum(rpdau_5),
    rpdau_20 = sum(rpdau_20),
    rpdau_100 = sum(rpdau_100),
    payers = sum(payers),
    payers_5 = sum(payers_5),
    payers_20 = sum(payers_20),
    payers_100 = sum(payers_100),
    revenue = sum(revenue),
    revenue_npu = sum(revenue_npu),
    revenue_rpdau = sum(revenue_rpdau),
    arpu = revenue / installs,
    retention = dau / installs,
    rpdau_ret = rpdau / installs,
    arpdau = revenue / dau,
    arppu = revenue / payers,
    conversion = payers / dau,
    arprpdau = revenue_rpdau / rpdau,
    rpdau_revshare = revenue_rpdau / revenue,
    rpdau_20_ret_installs = rpdau_20 / installs,
    rpdau_100_ret_installs = rpdau_100 / installs,
    step_5_conversion = payers_5 / installs,
    step_20_conversion = payers_20 / installs,
    step_100_conversion = payers_100 / installs
  ) %>%
  group_by(game_id, !!! group_variables) %>%
  arrange(cohortday) %>%
  mutate(
    ltv_5_users = cumsum(payers_5),
    ltv_20_users = cumsum(payers_20),
    ltv_100_users = cumsum(payers_100),
    cumrev = cumsum(revenue),
    cumrev_npu = cumsum(revenue_npu),
    cumrev_rpdau = cumsum(revenue_rpdau),
    cumarpu = cumsum(arpu),
    ltv_5_conversion = cumsum(step_5_conversion),
    ltv_20_conversion = cumsum(step_20_conversion),
    ltv_100_conversion = cumsum(step_100_conversion)
  ) %>%
  ungroup() %>%
  mutate(
    rpdau_5_ret = rpdau_5 / ltv_5_users,
    rpdau_20_ret = rpdau_20 / ltv_20_users,
    rpdau_100_ret = rpdau_100 / ltv_100_users
  ) %>%
  arrange(game_id, !!! group_variables, cohortday) %>%
  select(game_id, !!! type_variables, type, cohortday, everything())

# saveRDS(cohortdata, 'ms_niso_beta_cohortdata_noplatform.RData')
# saveRDS(cohortdata, 'miso_beta_cohortdata.RData')

saveRDS(cohortdata, 'miso_beta_rpdau_arpu_adjusted_organic_1_revenue_modeling_cohortday_noplatform_weeks.RData')

# olddata <- readRDS('ms_niso_beta_cohortdata.RData')
olddata <- readRDS('ms_niso_beta_cohortdata_noplatform.RData')
nisodata <- filter(olddata, game_id == 23)
# misodata <- readRDS('miso_beta_cohortdata.RData')
misodata <- readRDS('miso_beta_cohortdata_noplatform.RData')

misodata <- misodata %>%
  filter(installs > 100)

metric_vars <- quos(rpdau_ret, arpu, cumarpu)

joindata <- misodata %>%
  select(!!! group_variables, type, cohortday, installs, !!! metric_vars) %>%
  rename(miso_installs = installs, miso_rpdau_ret = rpdau_ret, miso_arpu = arpu, miso_cumarpu = cumarpu) %>%
  left_join(select(nisodata, !!! group_variables, type, cohortday, installs, !!! metric_vars), by=c("campaign_type", "type", 'cohortday')) %>%
  rename(niso_installs = installs, niso_rpdau_ret = rpdau_ret, niso_arpu = arpu, niso_cumarpu = cumarpu) %>%
  mutate(
    rpdau_ret_ratio = miso_rpdau_ret / niso_rpdau_ret,
    arpu_ratio = miso_arpu / niso_arpu,
    cumarpu_ratio = miso_cumarpu / niso_cumarpu
  )

ggplot(joindata, aes(cohortday, rpdau_ret_ratio, color=type, group=type)) + geom_line() +
  # coord_cartesian(ylim=c(0, 5)) +
  geom_hline(data=ratiodata, aes(yintercept=rpdau_ret_ratio, color=type)) +
  geom_text(data=ratiodata, aes(x=runif(3, 10, 30), y=rpdau_ret_ratio+.05, color=type, label=round(rpdau_ret_ratio, 2)), size=5, show.legend=FALSE) +
  # geom_text(aes(x=5, y=2.13+.1, label=round(2.13, 2), color='organic'), show.legend = FALSE) +
  # geom_text(aes(x=20, y=0.324+.5, label=round(0.324, 2), color='nonincent'), show.legend = FALSE) +
  # geom_text(aes(x=18, y=0.258-.15, label=round(0.258, 2), color='incent'), show.legend = FALSE) +
  labs(title='RPDAU Retention Ratio MISO/NISO')

ratiodata <- joindata %>%
  group_by(type) %>%
  summarise(
    rpdau_ret_ratio = weighted.mean(rpdau_ret_ratio, miso_installs),
    arpu_ratio = weighted.mean(arpu_ratio, miso_installs),
    cumarpu_ratio = weighted.mean(cumarpu_ratio, miso_installs)
  )

ratiodata$arpu_ratio[which(ratiodata$type == 'organic')] <- 1

cohortdata <- bind_rows(olddata, misodata) %>%
  mutate(
    game_id = as.factor(game_id)
  )

cohortdata <- cohortdata %>%
  left_join(ratiodata, by='type') %>%
  mutate(
    rpdau_ret = rpdau_ret * rpdau_ret_ratio,
    arpu = arpu * arpu_ratio
  )

# saveRDS(cohortdata, 'ratio_adjusted_NISO_MISO.RData')
cohortdata <- readRDS('ratio_adjusted_NISO_MISO.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday.RData')
cohortdata <- readRDS('revenue_modeling_cohortday.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_noplatform.RData')
# cohort_noplatform <- readRDS('revenue_modeling_cohortday_noplatform.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_weeks.RData')
# cohortdata <- readRDS('revenue_modeling_cohortday_weeks.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_noplatform_weeks.RData')
# cohort_noplatform <- readRDS('revenue_modeling_cohortday_noplatform_weeks.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_months.RData')
# cohortdata <- readRDS('revenue_modeling_cohortday_months.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_noplatform_months.RData')
# cohort_noplatform <- readRDS('revenue_modeling_cohortday_noplatform_months.RData')

# saveRDS(cohortdata, 'revenue_modeling_cohortday_june_aug_2018.RData')

meltdata <- melt(cohort_noplatform, id.vars=c('cohortday', sapply(group_variables, quo_name), 'type'))
meltdata <- melt(cohortdata, id.vars=c('game_id', 'cohortday', sapply(type_variables, quo_name), 'type'))

ggplot(filter(cohortdata, cohortday < 40), aes(cohortday, rpdau_ret, group=interaction(type, game_id), color=type, linetype=game_id)) +
  geom_line() +
  scale_linetype_manual(values=c('dotted', 'dashed', 'solid')) +
  labs(title='RPDAU Retention Beta Performance')

plotdata <- cohortdata
plotdata <- filter(plotdata, cohortday == 270)
plotdata <- mutate(plotdata, !! quo_name(time_variable) := as.factor(!! time_variable))

ggplot(plotdata, aes(cohortday, cumarpu, group=!! time_variable, color=!! time_variable)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  labs(title = 'CUMARPU by Install Week')

ggplot(plotdata, aes(!!time_variable, cumarpu, group=type)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  labs(title = 'CUMARPU by Install Week')

ggplot(plotdata, aes(cohortday, retention, group=install_group, color=install_group)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  labs(title = 'Retention by Install Group', subtitle = 'Install Group in Millions')

for(i in 1:4){
  plotdata <- meltdata %>%
    filter(cohortday <= 300, country_tier == i) %>%
    filter(!variable %in% sapply(quos(dau, payers, payers_5, payers_20, payers_100, revenue, revenue_npu, revenue_rpdau, ltv_5_users, ltv_20_users, ltv_100_users), quo_name))
  p1 <- ggplot(plotdata, aes(cohortday, value, group=type, color=type)) +
    geom_line() +
    facet_wrap(~variable, scales='free_y') +
    labs(title = paste0('Tier ', i, ' Countries'))
  ggplot2::ggsave(paste0('tier_', i, '_dx_month_2_4.png'),
                  path = '../Graphs',
                  plot = p1, # or give ggplot object name as in myPlot,
                  width = 16, height = 9,
                  units = "in", # other options c("in", "cm", "mm"),
                  dpi = 'retina')
}




# spenddata <- read.csv('revenue_modeling_spend_daily_pull.csv')
#
# sapply(spenddata, class)
# summary(spenddata)
# spenddata <- spenddata %>%
#   mutate(
#     game_id = as.factor(game_id),
#     install_date = as.Date(install_event_date),
#     dup_account = as.factor(dup_account),
#     month = as.factor(format(install_date, '%Y-%m')),
#     dup_account = as.factor(ifelse(dup_account == 1, 'dup', 'non-dup'))
#   ) %>%
#   select(-c(game_id, install_event_date)) %>%
#   select(install_date, month, everything()) %>%
#   dplyr::filter(
#     install_date >= as.Date('2017-6-28'),
#     install_date < max(install_date, na.rm=TRUE) - 7
#   ) %>%
#   mutate(
#     install_platform = factor(install_platform),
#     type = droplevels(interaction(campaign_type, install_platform, dup_account))
#   )
#
# saveRDS(spenddata, 'revenue_modeling_spend_daily_pull.RData')

# plotdata <- newdata %>%
#   mutate(
#     type = interaction(campaign_type, install_platform, dup_account),
#   ) %>%
#   group_by(month, type) %>%
#   summarise(
#     installs = sum(installs)
#   ) %>%
#   group_by(month) %>%
#   mutate(
#     install_percent = installs / sum(installs)
#   )
#
# ggplot(plotdata, aes(month, install_percent, group=type, color=type, fill=type)) +
#   geom_area() +
#   labs(title='June RPDAU Gain Explanation by Dup')


spenddata <- readRDS('revenue_modeling_spend_daily_pull.RData')

spenddata <- spenddata %>% filter(install_date >= as.Date('2017-7-1'))

newcpi <- c(4.06, 3.98, 2.39, 1.55, 1.57, 1.58, 1.60, 1.61, 1.63, 1.65, 1.66, 1.68)
g5spend <- c(19500, 19500, 9500, 6500, 2500, 2500, 2500, 2500, 2500, 2500, 2500, 2500) * 1000

installdata <- spenddata %>%
  group_by(install_date) %>%
  summarise(
    installs = sum(installs),
    total_spend = sum(total_spend),
    cpi = total_spend / installs
  )

newmonth_date <- seq(from=as.Date('2018-11-1'), by='month', length.out = length(unique(spenddata$month)))
# olddates <- head(unique(installdata$install_date), -3)
datediff <- as.numeric(as.Date('2018-11-1') - min(installdata$install_date))
# newdates <- seq(from=as.Date('2018-11-01'), by='day', length.out = nrow(installdata))
# newdates <- head(newdates, -3)


monthdata <- spenddata %>%
  group_by(month) %>%
  summarise(
    monthly_spend = sum(total_spend),
    monthly_cpi = monthly_spend / sum(installs),
    installs = sum(installs),
    install_date = min(install_date)
  ) %>%
  mutate(
    g5spend = g5spend,
    newcpi = newcpi,
    newmonth_date = newmonth_date,
    newmonth = as.factor(format(newmonth_date, '%Y-%m')),
    newcpi_newinstalls = g5spend / newcpi,
    oldcpi_newinstalls = g5spend / monthly_cpi
  )

# monthdata <- spenddata %>%
#   group_by(month) %>%
#   summarise(
#     monthly_spend = sum(total_spend),
#     monthly_cpi = monthly_spend / sum(installs)
#   ) %>%
#   mutate(
#     g5spend = g5spend,
#     spend_ratio = g5spend / monthly_spend
#   )

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
  left_join(select(monthdata, newmonth, g5spend, newcpi_newinstalls), by=c('newmonth')) %>%
  mutate(
    total_spend_new = total_spend_percent * g5spend,
    installs_new = install_percent * newcpi_newinstalls,
    month = newmonth
    # install_date = install_date + datediff,
    # month =  as.factor(format(install_date, '%Y-%m'))
  )

# newmonthdata <- newdata %>%
#   group_by(newmonth) %>%
#   summarise(
#     monthly_spend = sum(total_spend),
#     monthly_cpi = monthly_spend / sum(installs),
#     installs = sum(installs),
#     monthly_spend_new = sum(total_spend_new),
#     monthly_cpi_new = monthly_spend_new / sum(installs_new),
#     installs_new = sum(installs_new),
#     # install_date = min(install_date)
#   )

dates <- data.frame(date = unique(newdata$install_date))
alldata <- merge(newdata, dates) %>% filter(date >= install_date) %>%
  mutate(
    cohortday = as.numeric(date - install_date)
  ) %>%
  select(install_date, date, cohortday, month, install_platform, campaign_type, dup_account, installs_new) %>%
  left_join(cohortdata, by=c('cohortday', 'campaign_type', 'install_platform', 'dup_account')) %>%
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

ggplot(predictdata_month, aes(date, rpdau, fill=month)) +
  geom_area() +
  labs(title='RPDAU by Install Month')

predictdata_month_type <- alldata %>%
  mutate(
    revenue = arpu * installs_new,
    rpdau = rpdau_ret * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date, month, type) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
  ) %>%
  left_join(installdata, by='date')

ggplot(predictdata_month_type, aes(date, rpdau, fill=type)) +
  geom_area() +
  facet_wrap(~month, scales='free_y') +
  labs(title='RPDAU by Month and Type')


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
    rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01'))),
  ) %>%
  left_join(rpdau_delta, by= 'month') %>%
  select(
    month, spend, revenue, rev_rpdau_mid_arprpdau, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_churn, arpdau, arprpdau, installs, cpi
  )

write.csv(predictdata, 'g5_predict_newcpi_scenario.csv', row.names=FALSE)
write.csv(month_output, 'g5_predict_month_newcpi_scenario.csv', row.names=FALSE)


novdata <- filter(alldata, month=='2018-11', date < as.Date('2018-12-1'))
novdata <- novdata %>%
  mutate(
    revenue = arpu * installs_new,
    rpdau = rpdau_ret * installs_new,
    dau = retention * installs_new,
    payers = conversion * dau
  ) %>%
  group_by(date, type) %>%
  summarise(
    revenue = sum(revenue),
    rpdau = sum(rpdau),
    dau = sum(dau),
    payers = sum(payers)
  )

ggplot(novdata, aes(date, rpdau, group=type, color=type)) +
  geom_line()







months <- monthdata %>%
  mutate(
    month = paste0(month, '-01'),
    month_ts = as.POSIXct(month, format='%Y-%m-%d', tz='UTC'),
    month_ts = month_ts + months(17),
    month = as.factor(format(month_ts, '%Y-%m'))
  ) %>%
  slice(-n()) %>%
  pull(month)

month_output$month <- as.factor(months)




plotdata <- melt(spenddata, id.vars=c('month_ts', 'month', 'install_platform', 'campaign_type'))
ggplot(plotdata, aes(month_ts, value, group = interaction(install_platform, campaign_type), color=interaction(install_platform, campaign_type))) +
  geom_line() +
  facet_wrap(~variable, scales='free_y')

gamedata <- gamedata %>%
  group_by(dup_account, campaign_type) %>%
  arrange(session_daily_ts) %>%
  mutate(
    cum_installs = cumsum(installs),
    rpdau_per_install = rpdau / cum_installs,
    rev_per_install = total_revenue / cum_installs
  ) %>%
  arrange(dup_account, campaign_type, session_daily_ts)



cohortdatedata <- read.csv('revenue_modeling_cohortday_all_dates_pull.csv')
zerodata <- cohortdatedata %>% filter(cohortday == 0) %>% arrange(install_event_date, install_platform, campaign_type, dup_account)

# CPI Interpolation

spenddata <- readRDS('revenue_modeling_spend_daily_pull.RData')
spendfilter <- spenddata %>%
  filter(
    campaign_type != 'organic',
    install_date >= max(install_date) - 90,
    install_date <= max(install_date) - 2
  )

spenddaydata <- spendfilter %>%
  group_by(install_date) %>%
  summarise(
    installs = sum(installs),
    spend = sum(total_spend),
    cpi = spend / installs,
    monthly_spend = spend * 30 / 1e6
  ) %>%
  slice(1:(n()-2)) %>%
  filter(monthly_spend < 20) %>%
  filter(cpi < 9, cpi > 2) %>%
  filter(install_date >= today() - 180)

spenddaydata <- spendfilter %>%
  group_by(install_date, campaign_type, install_platform) %>%
  summarise(
    type = droplevels(interaction(campaign_type, install_platform)),
    installs = sum(installs),
    spend = sum(total_spend),
    cpi = spend / installs,
    monthly_spend = spend * 30 / 1e6
  ) %>%
  filter(
    !(campaign_type == 'incent' & cpi > 7),
    !(campaign_type == 'not-incent' & cpi > 24),
    # !(campaign_type == 'incent' & monthly_spend > 15)
  )

%>%
  slice(1:(n()-2)) %>%
  filter(monthly_spend < 20) %>%
  filter(cpi < 9, cpi > 2) %>%
  filter(install_date >= today() - 180)

ggplot(spenddaydata, aes(monthly_spend, cpi)) +
  geom_point() +
  facet_wrap(~type, scales = 'free') +
  stat_smooth(method = "lm", col = "red", fullrange=TRUE) +
  # scale_x_continuous(breaks = seq(0, 20, 2), limits=c(0, 20)) +
  # scale_y_continuous(limits=c(0, max(spenddaydata$cpi))) +
  labs(title = 'CPI Linear Interpolation', subtitle = 'RSE: 0.5497', caption='Last 6 Months of Spend/CPI Data')

ggplot(spenddaydata, aes(monthly_spend, cpi)) +
  geom_point() +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) +b, se=FALSE, col='red', fullrange=TRUE, start = list(a=1,b=1)) +
  # stat_smooth(method = "nls", formula = y ~ a * log(x) + b, col = "red", start = list(a=1,b=1), fullrange=TRUE) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits=c(0, 20)) +
  scale_y_continuous(limits=c(0, max(spenddaydata$cpi))) +
  labs(title = 'CPI Logarithmic Interpolation', subtitle = 'R-squared: 0.5417', caption='Last 6 Months of Spend/CPI Data')

ggplot(spenddaydata, aes(monthly_spend, cpi)) +
  geom_point() +
  stat_smooth(method = 'loess', col = "red", fullrange=TRUE) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits=c(0, 20)) +
  scale_y_continuous(limits=c(0, max(spenddaydata$cpi))) +
  labs(title = 'CPI Loess Interpolation', subtitle='RSE: 0.5413', caption='Last 6 Months of Spend/CPI Data')

type_index <- 2
name <- levels(spenddaydata$type)[type_index]
modeldata <- filter(spenddaydata, type==levels(type)[type_index], install_date >= max(install_date) - 90)

lm_cpi <- lm(cpi ~ monthly_spend, modeldata, weights=spend)
summary(lm_cpi)
saveRDS(lm_cpi, paste0(name, '_cpi_spend_log_model.RData'))
predict(lm_cpi, data.frame(monthly_spend = 2.5))

loess_cpi <- loess(cpi ~ monthly_spend, modeldata)
summary(loess_cpi)
saveRDS(loess_cpi, 'cpi_spend_loess_model.RData')
cpi <- predict(loess_cpi, spend_predict)

nls_cpi <- nls(cpi ~ a * log(monthly_spend) + b, modeldata, start=list(a=1, b=1))
summary(nls_cpi)

spend_predict <- data.frame(monthly_spend =  c(9.5, 9.5, 9.5, 6.5, 6.5, 6.5, 6.5, 4.5, 4.5, 4.5, 4.5, 4.5))
notincent_spend <- data.frame(monthly_spend = c(7500000, 7500000, 7500000, 4260000, 4250000, 4220000, 4180000, 2900000, 2870000, 2840000, 2810000, 2750000) / 1e6)
incent_spend <- data.frame(monthly_spend = c(2e+06, 2e+06, 2e+06, 2240000, 2250000, 2280000, 2320000, 1600000, 1630000, 1660000, 1690000, 1750000) / 1e6)

predict_cpi <- predict(lm_cpi, notincent_spend)
predict_cpi <- predict(lm_cpi, incent_spend)


incent_spend$cpi <- predict_cpi
notincent_spend$cpi <- predict_cpi

nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,
            printEval = FALSE, warnOnly = FALSE)

type_index <- 2
plotdata <- filter(spenddaydata, type==levels(type)[type_index], install_date >= max(install_date) - 90)
p <- ggplot(plotdata, aes(x = 1e6*monthly_spend/30, y = cpi)) +
  geom_point() +
  # facet_wrap(~type, scales = 'free_x') +
  stat_smooth(method='loess', aes(color='loess'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se=FALSE, fullrange=TRUE, method.args = list(start = list(a=1,b=1))) +
  stat_smooth(method = 'nls', formula = y ~ a*(1 - exp(b*x)) + c, aes(colour = 'exponential'), se=FALSE, fullrange=TRUE, method.args = list(start = list(a=1,b=1, c=0), control = list(maxiter = 500))) +
  # scale_x_continuous(breaks = seq(0, max(plotdata$monthly_spend), 2), limits=c(0, max(plotdata$monthly_spend))) +
  # scale_y_continuous(limits=c(0, max(plotdata$cpi))) +
  labs(title = 'Curve Fitting for CPI', x='Monthly Spend (Millions)', subtitle=levels(plotdata$type)[type_index])
p



# cpi as a function of installs

spendday <- spenddata %>%
  filter(
    install_date < as.Date("2018-7-18"),
    campaign_type != 'organic',
    !(campaign_type == 'incent' & total_spend > 2e5),
    !(campaign_type == 'incent' & cpi > 7.5),
    !(campaign_type == 'not-incent' & cpi > 40),
    !(campaign_type == 'not-incent' & install_platform == 'android' & cpi > 20)
  ) %>%
  group_by(install_date, type) %>%
  summarise(
    total_spend = sum(total_spend),
    installs = sum(installs),
    cpi = total_spend / installs
  ) %>%
  group_by(type) %>%
  arrange(install_date) %>%
  mutate(
    cum_installs = cumsum(installs)
  )

spendday <- spendday %>%
  filter(
    install_date < as.Date("2018-7-18"),
    campaign_type != 'organic',
    !(campaign_type == 'incent' & total_spend > 2e5),
    !(campaign_type == 'incent' & cpi > 7.5),
    !(campaign_type == 'not-incent' & cpi > 40),
    !(campaign_type == 'not-incent' & install_platform == 'android' & cpi > 20)
  )

ggplot(spendday, aes(cum_installs, cpi)) +
  geom_point() +
  facet_wrap(~type, scales='free') +
  stat_smooth(method='loess', aes(color='loess'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se=FALSE, fullrange=TRUE) +
  labs(title='Cum Installs vs. CPI')

ggplot(spendday, aes(total_spend, cpi)) +
  geom_point() +
  stat_smooth(method='loess', aes(color='loess'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se=FALSE, fullrange=TRUE) +
  facet_wrap(~type, scales='free') +
  labs(title='Spend vs. CPI')

plotdata <- melt(spendday, id.vars = c('install_date', 'type'))
ggplot(plotdata, aes(install_date, value)) +
  facet_grid(variable ~ type, scales='free_y') +
  geom_line()

lm_cpi <- lm(cpi ~ total_spend + cum_installs, spendday)
summary(lm_cpi)
# saveRDS(lm_cpi, 'cpi_spend_log_model.RData')
predict(lm_cpi, data.frame(monthly_spend = 2.5))

# variable importance on d30 cumarpu

d30data <- cohortdata %>% filter(cohortday == 30) %>% select(-installs)
d30data <- d30data %>%
  left_join(spenddata, by=c('install_date', sapply(type_variables, quo_name), 'type')) %>%
  select(install_date, date, cohortday, !!! type_variables, type, everything()) %>%
  group_by(type) %>%
  arrange(install_date) %>%
  mutate(
    cum_installs = cumsum(installs),
    cum_impressions = cumsum(impressions / 1e3),
    days_since_launch = install_date - min(install_date)
  ) %>%
  ungroup() %>%
  arrange(type, install_date) %>%
  filter(campaign_type != 'organic')

# variable importance

modeldata <- select(d30data,
    campaign_type, install_platform, days_since_launch, installs, cum_installs, total_spend, cpi, impressions, cum_impressions, retention, rpdau_ret, arpdau, arprpdau, cumarpu
    )
pred_vars <- sapply(quos(retention, rpdau_ret, arpdau, arprpdau, cumarpu), quo_name)
pred_var <- 'cumarpu'

ind_vars <- names(modeldata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_vars))

var_names <- paste(ind_vars, collapse=' + ')

data_name <- 'd30_cumarpu_pred'

ggplot(modeldata, aes(cpi, cumarpu)) +
  geom_line() +
  facet_wrap(~ interaction(campaign_type, install_platform), scales='free') +
  labs(title='CPI by Cumulative Installs')

# ggplot(modeldata, aes(days_since_launch, cumarpu)) +
#   geom_line() +
#   facet_wrap(~ interaction(campaign_type, install_platform), scales='free_y') +
#   labs(title='d30 CumARPI by Days Since Launch')

modeldata <- modeldata %>%
  filter(
    campaign_type == 'incent',
    install_platform == 'android',
    days_since_launch != 0,
    cpi < 40,
    !(campaign_type == 'not-incent' & install_platform == 'android' & (cpi > 20 | cpi < 2.5)),
    !(campaign_type == 'incent' & install_platform == 'android' & cpi > 5),
    !(campaign_type == 'incent' & install_platform == 'iOS' & cpi > 7)
  )

type <- 'Incent Android'

# imputedata <- mydata[,!(colnames(mydata) %in% c('trophy'))]
# aggr_plot <- aggr(imputedata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata),
#   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# installdata <- ddply(mydata, .(install_date), summarise,
#                      d3churn = mean(d3churn)
# )

model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))

# Partition into training, validation, testing set

traindata <- modeldata

# spec = c(treat = .2, train = .7, test = .09, validate = 0.01)
# spec = c(train = .9, test = .09, validate = .01)
spec = c(treat = .4, train = .6)

g = sample(cut(
  seq(nrow(modeldata)),
  nrow(modeldata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(modeldata, g)

treatdata <- res$treat
traindata <- res$train
validdata <- res$validate
testdata <- res$test

saveData(res, name='tow_d2ret_day0_95_parsed_labeleddata_v1', overwrite=TRUE)
saveData(traindata, name='tow_d2ret_day0_95_parsed_traindata_v1', overwrite=TRUE)
saveData(validdata, name='tow_d2ret_day0_95_parsed_validdata_v1', overwrite=TRUE)
saveData(testdata, name='tow_d2ret_day0_95_parsed_testdata_v1', overwrite=TRUE)

res <- loadData(name='tow_d2ret_day0_95_parsed_labeleddata_v1')
traindata <- loadData(name='tow_d2ret_day0_95_parsed_traindata_v1')
validdata <- loadData(name='tow_d2ret_day0_95_parsed_validdata_v1')
testdata <- loadData(name='tow_d2ret_day0_95_parsed_testdata_v1')


# TREAT DATA

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameNExperiment(traindata, varlist = ind_vars, outcomename = pred_var, #outcometarget = 1,
                               scale=TRUE,
                               rareCount=5,  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)
treatment_plan_factor <- cfe$treatments
sf <- treatment_plan_factor$scoreFrame
var_names <- sf$varName[sf$sig <= 1/nrow(sf)]
train.treat <- cfe$crossFrame

sf %>% arrange(sig)

PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame))
# use separate data to treat and train

treatment_plan_factor <- designTreatmentsN(treatdata, varlist = ind_vars, outcomename = pred_var,
                                        rareCount=5,  # Note set this to something larger, like 5
                                        rareSig=.3 # Note set this to something like 0.3
)

treatment_name <- paste0(data_name, '_treatment_plan.RData')
saveRDS(treatment_plan_factor, treatment_name)
treatment_plan_factor <- readRDS(treatment_name)

# treat data

train.treat <- prepare(treatment_plan_factor, traindata, scale=TRUE, pruneSig=PRUNE_SIG)
var_names <- setdiff(colnames(train.treat), c(pred_vars))

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))

#
# # treat validation and testing set
#
# valid.treat <- vtreat::prepare(treatment_plan_factor, validdata, scale=TRUE, pruneSig=PRUNE_SIG)
# test.treat <- vtreat::prepare(treatment_plan_factor, testdata, scale=TRUE, pruneSig=PRUNE_SIG)
# train.treat[,pred_var_num] <- traindata[,pred_var_num]
# valid.treat[,pred_var_num] <- validdata[,pred_var_num]
# test.treat[,pred_var_num] <- testdata[,pred_var_num]

fit.lm <- lm(factor_formula, data=train.treat)
summary(fit.lm)




# GBM

model_name <- paste(data_name, 'gbm', 'v1.RData', sep='_')

n.trees = 4000
gbm1 <- gbm(factor_formula, data=train.treat, distribution='gaussian', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
best.iter <- gbm.perf(gbm1, plot.it=FALSE)
if(gbm1$n.trees < best.iter*1.2){
  gbm1 <- gbm.more(gbm1, n.new.trees=(best.iter*1.2 - gbm1$n.trees))
  best.iter <- gbm.perf(gbm1, plot.it=FALSE)
}
# gbm1 <- gbm.more(gbm1, n.new.trees=1000)
# saveRDS(gbm1, file=model_name)
# gbm1 <- readRDS(model_name)

plot(gbm1, i.var=12)

gbm.perf(gbm1)
best.iter <- gbm.perf(gbm1, method='test', plot.it=TRUE)
best.iter
# par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance', plotit=TRUE)
var_imp <- subset(var_imp, rel.inf != 0)
var_imp <- var_imp %>% arrange(desc(rel.inf))
var_imp$var <- factor(var_imp$var, levels=rev(var_imp$var))
ggplot(var_imp, aes(var, rel.inf)) +
  geom_bar(stat='identity', fill='blue') +
  coord_flip() +
  ggtitle('GBM Variable Importance on d30 CumARPI') +
  labs(x ='Variable', y = 'Relative Influence', subtitle=type) +
  theme(plot.title = element_text(hjust=.5), plot.subtitle=element_text(hjust=.5))

ggplot2::ggsave(paste0(model_name, '.png'),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# ICE MODELING

dev.off()

var_orig = 'days_since_launch'
var_clean = paste0(var_orig, '_clean')

installs_treat <- data.frame(orig=pull(traindata, !! enquo(var_orig)), scaled=train.treat[,var_clean])
installs_lm <- lm(scaled ~ orig, installs_treat)
installs_treat$lm <- installs_lm$coefficients['(Intercept)'] + installs_treat$orig * installs_lm$coefficients['orig']
all(installs_treat$lm - installs_treat$scaled < .000001 | is.na(installs_treat$orig))

# ice_lr = ice(object = gbm1, X = train.treat[,var_names], predictor = var_clean,log.odds=TRUE, frac_to_build = .01,
#              predictfcn = function(object, newdata){
#                predict(object, newdata, proba=TRUE)[[2]][,1]
#              })
ice_gbm = ice(object = gbm1, X = train.treat[,var_names], predictor = var_clean,log.odds=TRUE, frac_to_build = 1,
              predictfcn = function(object, newdata){
                predict(object, newdata, n.trees=best.iter, type='response')
              })
# ice_lm = ice(object = fit.lm, X = train.treat[,var_names], predictor = var_clean,log.odds=TRUE, frac_to_build = 1)
ice_scaled <- ice_gbm
# plot(ice_gbm)


low <- quantile(train.treat[,var_clean], seq(0,1,.01))['5%']
middle <- median(train.treat[,var_clean])
high <- quantile(train.treat[,var_clean], seq(0,1,.01))['95%']
low_gridpt_index <- which(abs(ice_scaled$gridpts-low)==min(abs(ice_scaled$gridpts-low)))
middle_gridpt_index <- which(abs(ice_scaled$gridpts-middle)==min(abs(ice_scaled$gridpts-middle)))
high_gridpt_index <- which(abs(ice_scaled$gridpts-high)==min(abs(ice_scaled$gridpts-high)))

ice_scaled$gridpts <- (ice_scaled$gridpts - installs_lm$coefficients['(Intercept)']) / installs_lm$coefficients['orig']
ice_scaled$xj <- (ice_scaled$xj - installs_lm$coefficients['(Intercept)']) / installs_lm$coefficients['orig']
if(installs_lm$coefficients['orig'] < 0){
  ice_scaled$gridpts <- sort(ice_scaled$gridpts)
  ice_scaled$xj <- sort(ice_scaled$xj)
  ice_scaled$actual_prediction <- rev(ice_scaled$actual_prediction)
  ice_scaled$ice_curves <- ice_scaled$ice_curves[,ncol(ice_scaled$ice_curves):1]
  colnames(ice_scaled$ice_curves) <- ice_scaled$gridpts
  ice_scaled$pdp <- rev(ice_scaled$pdp)
}
ice_scaled$xlab <- var_orig

average.lm <- lm(y ~ x, data.frame(x=ice_scaled$gridpts, y=ice_scaled$pdp))
summary(average.lm)

x <- data.frame(x=ice_scaled$gridpts, y=unname(ice_scaled$pdp))

plot(ice_scaled, frac_to_plot = .5, plot_orig_pts_preds = TRUE, main=paste0('Blended ICE Curves for ', var_orig, ' for ', type), ylab=pred_var)
mtext(paste0('Slope: ', round(average.lm$coefficients['x'], 5)))
abline(average.lm, col='red', lwd=5)
# axis(side=1, at=seq(0, 1, .05), labels=seq(0, 1, .05))

# sign_num <- ifelse(installs_lm$coefficients['orig'] < 0, -1, 1)
# if(installs_lm$coefficients['orig'] < 0){
#   ice_scaled$gridpts <- sort(ice_scaled$gridpts)
#   ice_scaled$xj <- sort(ice_scaled$xj)
# }
dice_scaled <- dice(ice_scaled)
# dice_scaled$dpdp <- sign_num * dice_scaled$dpdp
# dice_scaled$d_ice_curves <- sign_num * dice_scaled$d_ice_curves
# dice_scaled$actual_deriv <- sign_num * dice_scaled$actual_deriv
# dice_scaled$gridpts <- (dice_scaled$gridpts - installs_lm$coefficients['(Intercept)']) / installs_lm$coefficients['orig']
# dice_scaled$xj <- (dice_scaled$xj - installs_lm$coefficients['(Intercept)']) / installs_lm$coefficients['orig']
dice_scaled$xlab <- var_orig
plot(dice_scaled, frac_to_plot = .5, main=paste0('DICE Curves for ', var_orig, ' for ', type), plot_sd=FALSE)
