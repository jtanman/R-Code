# By: James Tan

# Date: 7/3/2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)
library(RVAideMemoire)
library(markovchain)

source('~/.Rprofile')
setwd(datapath)

dldata <- read.csv('revenue_modeling_cohortday_all_dates_extrafields_pull_beta_zeus_australia.csv')

type_variables <- quos(campaign_type)

summary(dldata)
mydata <- dldata %>%
  filter(install_platform == 'iOS' | install_platform == 'android') %>%
  rename(install_date = install_event_date) %>%
  mutate(
    game_id = as.factor(game_id),
    install_date = as.Date(install_date),
    install_month = as.factor(format(install_date, '%Y-%m')),
    date = as.Date(date),
    # install_platform = as.factor(ifelse(install_platform == 1, 'iOS', ifelse(install_platform == 2, 'android', 'Other'))),
    # dup_account = as.factor(ifelse(dup_account == 0, 'dup', 'non-dup')),
    install_platform = droplevels(install_platform),
    # dup_account = as.factor(dup_account),
    # country_tier = as.factor(country_tier),
    type = droplevels(interaction(!!! type_variables)),
    arpu = revenue / installs,
    retention = dau / installs,
    rpdau_ret = rpdau / installs,
    arpdau = revenue / dau,
    arppu = revenue / payers,
    conversion = payers / dau,
    streak_conversion = payers_streak / dau,
    arprpdau = revenue_rpdau / rpdau,
    rpdau_revshare = revenue_rpdau / revenue,
    rpdau_20_ret_installs = rpdau_20 / installs,
    rpdau_100_ret_installs = rpdau_100 / installs,
    step_5_conversion = payers_5 / installs,
    step_20_conversion = payers_20 / installs,
    step_100_conversion = payers_100 / installs,
    streak_revshare = revenue_streak / revenue,
    streak_arpdau = revenue_streak / payers_streak,
    unique_arpdau = revenue_unique / payers_unique,
  ) %>%
  filter(date <= as.Date("2018-10-13")) %>%
  group_by(install_date, game_id, !!! type_variables) %>%
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
  arrange(game_id, install_date, type, cohortday)

monnb <- function(d){
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
}

mydata <- mydata %>%
  mutate(
    days_since_launch = as.numeric(install_date - min(install_date)),
    weeks_since_launch = as.numeric(floor(difftime(install_date, min(install_date), units='weeks'))),
    months_since_launch = monnb(install_date) - monnb(min(install_date))
  )

misodata <- mydata

saveRDS(mydata, 'revenue_modeling_cohortday_all_dates_beta_zeus.RData')
mydata <- readRDS('revenue_modeling_cohortday_all_dates_beta.RData')



ltvdata <- mydata %>%
  filter(type == 'organic.iOS.non-dup') %>%
  select(install_date, date, cohortday, type, installs, dau, rpdau, payers, ltv_5_users, ltv_20_users, ltv_100_users, rpdau_5_ret, rpdau_20_ret, rpdau_100_ret_installs, everything())

cohortday_start <- 2
month.dist <- function(monthdata){
  distdata <- dcast(monthdata, cohortday ~ install_month, mean, value.var = 'cumarpu_mult')
  distdata <- distdata %>% select(-cohortday)
  dist.matrix <- dist(t(distdata))
  return(mean(dist.matrix))
}

distdata <- data.frame(matrix(ncol=6, nrow=0))
names(distdata) <- c("campaign_type", "install_platform", "dup_account", "type",
                     "dist", "cohortday_start")

# saveRDS(alldata, 'cohortdata_all.RData')
alldata <- readRDS('cohortdata_all.RData')
meltdata <- melt(alldata, id.vars=c('cohortday', 'campaign_type', 'install_platform', 'dup_account', 'type'))
ggplot()

# group_variables <- quos(install_month, campaign_type, install_platform, dup_account)
group_variables <- c(quo(game_id), type_variables, quo(type))

for(i in 0:30){
  cohortday_start <- i
  cohortday_start <- 0
  monthdata <- mydata %>%
    # filter(date >= max(mydata$date) - 60) %>%
    # filter(install_date <= min(install_date) + 6) %>%
    group_by(cohortday, !!! group_variables) %>%
    summarise(
      n = n(),
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
      ltv_5_users = sum(ltv_5_users),
      ltv_20_users = sum(ltv_20_users),
      ltv_100_users = sum(ltv_100_users),
      revenue = sum(revenue),
      revenue_npu = sum(revenue_npu),
      revenue_rpdau = sum(revenue_rpdau),
      cumrev = sum(cumrev),
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
      step_100_conversion = payers_100 / installs,
      rpdau_5_ret = rpdau_5 / ltv_5_users,
      rpdau_20_ret = rpdau_20 / ltv_20_users,
      rpdau_100_ret = rpdau_100 / ltv_100_users
    ) %>%
    group_by(!!! group_variables) %>%
    arrange(cohortday) %>%
    mutate(
      cumarpu = cumsum(arpu),
      ltv_5_conversion = cumsum(step_5_conversion),
      ltv_20_conversion = cumsum(step_20_conversion),
      ltv_100_conversion = cumsum(step_100_conversion),
      cumarpu_growth = cumarpu / lag(cumarpu, n=1),
      cumarpu_growth = ifelse(cohortday <= cohortday_start, 1, cumarpu_growth),
      cumarpu_mult = cumprod(cumarpu_growth),
      cumarpu_mult = ifelse(cohortday < cohortday_start, NA, cumarpu_mult)
    ) %>%
    ungroup() %>%
    arrange(!!! group_variables, cohortday)

  distdata_temp <- monthdata %>%
    dplyr::filter(!install_month %in% c(levels(install_month)[1], levels(install_month)[length(levels(install_month))])) %>%
    dplyr::group_by(campaign_type, install_platform, dup_account, type) %>%
    do(
      data.frame(dist = month.dist(.))
    ) %>%
    ungroup() %>%
    mutate(cohortday_start = cohortday_start)

  distdata <- rbind(distdata, distdata_temp)
}

# Distance work

rm(distdata_temp)
saveRDS(distdata, 'cohortstart_distdata.RData')
distdata <- readRDS('cohortstart_distdata.RData')

ggplot(distdata, aes(cohortday_start, dist, group=type, color=type)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0,30,2)) +
  labs(title='Pairwise Distance b/w Install Month Curves', caption='Using manhattan distance')

# testdata <- filter(monthdata, type == levels(type)[1])

distdata <- monthdata %>%
  dplyr::filter(!install_month %in% c(levels(install_month)[1], levels(install_month)[length(levels(install_month))])) %>%
  dplyr::group_by(campaign_type, install_platform, dup_account, type) %>%
  do(dist = month.dist(.))

meltdata <- melt(monthdata, id.vars=c('install_month', 'cohortday', 'campaign_type', 'install_platform', 'dup_account', 'type'))
cumarpudata <- filter(meltdata, variable == 'cumarpu_mult')

plotdata <- filter(meltdata, variable == 'cumarpu_mult')
ggplot(plotdata, aes(cohortday, value, group=install_month, color=install_month)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  labs(title = 'CumARPI Percentage Growth by Install Month and Type', y = 'CumARPI Growth')

cumarpudata <- dcast(cumarpudata, type + cohortday ~ install_month, mean)
distdata <- testdata[,3:ncol(testdata)]
cumarpu.dist <- dist(t(distdata), method = 'manhattan')


testdata <- filter(cumarpudata, type == levels(type)[7]) %>% select('2017-08', '2017-09') %>% filter(complete.cases(.))

CvM.test(testdata[,1], testdata[,2])

ggplot(mydata, aes(cohortday, rpdau_ret, group=type, color=type)) + geom_line()

plotdata <- filter(meltdata, cohortday <= 300)
ggplot(plotdata, aes(cohortday, value, group=type, color=type)) +
  geom_line() +
  facet_wrap(~variable, scales='free_y')

# FFXV Modeling
lower_limit <- 240
ggplot(filter(monthdata, cohortday >= lower_limit, cohortday <= max(cohortday)), aes(cohortday, cumarpu_growth)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  stat_smooth(method='loess', aes(color='loess'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', aes(colour = 'linear'), se=FALSE, fullrange=TRUE) +
  # stat_smooth(method = 'lm', formula = log(y) ~ x, aes(colour = 'exp_linear'), se=FALSE, fullrange=TRUE) +
  # stat_smooth(method = 'lm', formula = y ~ log(x), aes(colour = 'log_linear'), se=FALSE, fullrange=TRUE) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), aes(colour = 'polynomial'), se=FALSE, fullrange=TRUE) +
  # stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se=FALSE, fullrange=TRUE, method.args = list(start = list(a=-1, b=1), control = list(maxiter = 10000, minFactor=1/1e6))) +
  stat_smooth(method = 'nls', formula = y ~ a * log(x) + b, aes(colour = 'logarithmic'), se=FALSE, fullrange=TRUE, method.args = list(start = list(a=-1, b=1), control = list(maxiter = 10000, minFactor=1/1e6))) +
  stat_smooth(method = 'nls', formula = y ~ c*exp(-b*x), aes(colour = 'exponential decay'), se=FALSE, fullrange=TRUE, method.args = list(start = list(c=1, b=1/1000), control = list(maxiter = 10000, minFactor=1/1e6))) +
  # stat_smooth(method = 'nls', formula = y ~ -c*exp(b*(x)), aes(colour = 'exponential growth'), se=FALSE, fullrange=TRUE, method.args = list(start = list(c=1,b=1/10000), control = list(maxiter = 10000, minFactor=1/1e6))) +
  scale_x_continuous(limits=c(lower_limit,720)) +
  scale_y_continuous(limits=c(1, 1.004))

pick_type <- levels(monthdata$type)[5]
testdata <- filter(monthdata, type == pick_type, cohortday >= 240, cohortday <= max(cohortday))

ggplot(testdata, aes(cohortday, cumarpu_growth)) + geom_line() + labs(subtitle=pick_type)
test_lm <- lm(cumarpu_growth ~ cohortday, testdata)
test_nls <- nls(cumarpu_growth ~ c* exp(b*cohortday), data=testdata, start = list(c=-1,b=1/10), control = list(maxiter = 10000, minFactor=1/1e6))
summary(test_nls)

preddata <- data.frame(cohortday = seq(240, 1e4))
preddata$fitted_exp <- predict(test_nls, preddata)
preddata$fitted_lm <- predict(test_lm, preddata)

ggplot(preddata, aes(cohortday)) +
  geom_line(aes(cohortday, fitted_exp, color='fitted_exp')) +
  geom_line(aes(cohortday, fitted_lm, color='fitted_lm'))

ggplot(testdata, aes(cohortday, cumarpu_growth)) + geom_line() +
  geom_line(aes(cohortday, fitted_values, color='fitted')) +
  geom_line(aes(cohortday, fitted_values_lm, color='fitted_lm'))

currentdata <- filter(mydata, date == max(date)-2)

getCumarpuGrowth <- function(df, cohortdays){
  # browser()
  predict_df <- data.frame(cohortday = cohortdays)

  lm.group <- lm(cumarpu_growth ~ cohortday, df)

  log.group <- lm(cumarpu_growth ~ log(cohortday), df)

  # nls.group <- nls(cumarpu_growth ~ a*log(cohortday) + b, df, start=list(a=-1, b=1))

  cumarpu_growth_lm <- predict(lm.group, predict_df)
  cumarpu_growth_log <- predict(log.group, predict_df)
  return(list('cumarpu_growth_lm' = cumarpu_growth_lm, 'cumarpu_growth_log' = cumarpu_growth_log))
}

oldcurvedata <- monthdata %>%
  select(!!! group_variables, cohortday, cumarpu_growth) %>%
  filter(cohortday <= 360)

newcurvedata <- monthdata %>%
  select(!!! group_variables, cohortday, cumarpu_growth) %>%
  filter(cohortday >= 240, cohortday <= 360) %>%
  group_by(!!! group_variables) %>%
  do(
    data.frame(
      cohortday = seq(361, 916, by=1),
      cumarpu_growth_lm = getCumarpuGrowth(., seq(361, 916, by=1))[[1]],
      cumarpu_growth_log = getCumarpuGrowth(., seq(361, 916, by=1))[[2]]
    )
  ) %>% ungroup() %>%
  mutate(
    cumarpu_growth_lm = ifelse(cumarpu_growth_lm < 1, 1, cumarpu_growth_lm),
    cumarpu_growth_log = ifelse(cumarpu_growth_log < 1, 1, cumarpu_growth_log)
  )

curvedata <- newcurvedata %>%
  rename(cumarpu_growth = cumarpu_growth_lm) %>%
  select(-cumarpu_growth_log)

curvedata <- rbind(oldcurvedata, curvedata)
curvedata <- curvedata %>%
  mutate(
    cumarpu_growth = ifelse(cumarpu_growth < 1, 1, cumarpu_growth)
  )
ggplot(filter(curvedata, cohortday >= 240), aes(cohortday, cumarpu_growth)) +
  geom_line() +
  facet_wrap(~type, scales='free_y')

ggplot(filter(oldcurvedata, cohortday >= 240), aes(cohortday, cumarpu_growth)) +
  geom_line() +
  geom_line(data=newcurvedata, aes(cohortday, cumarpu_growth_lm, color='lm')) +
  geom_line(data=newcurvedata, aes(cohortday, cumarpu_growth_log, color='log')) +
  scale_y_continuous(limits=c(1,1.005)) +
  facet_wrap(~type, scales='free_y')

# new install modeling

spenddata <- readRDS('revenue_modeling_spend_daily_pull.RData')

group_variables <- quos(campaign_type, install_platform, dup_account, type)

blenddata <- spenddata %>%
  filter(!install_date == max(install_date)) %>%
  filter(install_date >= max(install_date) - 90) %>%
  group_by(!!! group_variables) %>%
  summarise(
    installs = sum(installs),
    total_spend = sum(total_spend),
    cpi = total_spend / installs
  ) %>%
  ungroup() %>%
  mutate(
    install_percent = installs / sum(installs),
    spend_percent = total_spend / sum(total_spend)
  )

ggplot(spenddata, aes(install_date, total_spend)) +
  geom_line() +
  facet_wrap(~type, scales='free_y')

aug_spend <- spenddata %>%
  filter(install_date >= as.Date('2018-7-1')) %>%
  group_by(!!! group_variables) %>%
  summarise(
    spend = mean(total_spend),
    installs = mean(installs),
    cpi = spend / installs,
    k = 1
  )

monthly_install <- blenddata %>%
  mutate(
    spend_factor = 5e6 / sum(total_spend),
    installs = spend_factor * installs,
    total_spend = spend_factor * total_spend
  ) %>%
  select(campaign_type, install_platform, dup_account, type, total_spend, cpi, installs)

get_cpi <- function(data){
  cpi_model <- readRDS(paste0(data$type, '_cpi_spend_log_model.RData'))
  cpi_pred <- predict(cpi_model, data.frame(monthly_spend = data$total_spend))
  return(cpi_pred)
}

cpi_data <- monthly_install %>%
  filter(campaign_type != 'organic') %>%
  mutate(
    type = droplevels(as.factor(interaction(campaign_type, install_platform))),
    total_spend = total_spend / 1e6
  ) %>%
  group_by(campaign_type, install_platform, type, total_spend, cpi) %>%
  do(
    data.frame(cpi_pred = get_cpi(.))
  ) %>%
  ungroup() %>%
  mutate(
    total_spend = total_spend * 1e6,
    installs = total_spend / cpi_pred
  )

new_install <- monthly_install %>%
  left_join(cpi_data, by=c('campaign_type', 'install_platform')) %>%
  mutate(
    type = type.x,
    installs = ifelse(campaign_type == 'organic', installs.x, installs.y),
    spend = ifelse(campaign_type == 'organic', 0, total_spend.x),
    cpi = ifelse(campaign_type == 'organic', 0, cpi_pred),
    k = 1
  ) %>%
  select(campaign_type, install_platform, dup_account, type, spend, cpi, installs, k)

max_date <- max(currentdata$date)

dates <- seq(max_date + 1, as.Date('2018-9-1') - 1, by='day')
augdata <- tibble(install_date = dates) %>%
  mutate(
    month = as.factor(format(install_date, format='%Y-%m')),
    k = 1
  ) %>%
  inner_join(aug_spend, by='k') %>%
  select(-k)

dates <- seq(as.Date('2018-9-1'), as.Date('2018-10-1') - 1, by='day')
name <- 'sep_5'

newdata <- tibble(install_date = dates) %>%
  mutate(
    month = as.factor(format(install_date, format='%Y-%m'))
  ) %>%
  group_by(month) %>%
  mutate(
    n = n(),
    k = 1
  ) %>%
  inner_join(new_install, by='k') %>%
  mutate(
    spend = spend / n,
    installs = installs / n
  ) %>%
  select(-c(k, n)) %>%
  ungroup()

newdata <- rbind(augdata, newdata) %>%
  arrange(!!!group_variables, install_date)
newdata <- augdata

# cohort_simulation <- mydata %>%
#   filter(install_date >= as.Date("2018-6-1"), date <= max(mydata$date) - 1) %>%
#   group_by(!!! group_variables, install_date) %>%
#   filter(date == max(date)) %>%
#   select(-date)

dates <- data.frame(date = seq(min(newdata$install_date), as.Date('2020-1-1') - 1, by='day'))

cohortdata <- readRDS('revenue_modeling_cohortday_june_aug_2018.RData')
cohortdata <- cohortdata %>% filter(cohortday <= 75)
cohort_simulation <- curvedata %>%
  left_join(cohortdata, by=c(sapply(group_variables, quo_name), 'cohortday')) %>%
  select(!!! group_variables, cohortday, cumarpu_growth, cumarpu) %>%
  arrange(!!! group_variables, cohortday) %>%
  group_by(!!! group_variables) %>%
  mutate(
    cumarpu_growth_total = ifelse(is.na(cumarpu), cumarpu_growth, 1),
    cumarpu_growth_total = cumprod(cumarpu_growth_total),
    cumarpu_flat = ifelse(!is.na(cumarpu), cumarpu, cumarpu[max(which(!is.na(cumarpu)))]),
    cumarpu_new = cumarpu_growth_total * cumarpu_flat
  ) %>%
  select(-c(cumarpu_growth_total, cumarpu_flat))


# while(any(is.na(cohort_simulation$cumarpu_serial))){
#   cohort_simulation <- cohort_simulation %>%
#     group_by(!!! group_variables) %>%
#     mutate(
#       cumarpu_serial = ifelse(is.na(cumarpu), lag(cumarpu_serial, n=1) * cumarpu_growth, cumarpu)
#     )
# }

ggplot(cohort_simulation, aes(cohortday, cumarpu_new, color=type)) +
  # geom_line(aes(cohortday, cumarpu_log, color=type)) +
  geom_line()


join_vars <- quos(cohortday, !!! group_variables)

newpred <- merge(newdata, dates) %>% filter(date >= install_date) %>%
  mutate(
    cohortday = as.numeric(date - install_date)
  ) %>%
  select(install_date, date, cohortday, month, !!! group_variables, installs) %>%
  left_join(cohort_simulation, by=sapply(join_vars, quo_name)) %>%
  # select(-c(installs, dau, rpdau, payers, revenue)) %>%
  arrange(!!!group_variables, install_date, cohortday) %>%
  mutate(
    cumrev = cumarpu_new * installs,
    rev = ifelse(cohortday == 0, cumrev, cumrev - lag(cumrev, 1)),
    install_month = month,
    month = as.factor(format(date, '%Y-%m')),
  ) %>% ungroup()

# existing users

summary(currentdata)
olduserdata <- currentdata %>%
  select(install_date, !!! group_variables, date, cohortday, installs, cumarpu)
olduserpred <- merge(select(olduserdata, -date), dates) %>%
  filter(date >= install_date) %>%
  mutate(
    cohortday = as.numeric(date - install_date)
  )

allolddata <- rbind(olduserdata, olduserpred) %>%
  mutate(cumarpu_new = NA) %>%
  arrange(!!! group_variables, install_date, cohortday)

oldpred <- allolddata %>%
  left_join(curvedata, by=sapply(join_vars, quo_name)) %>%
  group_by(!!! group_variables, install_date) %>%
  mutate(
    cumarpu_growth = ifelse(date == max_date, 1, cumarpu_growth),
    cumarpu_growth_total = cumprod(cumarpu_growth),
    cumarpu_new = cumarpu * cumarpu_growth_total,
    cumrev = cumarpu_new * installs,
    rev = cumrev - lag(cumrev, 1),
    install_month = as.factor(format(install_date, '%Y-%m')),
    month = as.factor(format(date, '%Y-%m')),
  ) %>%
  select(-cumarpu_growth_total) %>% ungroup()

testpred <- filter(oldpred, type == levels(type)[2])

plotdata <- filter(monthdata, cohortday > 300)
ggplot(plotdata, aes(cohortday, revenue)) +
  geom_line() +
  facet_wrap(~type, scales='free_y')

alldata <- rbind(oldpred, newpred)
alldata <- newpred

revdata <- alldata %>%
  group_by(date, month) %>%
  summarise(
    rev = sum(rev)
  )
revmonthdata <- revdata %>%
  group_by(month) %>%
  summarise(
    rev = sum(rev)
  )

write.csv(revmonthdata, paste0(name, '_new_user_rev.csv'), row.names=FALSE)
