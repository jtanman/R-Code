# By: James Tan

# Date: 12/11/2018

p_load(ggplot2, reshape2, plotly, lubridate, zoo, expm, dplyr, beepr, scales)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/generalized_markov_model_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')

game <- 'niso'

dldata <- read.csv(paste0('revenue_modeling_cohortday_all_dates_', game, '.csv'))
ccudata <- read_csv(sprintf('%s_ccu.csv', game))

start_date <- as.Date('2019-07-01')
# granularity

cohortdata <- parseCohortData(dldata, group_variables, detailed=TRUE)

daudata <- cohortdata %>%
  group_by(date) %>%
  summarise(
    dau = sum(dau)
  ) %>%
  ungroup() %>%
  left_join(ccudata, by='date') %>%
  mutate(
    max_ratio = max_value / dau,
    avg_ratio = avg_value / dau
  )

plotdata <- melt(select(daudata, date, max_ratio, avg_ratio), id.vars='date', variable.name='type', value.name='ratio')
ggplot(plotdata, aes(date, ratio, color=type)) +
  geom_line() +
  labs(title = sprintf('NISO CCU/DAU Ratios')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('niso_ccu_ratios.png')
  

monthlyratio <- daudata %>%
  mutate(
    date = start_date + as.numeric(date - min(date)),
    month = as.factor(format(date, format='%Y-%m'))
  ) %>%
  group_by(month) %>%
  summarise(
    max_ratio = sum(max_value) / sum(dau),
    avg_ratio = sum(avg_value) / sum(dau),
  )

write.csv(monthlyratio, 'g4_ccu_ratios.csv', row.names=FALSE)

aggregation_scale <- 'none'

if(isTRUE(aggregation_scale == 'weeks')){
  # weekly aggregation scale
  time_variable <- quo(weeks_since_launch)

}else if(isTRUE(aggregation_scale == 'months')){
  # monthly aggregation scale
  time_variable <- quo(months_since_launch)

}else if(isTRUE(aggregation_scale == 'days')){
  # monthly aggregation scale
  time_variable <- quo(days_since_launch)

}else{
  # default to overall aggregation
  time_variable <- NULL
}

group_variables <- c(group_variables, time_variable)
cohortdata <- regroupCohortData(cohortfilter, group_variables)

cohortfilter <- cohortdata %>%
  select(country, cohortday, retention, arpdau) %>%
  filter(cohortday %in% c(1,3,7,14,30))

cohortfilter

write.csv(cohortfilter, 'test_data.csv', row.names=FALSE)

filename <- paste0(game, '_cohortdata_', paste(sapply(group_variables, quo_name), collapse='_'))

retention_range <- cohortdata %>%
  filter(country != 'Rest',
         install_date <= min(install_date) + 270) %>%
  group_by(cohortday) %>%
  summarise(
    min_ret = min(retention),
    max_ret = max(retention)
  )

# saveRDS(cohortdata, paste0(filename, '.RData'))
# cohortdata <- readRDS(paste0(filename, '.RData'))
# write.csv(cohortdata, paste0(filename, '.csv'), row.names=FALSE)

# cohortdata <- readRDS('revenue_modeling_cohortday.RData')
# cohortdata_weeks <- readRDS('revenue_modeling_cohortday_weeks.RData')

ggplot(cohortdata, aes(cohortday, retention, color=country)) +
  geom_line()

# USER INPUTS

# cohort_simulation <- simulation_results$result_summary
# max_cohortday <- max(cohort_simulation$cohortday)

max_cohortday <- max(cohortdata$cohortday)


# True if using user provided cpi, false if using installs
customcpi <- FALSE

# End date, TRUE if up to max cohortday, FALSE if up to max date in install assumptions
up_to_max_cohortday <- TRUE

# Generate daily installs from monthly data

install_assumptions <- read.csv('g4_install_assumptions.csv')

sapply(install_assumptions, class)
install_assumptions <- install_assumptions %>%
  mutate(
    installs = installs * 1000
  )

install_assumptions <- install_assumptions %>%
  arrange(month) %>%
  mutate(
    month_start = as.Date(paste0(as.character(month), '-01'), format='%Y-%m-%d')
  ) %>%
  rowwise() %>%
  mutate(
    numdays = numberOfDays(month_start)
  ) %>%
  ungroup()

start_date <- as.Date(paste0(install_assumptions$month[1], '-01'))

if(up_to_max_cohortday){
  dates <- seq(start_date, by='day', length.out=(max_cohortday+1))
}else{
  end_date <- as.Date(paste(tail(install_assumptions, 1)$month, tail(install_assumptions, 1)$numdays, sep='-'))
  dates <- seq(start_date, end_date, by='day')
}

newdata <- getDailyInstalls(dates, install_assumptions, customcpi, spend_included = FALSE)

# daily installs from daily data

# start_date <- as.Date('2019-06-01')
# newdata <- install_assumptions
# newdata <- newdata %>%
#   filter(cohortday <= max_cohortday) %>%
#   mutate(
#     install_date = start_date + cohortday,
#     month = as.factor(format(install_date, '%Y-%m')),
#   ) %>%
#   rename(installs_new = installs)

 # Run model

revdata <- getRevDaily(newdata, cohortdata, dates, group_variables, max_cohortday, detailed=TRUE, spend_included = FALSE)
alldata <- filter(revdata[['alldata']], cohortday <= max_cohortday)
predictdata <- revdata[['predictdata']]
installdata <- revdata[['installdata']]
monthdata <- getRevMonthly(predictdata, detailed=TRUE, alldata, installdata, spend_included = FALSE)

write.csv(filter(predictdata, !is.na(revenue)), paste0('g4_rev_daily_predictions_', filename, '.csv'), row.names=FALSE)
write.csv(filter(monthdata, !is.na(revenue)), paste0('g4_rev_predictions_', filename, '.csv'), row.names=FALSE)
