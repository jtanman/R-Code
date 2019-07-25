# By: James Tan

# Date: 3/4/2019

p_load(ggplot2, reshape2, plotly, lubridate, zoo, expm, dplyr, beepr, scales)

source('~/.Rprofile')
source('~/MZ/R Code/generalized_markov_model_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')

setwd(datapath)

game <- 'niso'

dldata <- read.csv(paste0('revenue_modeling_cohortday_all_dates_', game, '.csv'))

dldata <- readRDS('revenue_modeling_cohortday_all_dates_APAC_WEST_niso.RData')

nondupdata <- filter(dldata, dup_account == 'nondup')

# granularity
group_variables <- quos(install_date, country)

cohortdata <- parseCohortData(nondupdata, detailed=TRUE)

saveRDS(cohortdata, file=sprintf('revenue_modeling_cohortdata_all_dates_%s_parsed.RData', game))
cohortdata <- readRDS(sprintf('revenue_modeling_cohortdata_all_dates_%s_parsed.RData', game))

cohortfilter <- filter(cohortdata, cohortday <= 270, date <= min(install_date) + 270)
# cohortfilter <- cohortdata

traindata <- cohortfilter %>%
  group_by(!!! group_variables, date) %>%
  summarise(
    dau = sum(dau),
    revenue = sum(revenue)
  )

installdata <- cohortfilter %>%
  filter(cohortday == 0) %>%
  group_by(!!! group_variables, install_date) %>%
  summarise(
    installs = sum(installs)
  ) %>%
  rename(date = install_date) %>%
  left_join(traindata, by=c('country', 'date'))

outputdata <- melt(installdata, id.vars=c('country', 'date'))
outputdata <- dcast(outputdata, date ~ country + variable)
write.csv(outputdata, 'ff_actuals.csv', row.names=FALSE)

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

filename <- paste0(game, '_cohortdata_', paste(sapply(group_variables, quo_name), collapse='_'))

retdata <- dcast(cohortdata, country + cohortday ~ install_date, value.var='retention')

# saveRDS(cohortdata, paste0(filename, '.RData'))
# cohortdata <- readRDS(paste0(filename, '.RData'))
# write.csv(cohortdata, paste0(filename, '.csv'), row.names=FALSE)

# cohortdata <- readRDS('revenue_modeling_cohortday.RData')
# cohortdata_weeks <- readRDS('revenue_modeling_cohortday_weeks.RData')

ggplot(cohortdata, aes(cohortday, retention, color=country)) +
  geom_line()

plotdata <- filter(cohortdata, cohortday %in% c(0, 1, 7, 14, 30, 90), install_date <= min(install_date) + 270)

ggplot(plotdata, aes(install_date, retention, color=country)) +
  geom_point() +
  facet_wrap(~cohortday, scales = 'free_y') +
  labs(title = paste('Dx Retention Decay')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(sprintf('%s_retention_decay.png', game))

# USER INPUTS

# cohort_simulation <- simulation_results$result_summary
# max_cohortday <- max(cohort_simulation$cohortday)

max_cohortday <- max(cohortdata$cohortday)

variance_variables <- quos(country)

retention_variance <- cohortdata %>%
    group_by(!!! variance_variables, cohortday) %>%
    summarise(
      n = n(),
      users = sum(installs),
      mean_weighted = weighted.mean(retention, installs, na.rm=TRUE),
      mean_2 = sum(dau) / sum(installs),
      mean = mean(retention),
      median = median(retention),
      variance_weighted = weighted.var(retention, installs, na.rm=TRUE),
      variance = var(retention, na.rm=TRUE),
      se_weighted = sqrt(variance_weighted) / sqrt(n()),
      se = sqrt(variance) / sqrt(n()),
      dec.05 = quantile(retention, .05),
      dec.1 = quantile(retention, .1),
      dec.2 = quantile(retention, .2),
      dec.25 = quantile(retention, .25),
      dec.3 = quantile(retention, .3),
      dec.4 = quantile(retention, .4),
      dec.5 = quantile(retention, .5),
      dec.6 = quantile(retention, .6),
      dec.7 = quantile(retention, .7),
      dec.75 = quantile(retention, .75),
      dec.8 = quantile(retention, .8),
      dec.9 = quantile(retention, .9),
      dec.95 = quantile(retention, .95),
      error.95 = qt(.975, df=(n()-1)) * se_weighted,
      lower.95 = mean_weighted - error.95,
      upper.95 = mean_weighted + error.95
    ) %>%
    filter(users > 0) %>%
    ungroup()

write.csv(retention_variance, sprintf('%s_retention_variance.csv', game), row.names=FALSE)
