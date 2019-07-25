# By: James Tan

# Date: 3/10/2019

p_load(tidyverse, reshape2, beepr, scales, zoo)

source('~/.Rprofile')
setwd('~/MZ/python/ds_rpdau_simulation/code/gof_revenue_email/data')

mydata <- read_csv('cohort_updated.csv')
mydata <- mydata %>%
  mutate(
    game_id = as.factor(game_id),
    campaign_type = as.factor(campaign_type),
    install_platform = as.factor(install_platform),
    country = as.factor(country),
    scenario = as.factor(scenario),
    game = as.factor(game),
    type = as.factor(type)
  )

ggplot(mydata, aes(cohortday, retention, color=type)) +
  geom_line()

ggplot(mydata, aes(cohortday, arpdau, color=type)) +
  geom_line()


plotdata <- mydata %>%
  rename(retention_cohort_pred = ret_cohort_pred, retention_updated = retention) %>%
  gather(key='ret_type', value='retention', retention_cohort, retention_launch, retention_cohort_pred, retention_updated) %>%
  mutate(
    ret_type = factor(ret_type, levels=c('retention_cohort_pred', 'retention_cohort', 'retention_launch', 'retention_updated'))
  )

ggplot(plotdata, aes(cohortday, retention, color=ret_type)) +
  geom_line() +
  facet_grid(country ~ campaign_type + install_platform) +
  labs(title='Retention Predictions by Type') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('gof_email_retention.png')

plotdata <- mydata %>%
  rename(arpdau_updated = arpdau) %>%
  gather(key='arpdau_type', value='arpdau', arpdau_cohort, arpdau_launch, arpdau_cohort_pred, arpdau_updated) %>%
  mutate(
    arpdau_type = factor(arpdau_type, levels=c('arpdau_cohort_pred', 'arpdau_cohort', 'arpdau_launch', 'arpdau_updated'))
  )

ggplot(plotdata, aes(cohortday, arpdau, color=arpdau_type)) +
  geom_line() +
  facet_wrap(~type, scales='free_y') +
  # facet_grid(country ~ campaign_type + install_platform, scales='free_y') +
  labs(title='ARPDAU Predictions by Type') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('gof_email_arpdau.png')

# Plot Errors

errordata <- read_csv('df_model_errors.csv')
errordata <- errordata %>%
  mutate(
    method = as.factor(method),
    timeframe = factor(timeframe, levels=c(7, 14, 30, 90, 'all')),
  )

errordata_plot <- errordata %>%
  gather(key='type', value='error', contains('error')) %>%
  mutate(
    type = factor(type, levels=c('ret_error', 'arpdau_error', 'cumarpi_error', 'cumarpi_end_error')),
    prior_confidence = as.factor(prior_confidence)
  )

for(tf in levels(errordata$timeframe)){
  plotdata <- filter(errordata_plot, timeframe == tf)
  ggplot(plotdata, aes(num_days, error, color=method)) +
  geom_line() +
  facet_grid(type ~ prior_confidence, scales='free_y', labeller = labeller(prior_confidence = label_both)) +
  labs(title='D270 ARPI Error by Method', subtitle=sprintf('Timeframe: %s days', tf)) +
  theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5))
  
  ggsave_default(sprintf('method_error_%s.png', tf))
  
  ggplot(plotdata, aes(num_days, error, color=prior_confidence, group=prior_confidence)) +
    geom_line() +
    facet_grid(type ~ method, scales='free_y') +
    labs(title='D270 ARPI Error by Prior Confidence', subtitle=sprintf('Timeframe: %s days', tf)) +
    theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5)) +
    scale_color_brewer(type='seq', palette=1)
  
  ggsave_default(sprintf('prior_confidence_error_%s.png', tf))
  
  plotdata <- filter(plotdata, method != 'bayesian_data')
  
  ggplot(plotdata, aes(num_days, error, color=prior_confidence, group=prior_confidence)) +
    geom_line() +
    facet_grid(type ~ method, scales='free_y') +
    labs(title='D270 ARPI Error by Prior Confidence', subtitle=sprintf('Timeframe: %s days', tf)) +
    theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5)) +
    scale_color_brewer(type='seq', palette=1)
  
  ggsave_default(sprintf('prior_confidence_error_%s_no_bayesian_data.png', tf))
}

