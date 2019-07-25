# By: James Tan

# Date: 3/10/2019

p_load(tidyverse, reshape2, beepr, scales, zoo)

source('~/.Rprofile')
setwd(datapath)

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
  facet_wrap(~type) +
  labs(title='Retention Predictions by Type') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('gof_email_retention.png')

plotdata <- mydata %>%
  rename(arpdau_updated = arpdau) %>%
  gather(key='ret_type', value='retention', retention_cohort, retention_launch, retention_cohort_pred, retention_updated) %>%
  mutate(
    ret_type = factor(ret_type, levels=c('retention_cohort_pred', 'retention_cohort', 'retention_launch', 'retention_updated'))
  )

ggplot(plotdata, aes(cohortday, retention, color=ret_type)) +
  geom_line() +
  facet_wrap(~type) +
  labs(title='Retention Predictions by Type') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('gof_email_retention.png')
