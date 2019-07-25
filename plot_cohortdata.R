# By: James Tan

# Date: 3/10/2019

p_load(reshape2, beepr, scales, zoo, tidyverse)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/revenue_impact_finance_helper.R')

mydata <- read_csv('experiment_results_niso.csv', col_types = cols(
  game_id = col_factor(NULL),
  experiment_group = col_factor(NULL),
  campaign_type = col_factor(NULL),
  install_platform = col_factor(NULL),
  dup_account = col_factor(NULL),
  country = col_factor(NULL)
))

group_variables <- quos(experiment_group)
cohortdata <- parseCohortData(mydata, group_variables, detailed=TRUE)

cohortfilter <- cohortdata %>% filter(
  date >= as.Date('2019-03-07'),
  date < as.Date('2019-03-12')
)

cohortdata <- regroupCohortData(cohortfilter, group_variables)

plotdata <- cohortdata %>%
  gather(key='metric', value='value', retention, arpdau, cumarpi,  ltv_5_conversion, arprpdau, rpdau_ret)

ggplot(plotdata, aes(cohortday, value, color=experiment_group)) +
  # geom_line() +
  geom_smooth() +
  facet_wrap(~metric, scales='free_y')

plotdata <- cohortdata %>%
  group_by(cohortday) %>%
  summarise(installs = sum(installs))

ggplot(plotdata, aes(cohortday, installs)) +
  geom_bar(stat = 'identity')
