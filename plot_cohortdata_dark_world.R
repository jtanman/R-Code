# By: James Tan

# Date: 3/10/2019

p_load(reshape2, beepr, scales, zoo, tidyverse)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/revenue_impact_finance_helper.R')

# case when current_isogroup = 2 then ‘20190420’
# when current_isogroup in (12,17,21) then ‘20190421’
# when current_isogroup = 25 then ‘20190507’
# when current_isogroup = 27 then ‘20190514’
# else null
# dark world release date

mydata <- read_csv('revenue_modeling_cohortday_dark_world_niso_iso_12_17_21.csv', col_types = cols(
  game_id = readr::col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
)) %>%
  filter(install_platform == 'iOS' | install_platform == 'android') %>%
  filter(cohortday >= 400) %>%
  rename(date = curr_date) %>%
  mutate(
    group = as_factor(ifelse(date >= as.Date('2019-04-21'), 'Dark World', 'Pre')),
    install_platform = droplevels(install_platform),
  )

group_variables <- quos(group)

cohortdata <- regroupCohortData(mydata, group_variables)

write_csv(cohortdata, 'dark_world_metrics.csv')

comparedata <- cohortdata %>%
  select(cohortday, group, arppu, retention, arpdau, arpi, cumarpi, conversion, arprpdau, rpdau_ret) %>%
  mutate(group = recode(group, 'Dark World' = 'dark_world', 'Pre' = 'pre')) %>%
  pivot_wider(names_from=group, values_from=c(cumarpi, retention, arpdau, arpi, arppu, conversion, arprpdau, rpdau_ret))

write_csv(comparedata, 'dark_world_metrics_compare.csv')

plotdata <- cohortdata %>%
  gather(key='metric', value='value', cumarpi, retention, arpdau, arpi, arppu, conversion, arprpdau, rpdau_ret)


ggplot(plotdata, aes(cohortday, value, color=group)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~metric, nrow=2, scales='free_y') +
  labs(title = 'Dark World vs Pre Metrics', caption='4/21/19 Cutoff') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('dark_world_metrics.png')


plotdata <- cohortdata %>%
  group_by(cohortday) %>%
  summarise(installs = sum(installs))

ggplot(plotdata, aes(cohortday, installs)) +
  geom_bar(stat = 'identity')
