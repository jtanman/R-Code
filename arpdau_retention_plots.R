# By: James Tan

# Date: 7/2/2019

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/revenue_impact_finance_helper.R')

group_variables <- quos(game_id, campaign_type, install_platform, country)
cohortday_list <- c(1, 3, 7, 14, 21, 30)

nisodata <- read_csv('revenue_modeling_cohortday_niso_24hr_metrics_2017-06-28_2019-06-29.csv', col_types = cols(
  game_id = readr::col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
))

misodata <- read_csv('revenue_modeling_cohortday_miso_24hr_metrics_2018-11-01_2019-06-29.csv', col_types = cols(
  game_id = col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
))

odydata <- read_csv('revenue_modeling_cohortday_ody_24hr_metrics_2013-07-25_2019-06-29.csv', col_types = cols(
  game_id = col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
))

mydata <- reduce(list(nisodata, misodata), rbind)

cohortdata <- parseCohortData(mydata, group_variables, detailed=TRUE)

summary(cohortdata)

plotdata <- filter(cohortdata, cohortday %in% cohortday_list)
  

ggplot(plotdata, aes(retention, arpdau)) +
  geom_hex() +
  facet_grid(game_id ~ cohortday, scales='free', labeller = labeller(game_id=label_both)) +
  scale_fill_distiller(palette='Spectral', direction=-1) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = dollar) +
  labs(title = 'Retention by ARPDAU Density by Cohortday and Game') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('retention_by_arpdau_density.png')

ggplot(plotdata, aes(retention, arpdau, color=days_since_launch)) +
  geom_point(alpha=.2) +
  facet_grid(game_id ~ cohortday, scales='free', labeller = labeller(game_id=label_both)) +
  scale_color_distiller(palette='Spectral') +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = dollar) +
  labs(title = 'Retention by ARPDAU Age by Cohortday and Game') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('retention_by_arpdau_age.png')
