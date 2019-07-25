# By: James Tan

# Date: 7/11/2019

p_load(reshape2, beepr, scales, zoo, tidyverse)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/revenue_impact_finance_helper.R')

mydata <- read_csv('revenue_modeling_cohortday_niso_24hr_metrics_2017-06-28_2017-12-28_270.csv', col_types = cols(
  game_id = readr::col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
))

group_variables <- quos(game_id, country)

cohortdata <- parseCohortData(mydata, group_variables, detailed=FALSE)
cohortdata <- regroupCohortData(cohortdata, group_variables)

gamedata <- readRDS('g4_all_models_interpolated_scaled_gamedata.RData')
gamedata <- left_join(gamedata, select(cohortdata, country, cohortday, matches('ltv_\\d+_conversion')), by=c('region' = 'country', 'cohortday'))

write_csv(gamedata, 'g4_all_models_interpolated_scaled_gamedata.csv')


