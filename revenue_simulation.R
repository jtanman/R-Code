# By: James Tan

# Date: 2/7/2019

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales)

source('~/.Rprofile')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
setwd(datapath)

dldata <- read.csv('revenue_modeling_cohortday_all_dates_miso.csv')

group_variables <- quos(game_id, campaign_type, install_platform, dup_account, country)

summary(dldata)

mydata <- parseCohortData(dldata, group_variables)

summary(mydata)

aggregation_scale <- 'weeks'

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
mydata <- regroupCohortData(mydata, group_variables)

summary(mydata)

install_assumptions <- read.csv('g4_install_assumptions.csv')

sapply(install_assumptions, class)
install_assumptions <- install_assumptions %>%
  mutate(
    installs = installs * 1000
  )

