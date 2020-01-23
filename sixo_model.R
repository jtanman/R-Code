# By: James Tan

# Date: 11/21/2019

p_load(reshape2, plotly, zoo, expm, beepr, scales, tidyverse, glue, feather)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/generalized_markov_model_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')

game <- 'niso'
start_date <- '2017-06-28'
end_date <- '2017-12-28'

# dldata <- read.csv(glue('revenue_modeling_cohortday_{game}_24hr_metrics_{start_date}_{end_date}.csv'))
# 
# # dldata <- readRDS('revenue_modeling_cohortday_all_dates_APAC_WEST_niso.RData')
# 
# nondupdata <- filter(dldata, dup_account == 'nondup')
# 
# # granularity
# group_variables <- NULL
# 
# cohortdata <- parseCohortData(nondupdata, group_variables, detailed=TRUE)
# 
# saveRDS(cohortdata, file=glue('revenue_modeling_cohortday_{game}_24hr_metrics_{start_date}_{end_date}_parsed.RData'))

cohortdata <- readRDS(glue('revenue_modeling_cohortday_{game}_24hr_metrics_{start_date}_{end_date}_parsed.RData'))

cohortfilter <- filter(cohortdata, date < as.Date(end_date))
# cohortfilter <- cohortdata


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
datedata <- regroupDateData(cohortfilter, group_variables, quos(campaign_type, install_platform, dup_account, country))
write_feather(datedata, glue('revenue_modeling_date_{game}_24hr_metrics_{start_date}_{end_date}_parsed.feather'))

ggplot(datedata, aes(date, revenue)) +
  geom_line()

ggplot(datedata, aes(date, cumrev)) +
  geom_line()

cohortdata <- regroupCohortData(cohortfilter, group_variables)
write_feather(cohortdata, glue('revenue_modeling_cohortday_{game}_24hr_metrics_{start_date}_{end_date}_parsed.feather'))

ggplot(cohortdata, aes(cohortday, ltv_5_conversion)) +
  geom_line()
