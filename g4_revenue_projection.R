# By: James Tan

# Date: 2/27/2018

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales, zoo)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
setwd(datapath)

up_to_max_cohortday <- TRUE
regions <- c('west', 'apac')
max_cohortday <- 270
startday <- 2
bounded <- TRUE
use_CI <- FALSE
# games <- c('All.CRPG', 'Fire.Emblem', 'Marvel.Strike.Force')

# granularity
granularity <- 'none'

if(isTRUE(granularity == 'all')){
  # all group variables
  group_variables <- quos(campaign_type, install_platform, dup_account)

}else if(isTRUE(granularity == 'noplatform')){
  # all except install platform
  group_variables <- quos(campaign_type, dup_account)

}else{
  # no group variables
  group_variables <- NULL
}

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

group_variables <- c(quo(region), group_variables, time_variable)

# name <- 'install_assumptions'
name <- 'model_1'
print_name <- toTitleCase(gsub('_', ' ', name))

install_assumptions <- read.csv(sprintf('g4_%s.csv', name))

region_ratio <- list(apac=.1, west=.9)

sapply(install_assumptions, class)
apac_install_assumptions <- install_assumptions %>%
  mutate(
    region = 'apac',
    installs = installs * 1000 * region_ratio[['apac']]
  )

west_install_assumptions <- install_assumptions %>%
  mutate(
    region = 'west',
    installs = installs * 1000 * region_ratio[['west']]
  )

install_assumptions <- rbind(apac_install_assumptions, west_install_assumptions)

rm(apac_install_assumptions, west_install_assumptions)

install_assumptions <- install_assumptions %>%
  arrange(!!! group_variables, month) %>%
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

newdata <- getDailyInstalls(dates, install_assumptions, customcpi, group_variables, spend_included = FALSE)

# load(sprintf('%d_final_gamedata_ci.RData', max_cohortday))
gamedata <- readRDS(file='g4_interpolated_gamedata.RData')
gamedata <- rename(gamedata, supergame=game, game=scenario)

gamedata <- filter(gamedata,! game %in% c('NISO.Actual'))
gamedata$type <- 'predicted'

daily_predictions <- tibble()
monthly_predictions <- tibble()

games <- distinct(gamedata, region, game)

for(i in 1:nrow(games)){

  r <- games$region[i]
  g <- games$game[i]

  results <- getG4Predictions(newdata, gamedata, r, g, dates, group_variables, max_cohortday, use_CI)
  # results <- getG4Predictions(newdata, gamedata, r, g, dates, group_variables, max_cohortday, FALSE)

  daily_predictions <- rbind(daily_predictions, results$predictdata)
  monthly_predictions <- rbind(monthly_predictions, results$monthdata)

  # write.csv(filter(predictdata, !is.na(revenue)), sprintf('g4_rev_daily_predictions_%s_%s.csv', g, r), row.names=FALSE)
  # write.csv(filter(predictdata, !is.na(revenue)), sprintf('g4_rev_predictions_%s_%s.csv', g, r), row.names=FALSE)
}

summary(daily_predictions)

p1 <- ggplot(monthly_predictions, aes(month, revenue, color=game, group=game)) +
  geom_line() +
  facet_grid(~region, scales='free_y') +
  scale_y_continuous(labels = scales::dollar_format())

ggplotly(p1)

combined_daily <- daily_predictions %>%
  group_by(game, date, month, type) %>%
  summarise(
    region = 'combined',
    revenue = sum(revenue),
    dau = sum(dau),
    arpdau = revenue / dau,
    installs = sum(installs),
  ) %>%
  group_by(game, region, type) %>%
  arrange(date) %>%
  mutate(
    cum_rev = cumsum(revenue),
    cum_installs = cumsum(installs)
  ) %>%
  ungroup()

onmyoji_apac <- daily_predictions %>%
  filter(game == 'Onmyoji' & region == 'apac') %>%
  mutate(
    game = 'Onmyoji APAC + Marvel West'
  )

marvel_west <- daily_predictions %>%
  filter(game == 'Marvel.Strike.Force' & region == 'west') %>%
  mutate(
    game = 'Onmyoji APAC + Marvel West'
  )

onmyoji_marvel <- daily_predictions %>%
  filter((game == 'Onmyoji' & region == 'apac') | (game == 'Marvel.Strike.Force' & region == 'west')) %>%
  group_by(date, month, type) %>%
  summarise(
    game = 'Onmyoji APAC + Marvel West',
    region = 'combined',
    revenue = sum(revenue),
    dau = sum(dau),
    arpdau = revenue / dau,
    installs = sum(installs),
  ) %>%
  group_by(game, region, type) %>%
  arrange(date) %>%
  mutate(
    cum_rev = cumsum(revenue),
    cum_installs = cumsum(installs)
  ) %>%
  ungroup()

daily_predictions <- reduce(list(daily_predictions, combined_daily, onmyoji_apac, marvel_west, onmyoji_marvel), rbind)

daily_predictions$type <- factor(daily_predictions$type, levels=c('lower', 'predicted', 'upper'))

daily_predictions <- daily_predictions %>% arrange(game, region, date)

monthly_predictions <- getRevMonthly(daily_predictions, group_variables = quos(game, region, type), detailed=FALSE, spend_included=FALSE, cut_incomplete=TRUE) %>% ungroup()
monthly_predictions$type <- factor(monthly_predictions$type, levels=c('lower', 'predicted', 'upper'))

# write.csv(daily_predictions, sprintf('g4_%s_daily_all_predictions_public_data_CI.csv', name), row.names=FALSE)
# write.csv(monthly_predictions, sprintf('g4_%s_monthly_all_predictions_public_data_CI.csv', name), row.names=FALSE)

write.csv(daily_predictions, sprintf('g4_%s_daily_all_predictions_public_data_interpolated.csv', name), row.names=FALSE)
write.csv(monthly_predictions, sprintf('g4_%s_monthly_all_predictions_public_data_interpolated.csv', name), row.names=FALSE)

# ggplot(monthly_predictions, aes(month, revenue, color=type, group=type)) +
#   geom_line() +
#   facet_grid(region ~ game, scales='free_y') +
#   scale_y_continuous(labels = scales::dollar_format())

# plotdata <- dcast(daily_predictions, game + region + date ~ type, value.var='revenue')
# plotdata <- rename(plotdata, revenue=predicted)
# ggplot(plotdata, aes(date, revenue, group=interaction(region, game))) +
#   geom_ribbon(aes(ymax=upper, ymin=revenue), alpha=.4, color='green', fill='green') +
#   geom_ribbon(aes(ymax=revenue, ymin=lower), alpha=.4, color='red', fill='red') +
#   geom_line() +
#   facet_grid(region ~ game, scales='free_y') +
#   labs(title = sprintf('G4 %s Daily Revenue Prediction with Confidence Intervals', print_name)) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   scale_x_date(date_breaks = '2 month', date_labels = '%b')
# 
# ggsave_default(sprintf('g4_%s_daily_revenue_prediction_with_CI.png', name))
# 
# plotdata <- dcast(monthly_predictions, game + region + month ~ type, value.var='revenue')
# plotdata$month <- as.Date(paste0(as.character(plotdata$month), '-01'), format='%Y-%m-%d')
# plotdata <- rename(plotdata, revenue=predicted)
# ggplot(plotdata, aes(month, revenue, group=interaction(region, game))) +
#   geom_ribbon(aes(ymax=upper, ymin=revenue), alpha=.4, color='green', fill='green') +
#   geom_ribbon(aes(ymax=revenue, ymin=lower), alpha=.4, color='red', fill='red') +
#   geom_line() +
#   geom_point() +
#   geom_point(aes(y=upper), color='green') +
#   geom_point(aes(y=lower), color='red') +
#   facet_grid(region ~ game, scales='free_y') +
#   labs(title = sprintf('G4 %s Monthly Revenue Prediction with Confidence Intervals', print_name)) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   scale_x_date(date_breaks = '2 month', date_labels = '%b')
# 
# ggsave_default(sprintf('g4_%s_monthly_revenue_prediction_with_CI.png', name))
# 
# # plotdata <- dcast(monthly_predictions, month + game + region ~ type, value.var='revenue')
# # ggplot(plotdata, aes(month, predicted, ymin=lower, ymax=upper, color=game, group=game, fill=game)) +
# #   geom_line(lwd=2) +
# #   geom_ribbon(alpha=.2) +
# #   facet_wrap(~region) +
# #   scale_y_continuous(labels = scales::dollar_format())
# 
# plotdata <- dcast(monthly_predictions, game + region + month ~ type, value.var='cum_rev')
# plotdata$month <- as.Date(paste0(as.character(plotdata$month), '-01'), format='%Y-%m-%d')
# plotdata <- rename(plotdata, revenue=predicted)
# ggplot(plotdata, aes(month, revenue, group=interaction(region, game))) +
#   geom_ribbon(aes(ymax=upper, ymin=revenue), alpha=.4, color='green', fill='green') +
#   geom_ribbon(aes(ymax=revenue, ymin=lower), alpha=.4, color='red', fill='red') +
#   geom_line() +
#   geom_point() +
#   geom_point(aes(y=upper), color='green') +
#   geom_point(aes(y=lower), color='red') +
#   facet_grid(region ~ game, scales='free_y') +
#   labs(title = sprintf('G4 %s Total Revenue Prediction with Confidence Intervals', print_name)) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::dollar_format()) +
#   scale_x_date(date_breaks = '2 month', date_labels = '%b')
# 
# ggsave_default(sprintf('g4_%s_total_revenue_prediction_with_CI.png', name))

# daily_output <- dcast(daily_predictions, date + month + game + region ~ type, value.var = 'revenue')
# monthly_output <- dcast(monthly_predictions, month + game + region ~ type, value.var = 'revenue')

daily_output <- dcast(daily_predictions, date + month + region ~ game, value.var = 'revenue')
monthly_output <- dcast(monthly_predictions, month + region ~ game, value.var = 'revenue')

# ggplot(daily_predictions, aes(date, revenue, color=game)) +
#   geom_line() +
#   facet_wrap(~region) +
#   labs(title = paste('G4 Daily Revenue Prediction')) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::dollar_format())
#
# ggsave_default('g4_daily_rev_prediction_public_data.png')
#
# ggplot(monthly_predictions, aes(month, revenue, color=game, group=game)) +
#   geom_line() +
#   facet_wrap(~region) +
#   labs(title = paste('G4 Monthly Revenue Prediction')) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::dollar_format())
#
# ggsave_default('g4_monthly_rev_prediction_public_data.png')

# write.csv(daily_output, sprintf('g4_%s_daily_rev_prediction_public_data_CI.csv', name), row.names=FALSE)
# write.csv(monthly_output, sprintf('g4_%s_monthly_rev_prediction_public_data_CI.csv', name), row.names=FALSE)

plot_interpolated <- FALSE
if(plot_interpolated){
  
  ggplot(monthly_predictions, aes(month, cum_rev, color=game, group=game)) +
    geom_line() +
    facet_wrap(~region) +
    labs(title = paste('G4 Monthly Revenue Prediction')) +
    theme(plot.title = element_text(hjust=.5)) +
    scale_y_continuous(labels = scales::dollar_format())

  ggsave_default('g4_monthly_rev_prediction_public_data.png')
  
}

write.csv(daily_output, sprintf('g4_%s_daily_rev_prediction_public_data_interpolated.csv', name), row.names=FALSE)
write.csv(monthly_output, sprintf('g4_%s_monthly_rev_prediction_public_data_interpolated.csv', name), row.names=FALSE)

# save(daily_predictions, monthly_predictions, file=sprintf('g4_%s_revenue_predictions.RData', name))

