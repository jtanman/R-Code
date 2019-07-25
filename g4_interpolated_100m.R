# By: James Tan

# Date: 3/13/2018

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis)

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
name <- 'model_7'
finance_targets <- c(model_1=67807.76, model_2=56452.80, model_3=80457.05, model_4=56453, model_5=54238, model_6=78301.873, model_7=63148)
target <- finance_targets[name] * 1e3
print_name <- toTitleCase(gsub('_', ' ', name))

install_assumptions <- read_csv(sprintf('g4_%s.csv', name)) %>% mutate(month = as.factor(month))

sapply(install_assumptions, class)

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
gamedata$type <- as.factor('predicted')

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

daily_predictions <- daily_predictions %>%
  rename(scenario = game) %>%
  mutate(
    game = as.factor(ifelse(grepl('Marvel', scenario), 'Marvel.Strike.Force', 'Onmyoji')),
    scenario = as.factor(sprintf('NISO_Game_%s', regmatches(scenario, regexpr('[[:digit:]]+.*$', scenario))))
  )

level_ordering <- order(as.numeric(str_extract(levels(daily_predictions$scenario), '\\d+')))
daily_predictions$scenario <- factor(daily_predictions$scenario, levels(daily_predictions$scenario)[level_ordering])


# monthly_interpolated <- getRevMonthly(daily_interpolated, group_variables = quos(game, scenario, region, type), detailed=FALSE, spend_included=FALSE, cut_incomplete=TRUE) %>% ungroup()
#
#
# p1 <- ggplot(monthly_interpolated, aes(month, revenue, color=scenario, group=scenario)) +
#   geom_line() +
#   facet_grid(game ~ region, scales='free_y') +
#   scale_y_continuous(labels = scales::dollar_format())
# p1
# ggplotly(p1)
#
# ggsave_default('g4_interpolated_rev.png')

combined_daily <- daily_predictions %>%
  group_by(game, scenario, date, month, type) %>%
  summarise(
    region = as.factor('combined'),
    revenue = sum(revenue),
    dau = sum(dau),
    arpdau = revenue / dau,
    installs = sum(installs),
  ) %>%
  group_by(game, region, scenario, type) %>%
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
  group_by(scenario, date, month, type) %>%
  summarise(
    game = 'Onmyoji APAC + Marvel West',
    region = 'combined',
    revenue = sum(revenue),
    dau = sum(dau),
    arpdau = revenue / dau,
    installs = sum(installs),
  ) %>%
  group_by(game, region, scenario, type) %>%
  arrange(date) %>%
  mutate(
    cum_rev = cumsum(revenue),
    cum_installs = cumsum(installs)
  ) %>%
  ungroup()

daily_predictions <- reduce(list(daily_predictions, combined_daily, onmyoji_apac, marvel_west, onmyoji_marvel), rbind)

daily_predictions$type <- factor(daily_predictions$type, levels=c('lower', 'predicted', 'upper'))

daily_predictions <- daily_predictions %>% arrange(game, region, scenario, date)

arpdau_scale <- daily_predictions %>%
  filter(region == 'combined') %>%
  group_by(game, region, scenario) %>%
  summarise(
    arpdau_scale = target / cum_rev[which(date == as.Date('2019-12-31'))],
    # cohortday = as.numeric(date - min(date))
  ) %>%
  ungroup() %>%
  select(-region)

# filter(gamedata, game == 'NISO_Onmyoji_100%_0%' | game == 'NISO_Marvel.Strike.Force_100%_0%', cohortday < 5)

gamedata <- gamedata %>%
  rename(scenario = game, game = supergame) %>%
  mutate(
    scenario = as.factor(sprintf('NISO_Game_%s', regmatches(scenario, regexpr('[[:digit:]]+.*$', scenario))))
  )

level_ordering <- order(as.numeric(str_extract(levels(gamedata$scenario), '\\d+')))
gamedata$scenario <- factor(gamedata$scenario, levels(gamedata$scenario)[level_ordering])

# filter(gamedata, scenario == 'NISO_Game_100%_0%', cohortday < 5)

onmyoji_marvel_gamedata <- gamedata %>%
  filter((game == 'Onmyoji' & region == 'apac') | (game == 'Marvel.Strike.Force' & region == 'west')) %>%
  mutate(
    game = 'Onmyoji APAC + Marvel West'
  )

gamedata <- rbind(gamedata, onmyoji_marvel_gamedata) %>%
  left_join(arpdau_scale, by=c('game', 'scenario')) %>%
  mutate(
    arpdau = arpdau * arpdau_scale
  )


# level_ordering <- order(as.numeric(str_extract(levels(gamedata$scenario), '\\d+')))
# gamedata$scenario <- factor(gamedata$scenario, levels(gamedata$scenario)[level_ordering])

realgame <- readRDS(file='NISO_MISO_Marvel_Onmyoji_gamedata.RData')
onmyoji_marvel_combined_real <- realgame %>%
  filter((game == 'Onmyoji' & region == 'apac') | (game == 'Marvel.Strike.Force' & region == 'west')) %>%
  mutate(
    game = as.factor('Onmyoji APAC + Marvel West'),
    scenario = as.factor('exact'),
    type = as.factor('predicted'),
    arpdau_scale = 1
  ) %>%
  select(-c(retention_lower, retention_upper, arpdau_lower, arpdau_upper))

onmyoji_marvel_real <- realgame %>%
  filter((game == 'Onmyoji') | (game == 'Marvel.Strike.Force')) %>%
  mutate(
    game = droplevels(game),
    scenario = as.factor('exact'),
    type = as.factor('predicted'),
    arpdau_scale = 1
  ) %>%
  select(-c(retention_lower, retention_upper, arpdau_lower, arpdau_upper))

niso_miso_real <- realgame %>%
  filter((game == 'NISO.Actual') | (game == 'MISO.Actual')) %>%
  mutate(
    scenario = as.factor('exact'),
    type = as.factor('predicted'),
    arpdau_scale = 1
  ) %>%
  select(-c(retention_lower, retention_upper, arpdau_lower, arpdau_upper))

gamedata <- do.call(rbind, list(gamedata, onmyoji_marvel_combined_real, onmyoji_marvel_real))

gamedata_output <- do.call(rbind, list(gamedata, niso_miso_real))
gamedata_output$scenario <- relevel(gamedata_output$scenario, 'exact')
# levels(gamedata_output$scenario)
gamedata_output$model <- as.factor(name)

gamedata_west <- filter(gamedata_output, region == 'west')
gamedata_apac <- filter(gamedata_output, region == 'apac')

gamedata_combined <- inner_join(gamedata_west, gamedata_apac, by=c('game', 'scenario', 'type', 'model', 'cohortday'), suffix = c('_west', '_apac')) %>%
  mutate(
    region = 'combined',
    retention = region_ratio[['west']] * retention_west + region_ratio[['apac']] * retention_apac,
    arpdau = region_ratio[['west']] * arpdau_west + region_ratio[['apac']] * arpdau_apac,
    arpdau_scale = region_ratio[['west']] * arpdau_scale_west + region_ratio[['apac']] * arpdau_scale_apac
  ) %>%
  select(
    -contains('west'),
    -contains('apac')
  )

gamedata_output <- bind_rows(gamedata_output, gamedata_combined) %>%
  mutate(
    region = fct_inorder(region)
  )

gamedata_output <- gamedata_output %>%
  group_by(game, region, scenario) %>%
  arrange(cohortday) %>%
  mutate(
    arpi = retention * arpdau,
    cumarpi = cumsum(arpi)
  ) %>%
  select(game, region, scenario, type, model, cohortday, everything()) %>%
  arrange(game, region, scenario, cohortday)

rm(gamedata_west, gamedata_apac, gamedata_combined)

saveRDS(gamedata_output, file=sprintf('g4_%s_interpolated_scaled_100m_gamedata.RData', name))
write.csv(gamedata_output, file=sprintf('g4_%s_interpolated_scaled_100m_gamedata.csv', name), row.names=FALSE)

# gamedata <- readRDS(file=sprintf('g4_interpolated_scaled_100m_%s_gamedata.RData', name))

p1 <- ggplot(filter(gamedata_output, game != 'Onmyoji'), aes(cohortday, arpdau, color=scenario)) +
  geom_line() +
  facet_grid(game ~ region) +
  labs(title = sprintf('G4 %s Interpolated Scaled ARPDAU', print_name)) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format())

p1
ggsave_default(sprintf('g4_%s_interpolated_scaled_arpdau.png', name))

# p2 <- ggplot(filter(gamedata_output, scenario != 'exact', game != 'NISO.Actual' & game != 'MISO.Actual'), aes(cohortday, retention, color=scenario)) +
#   geom_line() +
#   facet_grid(game ~ region) +
#   labs(title = sprintf('G4 Interpolated Retention')) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::percent_format())
# p2
# 
# ggsave_default(sprintf('g4_interpolated_retention.png', name))

# ggplot(gamedata, aes(cohortday, retention, color=scenario)) +
#   geom_line() +
#   facet_grid(game ~ region) +
#   labs(title = sprintf('G4 Interpolated Retention', print_name)) +
#   theme(plot.title = element_text(hjust=.5)) +
#   scale_y_continuous(labels = scales::percent)
#
# ggsave_default(sprintf('g4_interpolated_retention.png', name))

# realdata <- readRDS('NISO_MISO_Marvel_Onmyoji_gamedata.RData')
#
# summary(gamedata)
# summary(realdata)
#
# realdata <- realdata %>%
#   mutate(
#     scenario = as.factor('exact'),
#     type = as.factor('predicted'),
#     arpdau_scale = 1
#   ) %>%
#   select(game, region, scenario, type, cohortday, retention, arpdau, arpdau_scale)
#
# gd <- gamedata
# gamedata <- rbind(gamedata, realdata)

gamedata <- gamedata %>%
  mutate(game = as.factor(paste(scenario, game, sep='_')))

daily_predictions <- tibble()
monthly_predictions <- tibble()

games <- distinct(gamedata, region, game)

for(i in 1:nrow(games)){

  r <- games$region[i]
  g <- games$game[i]

  results <- getG4Predictions(newdata, gamedata, r, g, dates, group_variables, max_cohortday, use_CI)

  daily_predictions <- rbind(daily_predictions, results$predictdata)
  monthly_predictions <- rbind(monthly_predictions, results$monthdata)
}

daily_predictions <- daily_predictions %>%
  rename(scenario=game) %>%
  mutate(
    game = as.factor(ifelse(stringr::str_detect(scenario, 'exact'), stringr::str_replace(scenario, '^exact_', ''), stringr::str_replace(scenario, '^\\S+%_\\d+%_', ''))),
    scenario = as.factor(ifelse(stringr::str_detect(scenario, 'exact'), 'exact', stringr::str_extract(scenario, '^\\S+%_\\d+%')))
  )

level_ordering <- c(1, level_ordering + 1)
daily_predictions$scenario <- factor(daily_predictions$scenario, levels(daily_predictions$scenario)[level_ordering])

niso_miso_rev <- readRDS(file='niso_miso_actual_daily_predictions_g4_startdate.RData')
niso_miso_rev <- niso_miso_rev %>% mutate(
  date = min(dates) - min(date) + date,
  scenario = as.factor('exact'),
  type = as.factor('predicted'),
  month = factor(format(date, '%Y-%m'))
)

daily_predictions <- rbind(niso_miso_rev, daily_predictions)

combined_daily <- daily_predictions %>%
  group_by(game, scenario, date, month, type) %>%
  summarise(
    region = as.factor('combined'),
    revenue = sum(revenue),
    dau = sum(dau),
    arpdau = revenue / dau,
    installs = sum(installs),
  ) %>%
  group_by(game, region, scenario, type) %>%
  arrange(date) %>%
  mutate(
    cum_rev = cumsum(revenue),
    cum_installs = cumsum(installs)
  ) %>%
  ungroup()

daily_predictions <- reduce(list(daily_predictions, combined_daily), rbind)

monthly_predictions <- getRevMonthly(daily_predictions, group_variables = quos(game, region, scenario, type), detailed=FALSE, spend_included=FALSE, cut_incomplete=TRUE) %>% ungroup()
monthly_predictions$type <- factor(monthly_predictions$type, levels=c('lower', 'predicted', 'upper'))

write.csv(daily_predictions, sprintf('g4_%s_daily_all_predictions_interpolated_scaled.csv', name), row.names=FALSE)
write.csv(monthly_predictions, sprintf('g4_%s_monthly_all_predictions_interpolated_scaled.csv', name), row.names=FALSE)

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

# daily_output <- dcast(daily_predictions, date + month + region ~ game, value.var = 'revenue')
# monthly_output <- dcast(monthly_predictions, month + region ~ game, value.var = 'revenue')

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

plot_interpolated <- TRUE
if(plot_interpolated){

  ggplot(filter(daily_predictions, game != 'Onmyoji'), aes(date, revenue, group=scenario, color=scenario)) +
    geom_line() +
    facet_grid(region ~ game, scales='free_y') +
    labs(title = sprintf('G4 %s Daily Revenue Prediction (Interpolated Scaled)', print_name)) +
    theme(plot.title = element_text(hjust=.5)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_breaks = '2 month', date_labels = '%b')

  ggsave_default(sprintf('g4_%s_daily_revenue_prediction_interpolated_scaled.png', name))


  plotdata <- monthly_predictions %>%
  filter(game != 'Onmyoji') %>%
    mutate(
      month = as.Date(paste0(as.character(month), '-01'), format='%Y-%m-%d')
    )
  ggplot(plotdata, aes(month, revenue, color=scenario, group=scenario)) +
    geom_line() +
    facet_grid(region ~ game, scales='free_y') +
    labs(title = sprintf('G4 %s Monthly Revenue Prediction (Interpolated Scaled)', print_name)) +
    theme(plot.title = element_text(hjust=.5)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_breaks = '2 month', date_labels = '%b')

  ggsave_default(sprintf('g4_%s_monthly_revenue_prediction_interpolated_scaled.png', name))

  ggplot(plotdata, aes(month, cum_rev, color=scenario)) +
    geom_line() +
    facet_grid(region ~ game, scales='free_y') +
    labs(title = sprintf('G4 %s Total Revenue Prediction (Interpolated Scaled)', print_name)) +
    theme(plot.title = element_text(hjust=.5)) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_breaks = '2 month', date_labels = '%b')

  ggsave_default(sprintf('g4_%s_total_revenue_prediction_interpolated_scaled.png', name))

}

# write.csv(daily_output, sprintf('g4_%s_daily_rev_prediction_interpolated_scaled.csv', name), row.names=FALSE)
# write.csv(monthly_output, sprintf('g4_%s_monthly_rev_prediction_interpolated_scaled.csv', name), row.names=FALSE)

# save(daily_predictions, monthly_predictions, file=sprintf('g4_%s_revenue_predictions.RData', name))

