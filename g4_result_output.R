# By: James Tan

# Date: 3/10/2019

p_load(tidyverse, reshape2, beepr, scales, zoo)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
setwd(datapath)


startday <- 2
max_cohortday <- 270
end_cohortday <- 270
split <- FALSE
plot_flag <- TRUE
bounded <- TRUE

regions <- c('west', 'apac')
cohortdays <- c(270)

# temp_my <- tibble()
# temp_game <- tibble()
#
# load(file=sprintf('%d_%d_gamedata_mydata.RData', startday, max_cohortday))
#
# temp_my <- mydata
# temp_game <- gamedata
#
# load(file=sprintf('%d_%d_gamedata_mydata.RData', 0, max_cohortday))
#
# temp_my <- temp_my %>%
#   filter(game != 'Onmyoji',
#          !(game == 'Final.Fantasy' & region == 'apac'))
# temp_game <- temp_game %>%
#   filter(game != 'Onmyoji',
#          !(game == 'Final.Fantasy' & region == 'apac'))
#
# add_my <- filter(mydata,
#                  game == 'Onmyoji' |
#                 (game == 'Marvel.Strike.Force' & region == 'apac') |
#                 (game == 'Age.of.Magic' & region == 'west') |
#                 (game == 'Final.Fantasy' & region == 'apac'))
#
# add_game <- filter(gamedata,
#                    game == 'Onmyoji' |
#                   (game == 'Marvel.Strike.Force' & region == 'apac') |
#                   (game == 'Age.of.Magic' & region == 'west') |
#                   (game == 'Final.Fantasy' & region == 'apac'))
#
# temp_my <- rbind(temp_my, add_my)
# temp_game <- rbind(temp_game, add_game)
#
# mydata <- temp_my
# gamedata  <- temp_game
#
# gamedata <- arrange(gamedata, region, game, cohortday)
# mydata <- arrange(mydata, region, game, days_since_launch)
#
#
# gamedata <- gamedata %>%
#   mutate(
#     game = as.factor(game),
#     region = as.factor(region)
#   )
# mydata <- mydata %>%
#   mutate(
#     game = as.factor(game),
#     region = as.factor(region)
#   )

# save(gamedata, mydata, file=sprintf('%d_final_gamedata_mydata.RData', max_cohortday))

# load(file=sprintf('%d_final_gamedata_mydata.RData', max_cohortday))

load(sprintf('%d_final_gamedata_ci.RData', max_cohortday))

gamedata <- filter(gamedata,! game %in% c('NISO.Actual'))
mydata <- filter(mydata, game != 'NISO.Actual')

retdata <- dcast(gamedata, cohortday ~ region + game, value.var='retention')
arpdaudata <- dcast(gamedata, cohortday ~ region + game, value.var='arpdau')
write.csv(retdata, sprintf('%d_final_fitted_retention.csv', max_cohortday), row.names=FALSE)
write.csv(arpdaudata, sprintf('%d_final_fitted_arpdau.csv', max_cohortday), row.names=FALSE)

ggplot(gamedata, aes(cohortday, arpdau, color=game)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted ARPDAU', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format())

ggsave_default(sprintf('%d_final_fitted_arpdau.png', max_cohortday))

ggplot(gamedata, aes(cohortday, retention, color=game)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted Retention', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::percent)

ggsave_default(sprintf('%d_final_fitted_retention.png', max_cohortday))

plotdata <- melt(mydata, id.vars=c('game', 'region', 'month', 'days_since_launch'))

dauplotdata <- filter(plotdata, variable %in% c('dau', 'dau_pred'))
ggplot(dauplotdata, aes(days_since_launch, value, color=game, group=interaction(game, variable), lty=variable)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted DAU', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(sprintf('%d_final_fitted_dau.png', max_cohortday))

revplotdata <- filter(plotdata, variable %in% c('rev', 'rev_pred'))

ggplot(revplotdata, aes(days_since_launch, value, color=game, group=interaction(game, variable), lty=variable)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted Rev', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(sprintf('%d_final_fitted_rev.png', max_cohortday))

# output data

game_output <- gamedata
game_output <- game_output %>%
  filter(
    game %in% c('Marvel.Strike.Force', 'Onmyoji')
  )
output_cohortdays <- c(1, 3, 7, 14, 30)

niso_cohortdata <- readRDS('niso_cohortdata_country.RData')
niso_real <- niso_cohortdata %>%
  select(country, cohortday, retention, arpdau) %>%
  filter(country != 'Rest') %>%
  rename(region = country) %>%
  mutate(
    game = 'NISO.Actual',
    region = tolower(region),
    retention_lower = NA,
    retention_upper = NA,
    arpdau_lower = NA,
    arpdau_upper = NA
  )

miso_cohortdata <- readRDS('miso_cohortdata_country.RData')
miso_real <- miso_cohortdata %>%
  select(country, cohortday, retention, arpdau) %>%
  filter(country != 'Rest') %>%
  rename(region = country) %>%
  mutate(
    game = 'MISO.Actual',
    region = tolower(region),
    retention_lower = NA,
    retention_upper = NA,
    arpdau_lower = NA,
    arpdau_upper = NA
  )

combined_output <- reduce(list(game_output, niso_real, miso_real), rbind)

# summary(combined_output, maxlevels=100)

game_output <- combined_output %>%
  filter(
    cohortday %in% output_cohortdays
  ) %>%
  mutate(
    game = factor(game, levels=c('NISO.Actual', 'MISO.Actual', 'Marvel.Strike.Force', 'Onmyoji')),
    retention = percent(retention),
    arpdau = format.money(arpdau),
    retention_upper = ifelse(is.na(retention_upper), NA, percent(retention_upper)),
    retention_lower = ifelse(is.na(retention_lower), NA, percent(retention_lower)),
    arpdau_upper = ifelse(is.na(arpdau_upper), NA, format.money(arpdau_upper)),
    arpdau_lower = ifelse(is.na(arpdau_lower), NA, format.money(arpdau_lower))
  ) %>%
  arrange(game, region)


# saveRDS(game_output, 'NISO_MISO_Marvel_Onmyoji_gamedata.RData')


sapply(game_output, class)
levels(game_output$game)
ret_output <- dcast(game_output, cohortday ~ game + region, value.var='retention')
ret_output <- mutate(ret_output, Marvel.Strike.Force_west_2 = Marvel.Strike.Force_west)
arpdau_output <- dcast(game_output, cohortday ~ game + region, value.var='arpdau')
arpdau_output <- mutate(arpdau_output, Marvel.Strike.Force_west_2 = Marvel.Strike.Force_west)

metric_output <- rbind(ret_output, arpdau_output)
write.csv(metric_output, 'g4_marvel_onmyoji_metrics_table.csv', row.names=FALSE)

name <- 'model_1'
load(sprintf('g4_%s_revenue_predictions.RData', name))

miso_metricdata <- readRDS('revenue_modeling_cohortdata_all_dates_miso_parsed.RData')
niso_metricdata <- readRDS('revenue_modeling_cohortdata_all_dates_niso_parsed.RData')

niso_rev <- niso_metricdata %>%
  rename(month=date_month, region=country) %>%
  mutate(
    month = as.numeric(month)
  ) %>%
  filter(region != 'Rest', month != max(month), month != min(month)) %>%
  mutate(
    month = as.numeric(as.factor(month))
  ) %>%
  group_by(month, region) %>%
  summarise(
    game = 'NISO.Actual',
    revenue = sum(revenue)
  ) %>% ungroup()

miso_rev <- miso_metricdata %>%
  rename(month=date_month, region=country) %>%
  mutate(
    month = as.numeric(month)
  ) %>%
  filter(region != 'Rest', month != max(month)) %>%
  group_by(month, region) %>%
  summarise(
    game = 'MISO.Actual',
    revenue = sum(revenue)
  ) %>% ungroup()

monthly_rev <- monthly_predictions %>%
  filter(
    region != 'combined',
    game %in% c('Marvel.Strike.Force', 'Onmyoji APAC + Marvel West'),
    type == 'predicted'
  ) %>%
  mutate(
    month = as.numeric(month)
  ) %>%
  select(month, game, region, revenue)

monthly_output <- reduce(list(niso_rev, miso_rev, monthly_rev), rbind)
monthly_output$game <- factor(monthly_output$game, levels=c('NISO.Actual', 'MISO.Actual', 'Marvel.Strike.Force', 'Onmyoji', 'Onmyoji APAC + Marvel West'))
monthly_output <- dcast(monthly_output, month ~ game + region, value.var='revenue')

write.csv(monthly_output, 'g4_marvel_onmyoji_revenue_table.csv', row.names=FALSE)

# miso_metricdata <- readRDS('revenue_modeling_cohortdata_all_dates_miso_parsed.RData')
# niso_metricdata <- readRDS('revenue_modeling_cohortdata_all_dates_niso_parsed.RData')
# 
# start_date <- as.Date('2019-07-01')
# end_date <- start_date + 270
# 
# niso_installs <- niso_metricdata %>%
#   rename(region=country) %>%
#   filter(region != 'Rest', install_date == date) %>%
#   group_by(region, date) %>%
#   summarise(
#     installs = sum(installs)
#   )
# 
# niso_rev <- niso_metricdata %>%
#   rename(region = country) %>%
#   filter(region != 'Rest') %>%
#   group_by(date, region) %>%
#   summarise(
#     game = as.factor('NISO.Actual'),
#     revenue = sum(revenue),
#     dau = sum(dau),
#     arpdau = revenue / dau
#   ) %>%
#   left_join(niso_installs, by=c('date', 'region')) %>%
#   group_by(region) %>%
#   arrange(date) %>%
#   mutate(
#     cum_rev = cumsum(revenue),
#     cum_installs = cumsum(installs),
#     date = start_date + as.numeric(date - min(date)),
#     month = as.factor(format(date, format='%Y-%m'))
#   )
# 
# miso_installs <- miso_metricdata %>%
#   rename(region=country) %>%
#   filter(region != 'Rest', install_date == date) %>%
#   group_by(region, date) %>%
#   summarise(
#     installs = sum(installs)
#   )
# 
# miso_rev <- miso_metricdata %>%
#   rename(region = country) %>%
#   filter(region != 'Rest') %>%
#   group_by(date, region) %>%
#   summarise(
#     game = as.factor('MISO.Actual'),
#     revenue = sum(revenue),
#     dau = sum(dau),
#     arpdau = revenue / dau
#   ) %>%
#   left_join(miso_installs, by=c('date', 'region')) %>%
#   group_by(region) %>%
#   arrange(date) %>%
#   mutate(
#     cum_rev = cumsum(revenue),
#     cum_installs = cumsum(installs),
#     date = start_date + as.numeric(date - min(date)),
#     month = as.factor(format(date, format='%Y-%m'))
#   )
# 
# rev_combined <- rbind(niso_rev,  miso_rev)
# rev_combined <- rev_combined %>%
#   ungroup() %>%
#   filter(date <= end_date) %>%
#   mutate(game = as.factor(game),
#          month = as.factor(month),
#          region = as.factor(tolower(region)))
# 
# saveRDS(rev_combined, file='niso_miso_actual_daily_predictions_g4_startdate.RData')
