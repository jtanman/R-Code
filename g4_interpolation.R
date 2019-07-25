# By: James Tan

# Date: 3/12/2019

p_load(tidyverse, reshape2, beepr, scales, zoo)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
setwd(datapath)

max_cohortday <- 270
yearend_cohortday <- 183
load(sprintf('%d_final_gamedata_ci.RData', max_cohortday))

gamedata <- gamedata %>%
  filter(
    game %in% c('Marvel.Strike.Force', 'Onmyoji')
  )

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

gamedata <- reduce(list(gamedata, niso_real), rbind)
gamedata <- gamedata %>%
  mutate(
    game = as.character(game)
  ) %>%
  select(game, region, cohortday, retention, arpdau)
retdata <- dcast(gamedata, cohortday ~ game + region, value.var='retention')
arpdaudata <- dcast(gamedata, cohortday ~ game + region, value.var='arpdau')

region_name <- 'west'
game_name <- 'Marvel.Strike.Force'

games <- distinct(filter(gamedata, game != 'NISO.Actual'), game, region)

x <- retdata$NISO.Actual_apac
y <- retdata$Marvel.Strike.Force_apac

interpolate_vectors <- function(x, y, n=10){

  funcs <- apply(rbind(x, y), 2, approxfun, x=c(0, n))
  dfs <- Map(function(f, i) data.frame(scenario=0:n, value=f(0:n), cohortday=i-1), funcs, seq(length(funcs)))
  df <- do.call(rbind, dfs)

  return(df)
}

interpolate_vectors(x, y, n=10)


getG4Interpolated <- function(gamedata, region_name, game_name, n=10, plot_flag=FALSE){
  
  # browser()

  nisodata <- filter(gamedata, region==region_name, game=='NISO.Actual')
  gametemp <- filter(gamedata, region==region_name, game==game_name)

  retdf <- interpolate_vectors(nisodata$retention, gametemp$retention, n=n) %>%
    rename(retention = value)
  arpdaudf <- interpolate_vectors(nisodata$arpdau, gametemp$arpdau, n=n) %>%
    rename(arpdau = value)

  game_levels <- seq(1, 0, length.out=n+1)
  game_levels <- sapply(game_levels, function(x) sprintf('NISO_%s_%s_%s', game_name, percent(x, 0), percent(1-x, 0)))

  tempdata <- retdf %>%
    mutate(
      game = as.factor(game_name),
      scenario = factor(scenario, labels=game_levels),
      region = region_name,
      arpdau = arpdaudf$arpdau
    )

  # browser()

  if(plot_flag){

    ggplot(tempdata, aes(cohortday, retention, color=game)) +
      geom_line()

    ggplot(tempdata, aes(cohortday, arpdau, color=game)) +
      geom_line()
  }

  return(tempdata)
}

temp_game <- tibble()

for(i in 1:nrow(games)){

  r <- games$region[i]
  g <- games$game[i]

  tempdata <- getG4Interpolated(gamedata, r, g, n=10, FALSE)
  # browser()
  temp_game <- rbind(temp_game, tempdata)
}

gamedata <- temp_game
rm(tempdata, temp_game)

summary(gamedata)

levels(gamedata$game)

ggplot(gamedata, aes(cohortday, arpdau, color=scenario)) +
  geom_line() +
  # facet_grid(game ~ region) +
  labs(title = sprintf('G4 Interpolated Scaled ARPDAU')) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format())
  
ggsave_default('g4_interpolated_scaled_arpdau.png')

ggplot(gamedata, aes(cohortday, retention, color=scenario)) +
  geom_line() +
  # facet_grid(game ~ region) +
  labs(title = sprintf('G4 Interpolated Retention')) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::percent)

ggsave_default('onmyoji_interpolated_retention.png')

ggsave_default('g4_interpolated_retention.png')

# gamedata <- gamedata %>%
#   group_by(game, region, scenario)
#   mutate(
#     which(cohortday == yearend_cohortday)
#   )

saveRDS(gamedata, file='g4_interpolated_gamedata.RData')
gamedata <- readRDS('g4_interpolated_gamedata.RData')

write.csv(gamedata, file='g4_interpolated_gamedata.csv', row.names=FALSE)

gamedata <- gamedata %>%
  filter(
    game == 'Marvel.Strike.Force',
    region == 'west'
  )

gamedata <- gamedata %>%
  mutate(
    arpi = retention * arpdau
  ) %>%
  group_by(scenario, game, region) %>%
  arrange(cohortday) %>%
  mutate(
    cumarpi = cumsum(arpi)
  ) %>%
  arrange(
    game, region, scenario
  )

display.brewer.all(type='seq', colorblindFriendly = TRUE)
getPalette <- colorRampPalette(brewer.pal(9, "YlOrRd"))
plt <- getPalette(length(unique(gamedata$scenario)))

ggplot(gamedata, aes(cohortday, arpdau, color=scenario)) +
  geom_line() +
  # facet_grid(game ~ region) +
  labs(title = sprintf('Interpolated ARPDAU Scenarios'), caption='Fitted on App Annie data') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values=plt)

ggsave_default('interpolated_arpdau_scenarios.png', width=9, height=9)

ggplot(gamedata, aes(cohortday, retention, color=scenario)) +
  geom_line() +
  # facet_grid(game ~ region) +
  labs(title = sprintf('Interpolated Retention Scenarios'), caption='Fitted on App Annie data') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values=plt)

ggsave_default('interpolated_retention_scenarios.png', width=9, height=9)

p1 <- ggplot(gamedata, aes(cohortday, cumarpi, color=scenario)) +
  geom_line() +
  labs(title = sprintf('Interpolated CumARPI Scenarios'), caption='Fitted on App Annie data') +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_color_manual(values=plt)
p1

ggsave_default('interpolated_cumarpi_scenarios.png', width=16, height=9)

ggplotly(p1)

ggsave_default('niso_scenarios_interpolation.png')

# plotdata <-

# niso_ret <- retdata$NISO.Actual_apac
# game_ret <- retdata$Marvel.Strike.Force_apac
# game_name <- 'Marvel.Strike.Force'
# region <- 'apac'
#
# x <- filter(gamedata, game=='NISO.Actual', region=='apac') %>% pull(arpdau)
# y <- filter(gamedata, game=='Marvel.Strike.Force', region=='apac') %>% pull(arpdau)







