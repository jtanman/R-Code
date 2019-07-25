# By: James Tan

# Date: 2/21/2019

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales, tools)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
setwd(datapath)

mydata <- tibble()
gamedata <- tibble()
splitdata <- tibble()

temp_my <- tibble()
temp_game <- tibble()

startday <- 0
max_cohortday <- 270
end_cohortday <- 270
split <- FALSE
plot_flag <- TRUE
bounded <- TRUE

regions <- c('west', 'apac')
cohortdays <- c(270)



for(r in regions){

  dldata <- read.csv(sprintf('crpg_%s_all_parsed.csv', r))
  games <- grep('^DAU.', names(dldata), value=TRUE)
  games <- sapply(games, function(x) sub('^DAU.', '', x))

  for(g in games){
    for(c in cohortdays){
      print(sprintf('Region: %s, Game: %s, Cohortday: %d', r, g, c))
      # game_results <- runGame(dldata, g, r, startday, c, end_cohortday, split, bounded, plot_flag)

      tryCatch({
        # mydata <- rbind(mydata, game_results[['mydata']])
        # gamedata <- rbind(gamedata, game_results[['gamedata']])
        # if(split){
        #   splitdata <- rbind(splitdata, game_results[['splitdata']])
        # }

        load(paste(g, r, startday, bounded, c, 'fit.RData', sep='_'))
        temp_my <- rbind(temp_my, mydata)
        temp_game <- rbind(temp_game, gamedata)

      }, error = function(e){
        print(paste('Error on', r, g, c, e, sep=' '))
      }, finally = {

      })

    }
  }
}

mydata <- temp_my
gamedata <- temp_game

save(gamedata, mydata, file=sprintf('%d_%d_gamedata_mydata.RData', startday, max_cohortday))

startday <- 2
load(file=sprintf('%d_%d_gamedata_mydata.RData', startday, max_cohortday))

gamedata <- filter(gamedata,! game %in% c('NISO.Actual'))
mydata <- filter(mydata, game != 'NISO.Actual')

retdata <- dcast(gamedata, cohortday ~ region + game, value.var='retention')
arpdaudata <- dcast(gamedata, cohortday ~ region + game, value.var='arpdau')
write.csv(retdata, sprintf('%d_%d_fitted_retention.csv', startday, max_cohortday), row.names=FALSE)
write.csv(arpdaudata, sprintf('%d_%d_fitted_arpdau.csv', startday, max_cohortday), row.names=FALSE)

ggplot(gamedata, aes(cohortday, arpdau, color=game)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted ARPDAU', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format())

ggsave_default(sprintf('%d_%d_fitted_arpdau.png', startday, max_cohortday))

ggplot(gamedata, aes(cohortday, retention, color=game)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted Retention', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::percent)

ggsave_default(sprintf('%d_%d_fitted_retention.png', startday, max_cohortday))

plotdata <- melt(mydata, id.vars=c('game', 'region', 'month', 'days_since_launch'))

dauplotdata <- filter(plotdata, variable %in% c('dau', 'dau_pred'))
ggplot(dauplotdata, aes(days_since_launch, value, color=game, group=interaction(game, variable), lty=variable)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted DAU', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(sprintf('%d_%d_fitted_dau.png', startday, max_cohortday))

revplotdata <- filter(plotdata, variable %in% c('rev', 'rev_pred'))

ggplot(revplotdata, aes(days_since_launch, value, color=game, group=interaction(game, variable), lty=variable)) +
  geom_line() +
  facet_wrap(~region) +
  labs(title = sprintf('%d Fitted Rev', max_cohortday)) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(sprintf('%d_%d_fitted_rev.png', startday, max_cohortday))

#
# for(r in regions){
#   if(r == 'apac'){
#     region_print <- toupper(region)
#   }else{
#     region_print <- toTitleCase(region)
#   }
#   for(c in cohortdays){
#     plotdata <- filter(splitdata, region == r, max_cohortday == c)
#     plotdata <- melt(plotdata, id.vars=c('game', 'region', 'max_cohortday', 'end_cohortday', 'cohortday'))
#     ggplot(filter(plotdata, grepl('retention', variable)), aes(cohortday, value, group=variable, color=variable)) +
#       geom_line() +
#       facet_wrap(~game, nrow=2) +
#       labs(title = paste(region_print, c, end_cohortday, 'Retention Split')) +
#       theme(plot.title = element_text(hjust=.5))
#
#     ggsave_default(paste(r, c, end_cohortday, 'retention_split.png', sep='_'))
#
#     ggplot(filter(plotdata, grepl('arpdau', variable)), aes(cohortday, value, group=variable, color=variable)) +
#       geom_line() +
#       facet_wrap(~game, nrow=2, scales='free_y') +
#       labs(title = paste(region_print, c, end_cohortday, 'ARPDAU Split')) +
#       theme(plot.title = element_text(hjust=.5))
#
#     ggsave_default(paste(r, c, end_cohortday, 'arpdau_split.png', sep='_'))
#   }
# }
#
#
#


temp_game <- tibble()
temp_my <- tibble()

startdays <- c(0, 2)
for(s in startdays){
  load(sprintf('%d_%d_gamedata_mydata.RData', s, max_cohortday))
  gamedata$startday <- s
  mydata$startday <- s

  temp_game <- rbind(temp_game, gamedata)
  temp_my <- rbind(temp_my, mydata)
}

mydata <- temp_my
gamedata <- temp_game
rm(temp_my, temp_game)

cohortdata <- readRDS('niso_cohortdata_country.RData')
niso_real <- cohortdata %>%
  select(country, cohortday, retention, arpdau) %>%
  filter(country != 'Rest') %>%
  rename(region = country) %>%
  mutate(
    game = 'NISO.Actual',
    startday = 'Actual',
    region = tolower(region)
  )

gamedata <- rbind(gamedata, niso_real)

mydata$startday <- as.factor(mydata$startday)
gamedata$startday <- as.factor(gamedata$startday)

ggplot(filter(gamedata, cohortday <= 7), aes(cohortday, retention, color=startday)) +
  geom_line(alpha=1) +
  facet_grid(game ~ region)

ggsave_default('all_fitted_retention.png')

ggplot(filter(gamedata), aes(cohortday, arpdau, color=startday)) +
  geom_line(alpha=1) +
  facet_grid(game ~ region, scales='free_y')

ggsave_default('all_fitted_arpdau.png')

ggplot(filter(gamedata, game == 'Final.Fantasy'), aes(cohortday, retention, color=startday)) +
  geom_line(alpha=1) +
  geom_point() +
  facet_wrap(~region)

ggsave_default('niso_public_fitted_retention.png')


summary(gamedata)

retdata <- dcast(gamedata, cohortday ~ region + game + startday, value.var = 'retention')

retdata <- dcast(filter(gamedata, game %in% FF_GAMES, region == 'apac'), cohortday ~ region +game + startday, value.var = 'retention')
msedata <- apply(retdata, MARGIN=2, FUN = function(x) sqrt(mean((x - retdata$apac_NISO.Actual_Actual)^2)))
msedata[order(msedata)]

retdata <- dcast(filter(gamedata, game %in% FF_GAMES, region == 'west'), cohortday ~ region +game + startday, value.var = 'retention')
msedata <- apply(retdata, MARGIN=2, FUN = function(x) sqrt(mean((x - retdata$west_NISO.Actual_Actual)^2)))
msedata[order(msedata)]

retdata <- dcast(filter(gamedata, game %in% FF_GAMES, startday == 0 | startday == 'Actual'), cohortday ~ region + game + startday, value.var = 'retention')
arpdaudata <- dcast(filter(gamedata, game %in% FF_GAMES, startday == 0 | startday == 'Actual'), cohortday ~ region +game + startday, value.var = 'arpdau')
write.csv(retdata, 'niso_fitted_retention.csv', row.names=FALSE)
write.csv(arpdaudata, 'niso_fitted_arpdau.csv', row.names=FALSE)

nisodata <- filter(gamedata, (game == 'Final.Fantasy' & startday == 0) | startday == 'Actual')
nisodata <- filter(gamedata, game %in% FF_GAMES)

p1 <- ggplot(nisodata, aes(cohortday, retention, color=interaction(game, startday))) +
  geom_line() +
  geom_point() +
  facet_wrap(~region)

ggplotly(p1)

ggsave_default('niso_public_fitted_retention.png')

p1 <- ggplot(nisodata, aes(cohortday, arpdau, color=interaction(game, startday))) +
  geom_line() +
  geom_point() +
  facet_wrap(~region)
p1
ggplotly(p1)

ggsave_default('niso_public_fitted_arpdau.png')
