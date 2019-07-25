# By: James Tan

# Date: 2/5/2019

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales, zoo)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
setwd(datapath)

region <- 'apac'
if(region == 'apac'){
  region_print <- toupper(region)
}else{
  region_print <- toTitleCase(region)
}

dldata <- read.csv(sprintf('crpg_%s_all_parsed.csv', region))

games <- grep('^DAU.', names(dldata), value=TRUE)
games <- sapply(games, function(x) sub('^DAU.', '', x))


# installdata <- dldata[,grep('Daily.Downloads', names(dldata))]
# daudata <- dldata[,grep('^DAU\\.', names(dldata))]

# ffdata <- tibble()
# game <- games[1]
game <- 'NISO.Actual'
startday <- 1
max_cohortday <- 270
end_cohortday <- 270
split <- FALSE
plot_flag <- TRUE

# game_results <- runGame(g, region, max_cohortday, end_cohortday, split)

mydata <- parseData(dldata, game, region, max_cohortday)

max_cohortday <- max(mydata$days_since_launch)

# plotdata <- mydata %>% select(days_since_launch, installs, dau)
# plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='num_users')
# ggplot(plotdata, aes(days_since_launch, num_users, color=metric)) +
#   geom_line() +
#   labs(title = paste(game, region_print, max_cohortday, 'Installs and DAU')) +
#   theme(plot.title = element_text(hjust=.5))

alpha <- 1
beta <- 1

# pl.beta(alpha, beta)




retention_fit <- fitRetention(mydata, max_cohortday, startday=startday, bounded=TRUE, na.rm=FALSE)
retention_opt <- retention_fit$retention
# pl.beta(retention_fit$values[1], retention_fit$values[2])

# cohortdata <- readRDS('niso_cohortdata_country.RData')
# mydata$dau_pred_actual_retention <- getDAU(mydata, 270, filter(cohortdata, country=='APAC')$retention)
# mean(abs(mydata$dau - mydata$dau_pred_actual_retention), na.rm=TRUE)
# mean((mydata$dau - mydata$dau_pred_actual_retention)^2, na.rm=TRUE)
# 
# mydata$dau_pred_opt <- getDAU(mydata, 270, retention_opt)
# mean(abs(mydata$dau - mydata$dau_pred_opt), na.rm=TRUE)
# mean((mydata$dau - mydata$dau_pred_opt)^2, na.rm=TRUE)
# 
# mydata <- mydata %>%
#   mutate(
#     retention_actual = filter(cohortdata, country=='APAC')$retention,
#     retention_opt = retention_opt
#   )
# 
# mydata <- select(mydata, -dau_pred)
# 
# plotdata <- melt(mydata, id.vars=c('game', 'region', 'month', 'days_since_launch'))
# 
# ggplot(filter(plotdata, grepl('^retention', variable)), aes(days_since_launch, value, color=variable)) +
#   geom_line()
# 
# ggsave_default('niso_apac_retention_comparison_opt_actual.png')
# 
# ggplot(filter(plotdata, grepl('^dau', variable)), aes(days_since_launch, value, color=variable)) +
#   geom_line()
# 
# ggsave_default('niso_apac_dau_comparison_opt_actual.png')

retentiondata <- tibble(cohortday = seq(0, max_cohortday), retention = retention_opt)
ggplot(retentiondata, aes(cohortday, retention)) +
  geom_line() +
  geom_point() +
  labs(title = paste(game, region_print, max_cohortday, 'Retention')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(paste(game, region, max_cohortday, 'retention.png', sep='_'))

mydata$dau_pred <- getDAU(mydata, max_cohortday, retention_opt)

plotdata <- mydata %>% select(days_since_launch, installs, dau, dau_pred) %>% rename(dau_actual=dau, dau_pred=dau_pred)
plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='num_users')
ggplot(plotdata, aes(days_since_launch, num_users, color=metric)) +
  geom_line() +
  labs(title = paste(game, region_print, max_cohortday, 'Installs and DAU')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(paste(game, region, max_cohortday, 'installs_dau.png', sep='_'))

# Fit ARPDAU

arpdau_fit <- fitARPDAU(mydata, retention_opt, max_cohortday, method='all')

mydata$rev_pred <- getRev(arpdau_fit$arpdau, retention_opt, max_cohortday, mydata)
gamedata <- tibble(game=game, region=region, cohortday=seq(0, max_cohortday), retention=retention_opt, arpdau=arpdau_fit$arpdau)

ggplot(gamedata, aes(cohortday, arpdau)) +
  geom_point() +
  geom_line() +
  labs(title = paste(game, region_print, max_cohortday, 'ARPDAU')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(paste(game, region, max_cohortday, 'arpdau.png', sep='_'))

plotdata <- mydata %>% select(days_since_launch, rev, rev_pred) %>% rename(rev_actual=rev)
plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='dollars')
ggplot(plotdata, aes(days_since_launch, dollars, color=metric)) +
  geom_line() +
  labs(title = paste(game, region_print, max_cohortday, 'Revenue')) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default(paste(game, region, max_cohortday, 'revenue.png', sep='_'))

save(mydata, gamedata, retention_fit, arpdau_fit, file=paste(game, region, max_cohortday, 'fit.RData', sep='_'))

if(split){
  
  # browser()
  retention_pre <- getRetention(end_cohortday, retention_fit$values[1], retention_fit$values[2])
  dau_pre <- getDAU(mydata, end_cohortday, retention_pre)
  
  arpdau_pre <- arpdau_fit$fn(seq(0, end_cohortday), arpdau_fit$values)
  rev_pre <- getRev(arpdau_pre, retention_pre, end_cohortday, mydata)
  
  installs_pre <- sum(mydata$installs)
  
  load(paste(game, region, end_cohortday, 'fit.RData', sep='_'))
  
  installs_combined <- sum(mydata$installs)
  installs_post <- installs_combined - installs_pre
  
  retention_combined <- retention_fit$retention
  retention_post <-  (installs_combined * retention_combined - installs_pre * retention_pre) / installs_post
  retention_post <- ifelse(retention_post < 0, 0, retention_post)
  
  arpdau_combined <- arpdau_fit$arpdau
  arpdau_post <-  (installs_combined * arpdau_combined - installs_pre * arpdau_pre) / installs_post
  arpdau_post <- ifelse(arpdau_post < 0, 0, arpdau_post)
  
  splitdata <- tibble(game=game, region=region, max_cohortday=max_cohortday, end_cohortday = end_cohortday,
                      cohortday = seq(0, end_cohortday), retention_pre = retention_pre, retention_post = retention_post,
                      retention_combined = retention_combined, arpdau_pre = arpdau_pre, arpdau_post = arpdau_post,
                      arpdau_combined = arpdau_combined)
  
  if(plot_flag){
    plotdata <- melt(splitdata, id.vars=c('game', 'region', 'max_cohortday', 'end_cohortday', 'cohortday'))
    
    ggplot(filter(plotdata, grepl('retention', variable)), aes(cohortday, value, color=variable)) +
      geom_line() +
      labs(title = paste(game, region_print, max_cohortday, end_cohortday, 'Retention Split')) +
      theme(plot.title = element_text(hjust=.5))
    
    ggsave_default(paste(game, region, max_cohortday, end_cohortday, 'retention_split.png', sep='_'))
    
    ggplot(filter(plotdata, grepl('arpdau', variable)), aes(cohortday, value, color=variable)) +
      geom_line() +
      labs(title = paste(game, region_print, max_cohortday, end_cohortday, 'ARPDAU Split')) +
      theme(plot.title = element_text(hjust=.5))
    
    ggsave_default(paste(game, region, max_cohortday, end_cohortday, 'arpdau_split.png', sep='_'))
  }
  
  save(splitdata, file=paste(game, region, max_cohortday, end_cohortday, 'split.RData', sep='_'))
  
  load(paste(game, region, max_cohortday, 'fit.RData', sep='_'))
  return(list(mydata=mydata, gamedata=gamedata, splitdata = splitdata, retention_fit=retention_fit, arpdau_fit=arpdau_fit))
}







