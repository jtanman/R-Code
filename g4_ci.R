# By: James Tan

# Date: 3/10/2019

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales, zoo)

source('~/.Rprofile')
source('~/MZ/R Code/g4_public_fit_helper.R')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
setwd(datapath)

max_cohortday <- 270
load(file=sprintf('%d_final_gamedata_mydata.RData', max_cohortday))

gamedata <- filter(gamedata,! game %in% c('NISO.Actual'))
mydata <- filter(mydata, game != 'NISO.Actual')

cohortdata <- readRDS('niso_cohortdata_country.RData')
niso_real <- cohortdata %>%
  select(country, cohortday, retention, arpdau) %>%
  filter(country != 'Rest') %>%
  rename(region = country) %>%
  mutate(
    game = 'NISO.Actual',
    region = tolower(region)
  )

gamedata <- rbind(gamedata, niso_real)

retdata <- dcast(gamedata, cohortday ~ region + game, value.var='retention')
arpdaudata <- dcast(gamedata, cohortday ~ region + game, value.var='arpdau')

errordata <- tibble(
  cohortday = retdata$cohortday,
  apac_retention_error = abs(1 - retdata$apac_NISO.Actual / retdata$apac_Final.Fantasy),
  west_retention_error = abs(1 - retdata$west_NISO.Actual / retdata$west_Final.Fantasy),
  apac_arpdau_error = abs(1 - arpdaudata$apac_NISO.Actual / arpdaudata$apac_Final.Fantasy),
  west_arpdau_error = abs(1 - arpdaudata$west_NISO.Actual / arpdaudata$west_Final.Fantasy)
)

plotdata <- melt(errordata, id.vars='cohortday')
ggplot(plotdata, aes(cohortday, value, color=variable)) +
  geom_point()



gamedata <- gamedata %>%
  group_by(game, region) %>%
  mutate(
    # retention_lower = ifelse(region == 'apac', retention * (1 - errordata$apac_retention_error[1:length(retention)]), retention * (1 - errordata$west_retention_error[1:length(retention)])),
    # retention_upper = ifelse(region == 'apac', retention * (1 + errordata$apac_retention_error[1:length(retention)]), retention * (1 + errordata$west_retention_error[1:length(retention)])),
    # arpdau_lower = ifelse(region == 'apac', arpdau * (1 - errordata$apac_arpdau_error[1:length(arpdau)]), arpdau * (1 - errordata$west_arpdau_error[1:length(arpdau)])),
    # arpdau_upper = ifelse(region == 'apac', arpdau * (1 + errordata$apac_arpdau_error[1:length(arpdau)]), arpdau * (1 + errordata$west_arpdau_error[1:length(arpdau)])),
    retention_lower = pmax(0, pmin(1, ifelse(region == 'apac', retention * (1 - errordata$apac_retention_error[1:length(retention)]), retention * (1 - errordata$west_retention_error[1:length(retention)])))),
    retention_upper = pmax(0, pmin(1, ifelse(region == 'apac', retention * (1 + errordata$apac_retention_error[1:length(retention)]), retention * (1 + errordata$west_retention_error[1:length(retention)])))),
    arpdau_lower = pmax(0, ifelse(region == 'apac', arpdau * (1 - errordata$apac_arpdau_error[1:length(arpdau)]), arpdau * (1 - errordata$west_arpdau_error[1:length(arpdau)]))),
    arpdau_upper = pmax(0, ifelse(region == 'apac', arpdau * (1 + errordata$apac_arpdau_error[1:length(arpdau)]), arpdau * (1 + errordata$west_arpdau_error[1:length(arpdau)]))),
  ) %>%
  ungroup()

summary(gamedata)

# ggplot(filter(gamedata, cohortday <= 14), aes(cohortday, retention, ymin=retention_lower, ymax=retention_upper, color='Fitted Retention', fill='CI')) +
#   geom_ribbon(alpha = .8, color='red') +
#   geom_line(lwd=1) +
#   facet_grid(game ~ region) +
#   scale_fill_manual(values='red') +
#   scale_color_manual(values='black') +
#   labs(title = sprintf('Fitted Retention and Confidence Intervals')) +
#   theme(plot.title = element_text(hjust=.5))

plotdata <- gamedata %>%
  filter(cohortday <= 100,
         game != 'NISO.Actual') %>%
  rename(upper = retention_upper, lower=retention_lower)
ggplot(plotdata, aes(cohortday, retention)) +
  geom_ribbon(aes(ymax=upper, ymin=retention), alpha=.4, color='green', fill='green') +
  geom_ribbon(aes(ymax=retention, ymin=lower), alpha=.4, color='red', fill='red') +
  geom_line() +
  facet_grid(region ~ game, scales='free_y') +
  labs(title = sprintf('Fitted Retention and Confidence Intervals')) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::percent_format())

ggsave_default(gsub(' ', '_', tolower('Fitted Retention and Confidence Intervals.png')))

# ggplot(filter(gamedata), aes(cohortday, arpdau, ymin=arpdau_lower, ymax=arpdau_upper, color='Fitted ARPDAU', fill='CI')) +
#   geom_ribbon(alpha = .8, color='red') +
#   geom_line(lwd=1) +
#   facet_grid(game ~ region, scale='free_y') +
#   scale_fill_manual(values='red') +
#   scale_color_manual(values='black') +
#   labs(title = sprintf('Fitted ARPDAU and Confidence Intervals')) +
#   theme(plot.title = element_text(hjust=.5))
#
# ggsave_default(gsub(' ', '_', tolower('Fitted ARPDAU and Confidence Intervals.png')))

plotdata <- gamedata %>%
  # filter(cohortday <= 100) %>%
  rename(upper = arpdau_upper, lower=arpdau_lower) %>%
  filter(game != 'NISO.Actual')
ggplot(plotdata, aes(cohortday, arpdau)) +
  geom_ribbon(aes(ymax=upper, ymin=arpdau), alpha=.4, color='green', fill='green') +
  geom_ribbon(aes(ymax=arpdau, ymin=lower), alpha=.4, color='red', fill='red') +
  geom_line() +
  facet_grid(region ~ game, scales='free_y') +
  labs(title = sprintf('Fitted Arpdau and Confidence Intervals')) +
  theme(plot.title = element_text(hjust=.5)) +
  scale_y_continuous(labels = scales::dollar_format())

ggsave_default(gsub(' ', '_', tolower('Fitted Arpdau and Confidence Intervals.png')))

save(gamedata, mydata, file=sprintf('%d_final_gamedata_ci.RData', max_cohortday))

write.csv(gamedata, 'fitted_retention_arpdau_with_CI.csv', row.names=FALSE)

# revenue calculation




