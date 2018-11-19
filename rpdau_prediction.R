# rpdau prediction

# By: James Tan

# Date: 5/25/2018

library(readr)
library(ggplot2)
library(reshape2)
library(tseries)
library(lmtest)
library(MSBVAR)
library(vtreat)
library(caret)
library(randomForest)
library(gbm)
library(gridExtra)
library(lubridate)
library(dplyr)


source('~/.Rprofile')
setwd(datapath)

rpdaudata <- read.csv('rpdau_isogroup.csv')

sapply(rpdaudata, class)

rpdaudata <- rpdaudata %>%
  mutate(
    current_isogroup = as.factor(current_isogroup),
    install_date = case_when(
      current_isogroup == '21' ~ as.Date('2017-11-1'),
      current_isogroup == '23' ~ as.Date('2017-12-1'),
      current_isogroup == '25' ~ as.Date('2018-1-1'),
      current_isogroup == '27' ~ as.Date('2018-2-1'),
      current_isogroup == '29' ~ as.Date('2018-3-1'),
      current_isogroup == '31' ~ as.Date('2018-4-1'),
      current_isogroup == '33' ~ as.Date('2018-5-1'),
    ),
    date = install_date + Days.Since.ISO.Open
  ) %>%
  dplyr::rename(
    install_isogroup = current_isogroup,
    days_since_iso_open = Days.Since.ISO.Open,
  ) %>%
  arrange(install_isogroup, date)

installdata <- read.csv('installs_isogroup.csv')
sapply(installdata, class)

installdata <- installdata %>%
  filter(as.numeric(as.character(install_isogroup)) >= 21) %>%
  mutate(
    install_isogroup = as.factor(install_isogroup),
    install_date = as.Date(install_date, format='%m/%d/%y'),
  ) %>%
  filter(
    !(install_isogroup == 21 & install_date < as.Date('2017-11-1')),
    !(install_isogroup == 23 & install_date < as.Date('2017-12-1')),
    !(install_isogroup == 25 & install_date < as.Date('2018-1-1')),
    !(install_isogroup == 27 & install_date < as.Date('2018-2-1')),
    !(install_isogroup == 29 & install_date < as.Date('2018-3-1')),
    !(install_isogroup == 31 & install_date < as.Date('2018-4-1')),
    !(install_isogroup == 33 & install_date < as.Date('2018-5-1')),
  )%>%
  group_by(install_isogroup) %>%
  arrange(install_date) %>%
  mutate(
    install_total = cumsum(installs)
  ) %>%
  rename(
    date = install_date
  )  %>%
  arrange(install_isogroup, date)


mergedata <- installdata %>%
  left_join(rpdaudata, by=c('install_isogroup', 'date')) %>%
  arrange(install_isogroup, date) %>%
  mutate(
    rpdau_per_install = rpdau / install_total,
    type = 'Normal'
  )

prefebdata <- mergedata %>%
  filter(date <= as.Date('2018-2-12'))

meandata <- prefebdata %>%
  group_by(days_since_iso_open) %>%
  summarise(
    rpdau_per_install = sum(rpdau) / sum(install_total),
    install_total = mean(install_total),
    installs = mean(installs),
    rpdau = rpdau_per_install * install_total,
    install_isogroup = 'Average',
    type = 'Average',
  )
plotdata <- select(prefebdata, days_since_iso_open, rpdau_per_install, installs, install_total, install_isogroup, type)
plotdata <- bind_rows(plotdata, meandata)



ggplot(meandata, aes(days_since_iso_open, rpdau_per_install)) +
  geom_line()

cbPalette <- c('#7fc97f','#beaed4','#fdc086','#ffff99','#000000')
legend_title <- 'Isogroup/Average'
ggplot(prefebdata, aes(days_since_iso_open, rpdau_per_install, color=install_isogroup, group=install_isogroup)) +
  geom_line(aes(lty='Normal')) +
  geom_line(data=meandata, aes(color='Average', group='Average', lty='Average')) +
  labs(title='RPDAU / Total Installs', caption='Pre Feb 12 Data') +
  scale_linetype_manual(name=legend_title, values=c('Normal' = 1, 'Average' = 2)) +
  scale_color_manual(name=legend_title, values=cbPalette) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 4), 2))))

ggplot(plotdata, aes(days_since_iso_open, rpdau_per_install, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='RPDAU / Total Installs', caption='Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Average' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                   linetype = c(rep(1, 4), 2))))

ggplot(plotdata, aes(days_since_iso_open, installs, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='Installs', caption='Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Average' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 4), 2))))

ggplot(plotdata, aes(days_since_iso_open, install_total, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='Cumulative Installs', caption='Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Average' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 4), 2))))

ggplot(meandata, aes(days_since_iso_open, rpdau)) +
  geom_line() +
  labs(title='RPDAU 200+ Prediction', caption='Assuming similar installs rate and rpdau/total installs rate')

csvdata <- select(meandata, -install_isogroup, -type)
csvdata <- head(csvdata, -1)
write.csv(csvdata, 'rpdau_baseline.csv', row.names=FALSE)


# vs actuals
meandata <- meandata %>%
  mutate(
    install_isogroup = 'Predicted',
    type = 'Predicted'
  )

newdata <- mergedata %>%
  filter(install_isogroup == 27 | install_isogroup == 29 | install_isogroup == 31)

newdata <- bind_rows(newdata, meandata)

cbPalette <- c('#7fc97f','#beaed4','#fdc086', '#000000')
ggplot(newdata, aes(days_since_iso_open, install_total, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='Cumulative Installs (Actual vs Predicted)', caption='Predicted using Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Predicted' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 3), 2))))

ggplot(newdata, aes(days_since_iso_open, rpdau, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='RPDAU 200+ (Actual vs Predicted)', caption='Predicted using Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Predicted' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 3), 2))))

newdata$rpdau_per_install[which(newdata$days_since_iso_open == 1 & newdata$install_isogroup == 31)] <- 0
ggplot(newdata, aes(days_since_iso_open, rpdau_per_install, color=install_isogroup, group=install_isogroup, lty=type)) +
  geom_line() +
  labs(title='RPDAU 200+/Total Installs (Actual vs Predicted)', caption='Predicted using Pre Feb 12 Data') +
  scale_color_manual(values=cbPalette) +
  scale_linetype_manual(values=c('Normal' = 1, 'Predicted' = 2)) +
  guides(linetype=FALSE,
         color=guide_legend(override.aes = list(color = cbPalette,
                                                linetype = c(rep(1, 3), 2))))





