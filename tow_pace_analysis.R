# James Tan

# Date: 8/29/2017

# ToW Zone Replay Analysis

library(plyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(plotly)
library(reshape2)
library(scales)
library(mltools)
library(RColorBrewer)

set.seed(21)
par.default <- par()

source('~/.Rprofile')
library(rutils)
setwd(datapath)

tow_run_date <- as.Date('2018-3-6')

mydata <- loadData(name='tow_battle_result_99')
mydata$install_date <- as.Date(mydata$install_date)
mydata$date <- as.Date(mydata$date)

# mydata$version <- factor(
#     case_when(
#         mydata$install_date < as.Date('2017-5-25') ~ '.81',
#         mydata$install_date >= as.Date('2017-5-25') & mydata$install_date < as.Date('2017-6-12') ~ '.83',
#         mydata$install_date >= as.Date('2017-6-12') & mydata$install_date < as.Date('2017-6-22') ~ '.84',
#         mydata$install_date >= as.Date('2017-6-22') & mydata$install_date < as.Date('2017-7-28') ~ '.85',
#         mydata$install_date >= as.Date('2017-7-28') & mydata$install_date < as.Date('2017-8-11') ~ '.86',
#         mydata$install_date >= as.Date('2017-8-11') & mydata$install_date < as.Date('2017-8-25') ~ '.87',
#         mydata$install_date >= as.Date('2017-8-25') & mydata$install_date < as.Date('2017-9-5') ~ '.88',
#         mydata$install_date >= as.Date('2017-9-5') & mydata$install_date < as.Date('2017-9-8') ~ '.89',
#         mydata$install_date >= as.Date('2017-9-8') & mydata$install_date < as.Date('2017-9-22') ~ '.90',
#         mydata$install_date >= as.Date('2017-9-22') & mydata$install_date < as.Date('2017-10-5') ~ '.91',
#         mydata$install_date >= as.Date('2017-10-5') & mydata$install_date < as.Date('2017-10-19') ~ '.92',
#         mydata$install_date >= as.Date('2017-10-19') & mydata$install_date < as.Date('2017-11-2') ~ '.93',
#         mydata$install_date >= as.Date('2017-11-2') & mydata$install_date < as.Date('2017-11-17') ~ '.94',
#         mydata$install_date >= as.Date('2017-11-17') & mydata$install_date < as.Date('2017-12-19') ~ '.95',
#         mydata$install_date >= as.Date('2017-12-19') ~ '.96'
        
#     )
# )

mydata$version <- factor('.99')

max(subset(mydata, install_date == as.Date('2017-10-18'))$cohortday)

world <- rep(NA, nrow(mydata))
world[grep('W\\d', mydata$battlezoneid)] <- regmatches(mydata$battlezoneid, regexpr('(?<=W)(\\d+)', mydata$battlezoneid, perl=TRUE))
region <- rep(NA, nrow(mydata))
region[grep('R\\d', mydata$battlezoneid)] <- regmatches(mydata$battlezoneid, regexpr('(?<=R)(\\d+)', mydata$battlezoneid, perl=TRUE))
zone <- rep(NA, nrow(mydata))
zone[grep('Zone\\d', mydata$battlezoneid)] <- regmatches(mydata$battlezoneid, regexpr('(?<=Zone)(\\d+)', mydata$battlezoneid, perl=TRUE))

mydata$world <- factor(world)
mydata$region <- factor(region)
mydata$zone <- factor(zone)

sessiondata <- loadData(name='tow_sessions_99')
sessiondata$install_date <- as.Date(sessiondata$install_date)
sessiondata$date <- as.Date(sessiondata$date)

usertabledata <- loadData(name='tow_users_99')
usertabledata$install_date <- as.Date(usertabledata$install_date)
# usertabledata$version <- factor(
#     case_when(
#         usertabledata$install_date < as.Date('2017-5-25') ~ '.81',
#         usertabledata$install_date >= as.Date('2017-5-25') & usertabledata$install_date < as.Date('2017-6-12') ~ '.83',
#         usertabledata$install_date >= as.Date('2017-6-12') & usertabledata$install_date < as.Date('2017-6-22') ~ '.84',
#         usertabledata$install_date >= as.Date('2017-6-22') & usertabledata$install_date < as.Date('2017-7-28') ~ '.85',
#         usertabledata$install_date >= as.Date('2017-7-28') & usertabledata$install_date < as.Date('2017-8-11') ~ '.86',
#         usertabledata$install_date >= as.Date('2017-8-11') & usertabledata$install_date < as.Date('2017-8-25') ~ '.87',
#         usertabledata$install_date >= as.Date('2017-8-25') & usertabledata$install_date < as.Date('2017-9-5') ~ '.88',
#         usertabledata$install_date >= as.Date('2017-9-5') & usertabledata$install_date < as.Date('2017-9-8') ~ '.89',
#         usertabledata$install_date >= as.Date('2017-9-8') & usertabledata$install_date < as.Date('2017-9-22') ~ '.90',
#         usertabledata$install_date >= as.Date('2017-9-22') & usertabledata$install_date < as.Date('2017-10-5') ~ '.91',
#         usertabledata$install_date >= as.Date('2017-10-5') & usertabledata$install_date < as.Date('2017-10-19') ~ '.92',
#         usertabledata$install_date >= as.Date('2017-10-19') & usertabledata$install_date < as.Date('2017-11-2') ~ '.93',
#         usertabledata$install_date >= as.Date('2017-11-2') & usertabledata$install_date < as.Date('2017-11-17') ~ '.94',
#         usertabledata$install_date >= as.Date('2017-11-17') & usertabledata$install_date < as.Date('2017-12-19') ~ '.95',
#         usertabledata$install_date >= as.Date('2017-12-19') ~ '.96'
#     )
# )
usertabledata$version <- factor('.99')

dates <- seq.Date(min(mydata$install_date), tow_run_date, by='day')
dates_df <- data.frame(dates, dummy=1)

# cohortdays <- usertabledata %>%
#     mutate(dummy = 1) %>%
#     left_join(dates_df, by='dummy') %>%
#     filter(dates >= install_date) %>%
#     mutate(
#         cohortday = dates - install_date
#     ) %>%
#     group_by(version, cohortday) %>%
#     summarise(
#         n = length(udid)
#     )


userdata <- mydata %>%
    filter(battlezoneid != '' & !grepl('tutorialZone', battlezoneid) & matchdurationseconds > 0) %>%
    group_by(udid) %>%
    summarise(
        totalBattles = length(udid),
        totalZones = length(unique(battlezoneid)),
        totalWorlds = length(unique(world))
    )

userdata_all <- mydata %>%
    filter(battlezoneid != '' & !grepl('tutorialZone', battlezoneid) & matchdurationseconds > 0) %>%
    group_by(udid) %>%
    arrange(ts) %>%
    mutate(
        battle_num = row_number(ts),
        distinct_zones = cummax(as.numeric(factor(battlezoneid, levels = unique(battlezoneid)))),
        replays = battle_num - distinct_zones,
        time = as.integer(difftime(ts, install_ts, units='secs')),
        cohortday_24h = as.integer(floor(difftime(ts, install_ts, units='secs') / (24 * 60 * 60)))
    ) %>%
    arrange(udid, ts)

userdata <- userdata_all %>%
    group_by(udid, version, distinct_zones) %>%
    filter(ts == min(ts))

zonedata <- userdata %>%
    group_by(udid, version, distinct_zones) %>%
    summarise(
        cohortday = min(cohortday),
        cohortday_24h = min(cohortday_24h)
    ) %>%
    group_by(version, distinct_zones) %>%
    summarise(
        n = length(udid),
        cohortday_mean = mean(cohortday),
        cohortday_median = median(cohortday),
        cohortday.1 = quantile(cohortday, .1),
        cohortday.2 = quantile(cohortday, .2),
        cohortday.3 = quantile(cohortday, .3),
        cohortday.4 = quantile(cohortday, .4),
        cohortday.5 = quantile(cohortday, .5),
        cohortday.6 = quantile(cohortday, .6),
        cohortday.7 = quantile(cohortday, .7),
        cohortday.8 = quantile(cohortday, .8),
        cohortday.9 = quantile(cohortday, .9)
    ) %>%
    filter(distinct_zones <= 50)

p1 <- ggplot(zonedata, aes(distinct_zones, cohortday_median)) +
    geom_ribbon(aes(ymin=cohortday.1, ymax=cohortday.9, fill='80%'), alpha=.8) +
    geom_ribbon(aes(ymin=cohortday.2, ymax=cohortday.8, fill='60%'), alpha=.8) +
    geom_ribbon(aes(ymin=cohortday.3, ymax=cohortday.7, fill='40%'), alpha=.8) +
    geom_ribbon(aes(ymin=cohortday.4, ymax=cohortday.6, fill='20%'), alpha=.8) +
    geom_path(size=1) +
    facet_wrap(~ version, scales='fixed') +
    scale_fill_brewer(type='seq', direction=-1, palette='Blues', guide = guide_legend(title='% of Players')) +
    # scale_x_continuous(limits=c(0, 50), breaks=seq(0,50,5)) +
    # scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,2)) +
    labs(title = 'Cohortday Percentiles by Distinct Zones Reached') +
    theme(plot.title = element_text(hjust=.5))
p1
ggplotly(p1)

p2 <- ggplot(zonedata, aes(distinct_zones, cohortday_median, group=version, color=version)) +
    geom_path(size=1, alpha=.7) +
    scale_fill_brewer(type='seq', direction=-1, palette='Blues', guide = guide_legend(title='% of Players')) +
    #scale_x_continuous(limits=c(0, 15), breaks=seq(0,15,1)) +
    #scale_y_continuous(limits=c(0, 4), breaks=seq(0,4,1)) +
    labs(title = 'Cohortday by Distinct Zones Reached') +
    #scale_color_brewer() +
    theme(plot.title = element_text(hjust=.5))
p2
ggplotly(p2)


quantiles <- 9
percentiles <- sapply(seq(floor(100/quantiles), floor(100/quantiles) * quantiles, floor(100/quantiles)), function(x){paste0(x, 'th Percentile')})
speedR2data <- userdata %>%
    filter(distinct_zones == 7) %>%
    group_by(version, udid, distinct_zones) %>%
    summarise(
        time = min(time)
    ) %>%
    group_by(version) %>%
    mutate(
        time_bin_numeric = as.numeric(bin_data(time, bins=quantile(time, seq(0, 1, 1/quantiles)))),
        time_bin = factor(time_bin_numeric, labels=percentiles)
    ) %>%
    as.data.frame()

retentiondata <- speedR2data %>%
    left_join(usertabledata, by=c('udid', 'version')) %>%
    mutate(dummy = 1) %>%
    left_join(dates_df, by='dummy') %>%
    filter(dates >= install_date) %>%
    mutate(cohortday = as.integer(dates - install_date)) %>%
    left_join(sessiondata, by=c('udid', 'cohortday')) %>%
    group_by(version, time_bin, cohortday) %>%
    summarise(
        installs = length(unique(udid)),
        dau = length(unique(udid[which(!is.na(ts_start))])),
        retention = dau/installs
    ) %>%
    filter(cohortday <= 15)

p3 <- ggplot(retentiondata, aes(cohortday, retention, group=time_bin, color=time_bin)) +
    geom_line() +
    facet_wrap(~ version) +
    scale_color_brewer() +
    labs(title='Retention by Time (Percentile) to Reach 7 Distinct Zones (Region 2 in .89)') +
    theme(plot.title=element_text(hjust=.5))
p3
ggplotly(p3)

quantiles <- 9
percentiles <- sapply(seq(floor(100/quantiles), floor(100/quantiles) * quantiles, floor(100/quantiles)), function(x){paste0(x, 'th Percentile')})
speedR3data <- userdata %>%
    filter(distinct_zones == 13) %>%
    group_by(version, udid, distinct_zones) %>%
    summarise(
        time = min(time)
    ) %>%
    group_by(version) %>%
    mutate(
        time_bin_numeric = as.numeric(bin_data(time, bins=quantile(time, seq(0, 1, 1/quantiles)))),
        time_bin = factor(time_bin_numeric, labels=percentiles)
    ) %>%
    as.data.frame()

retentiondata <- speedR3data %>%
    left_join(usertabledata, by=c('udid', 'version')) %>%
    mutate(dummy = 1) %>%
    left_join(dates_df, by='dummy') %>%
    filter(dates >= install_date) %>%
    mutate(cohortday = as.integer(dates - install_date)) %>%
    left_join(sessiondata, by=c('udid', 'cohortday')) %>%
    group_by(version, time_bin, cohortday) %>%
    summarise(
        installs = length(unique(udid)),
        dau = length(unique(udid[which(!is.na(ts_start))])),
        retention = dau/installs
    ) %>%
    filter(cohortday <= 15)

p4 <- ggplot(retentiondata, aes(cohortday, retention, group=time_bin, color=time_bin)) +
    geom_line() +
    facet_wrap(~ version) +
    scale_color_brewer() +
    labs(title='Retention by Time (Percentile) to Reach 13 Distinct Zones (Region 3 in .89)') +
    theme(plot.title=element_text(hjust=.5))
p4
ggplotly(p4)



sessions_parsed <- sessiondata %>%
    filter(cohortday < 15) %>%
    select(udid, ts_start, session_id)

zonesessiondata <- userdata %>%
    filter(cohortday < 15) %>%
    select(udid, version, install_date, ts, distinct_zones) %>%
    left_join(sessions_parsed, by=c('udid')) %>%
    filter(ts >= ts_start) %>%
    group_by(udid, version, distinct_zones) %>%
    summarise(
        session_id = max(session_id)
    ) %>%
    group_by(version, distinct_zones) %>%
    summarise(
        n = length(udid),
        session_id_mean = mean(session_id),
        session_id_median = median(session_id),
        session_id.1 = quantile(session_id, .1),
        session_id.2 = quantile(session_id, .2),
        session_id.3 = quantile(session_id, .3),
        session_id.4 = quantile(session_id, .4),
        session_id.5 = quantile(session_id, .5),
        session_id.6 = quantile(session_id, .6),
        session_id.7 = quantile(session_id, .7),
        session_id.8 = quantile(session_id, .8),
        session_id.9 = quantile(session_id, .9)
    ) %>%
    filter(n >= 100)

p5 <- ggplot(zonesessiondata, aes(distinct_zones, session_id_median)) +
    geom_ribbon(aes(ymin=session_id.1, ymax=session_id.9, fill='80%'), alpha=.8) +
    geom_ribbon(aes(ymin=session_id.2, ymax=session_id.8, fill='60%'), alpha=.8) +
    geom_ribbon(aes(ymin=session_id.3, ymax=session_id.7, fill='40%'), alpha=.8) +
    geom_ribbon(aes(ymin=session_id.4, ymax=session_id.6, fill='20%'), alpha=.8) +
    geom_path(size=1) +
    facet_wrap(~ version, scales='fixed') +
    scale_fill_brewer(type='seq', direction=-1, palette='Blues', guide = guide_legend(title='% of Players')) +
    # scale_x_continuous(limits=c(0, 50), breaks=seq(0,50,5)) +
    # scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,2)) +
    labs(title = 'Session Percentiles by Distinct Zones Reached') +
    theme(plot.title = element_text(hjust=.5))
p5
ggplotly(p5)

myblue <- brewer.pal(n=3, 'Blues')[1:3]
p6 <- ggplot(zonesessiondata, aes(distinct_zones, session_id_median, group=version, color=version)) +
    geom_path(size=1) +
    scale_x_continuous(limits=c(0, 30), breaks=seq(0,30,2)) +
    scale_y_continuous(limits=c(0, 50), breaks=seq(0,50,5)) +
    labs(title = 'Session by Distinct Zone Reached') +
    theme(plot.title = element_text(hjust=.5)) +
    scale_color_manual(values=myblue)
p6
ggplotly(p6)

timedata <- userdata %>%
    mutate(time = time / (60 * 60)) %>%
    group_by(version, distinct_zones) %>%
    summarise(
        n = length(udid),
        time_mean = mean(time),
        time_median = median(time),
        time.1 = quantile(time, .1),
        time.2 = quantile(time, .2),
        time.3 = quantile(time, .3),
        time.4 = quantile(time, .4),
        time.5 = quantile(time, .5),
        time.6 = quantile(time, .6),
        time.7 = quantile(time, .7),
        time.8 = quantile(time, .8),
        time.9 = quantile(time, .9)
    ) %>%
    filter(n >= 100)

p7 <- ggplot(timedata, aes(distinct_zones, time_median)) +
    geom_ribbon(aes(ymin=time.1, ymax=time.9, fill='80%'), alpha=.8) +
    geom_ribbon(aes(ymin=time.2, ymax=time.8, fill='60%'), alpha=.8) +
    geom_ribbon(aes(ymin=time.3, ymax=time.7, fill='40%'), alpha=.8) +
    geom_ribbon(aes(ymin=time.4, ymax=time.6, fill='20%'), alpha=.8) +
    geom_path(size=1) +
    facet_wrap(~ version, scales='fixed') +
    scale_fill_brewer(type='seq', direction=-1, palette='Blues', guide = guide_legend(title='% of Players')) +
    # scale_x_continuous(limits=c(0, 50), breaks=seq(0,50,5)) +
    # scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,2)) +
    labs(title = 'Time (Hours Since Install) Percentiles by Distinct Zones Reached') +
    theme(plot.title = element_text(hjust=.5))
p7
ggplotly(p7)

p8 <- ggplot(timedata, aes(distinct_zones, time_median, group=version, color=version)) +
    geom_path(size=1) +
    scale_x_continuous(limits=c(0, 30), breaks=seq(0,30,5)) +
    scale_y_continuous(limits=c(0, 250), breaks=seq(0,250,25)) +
    labs(title = 'Time (Hours Since Install) by Distinct Zone Reached') +
    theme(plot.title = element_text(hjust=.5)) +
    scale_color_manual(values=myblue)
p8
ggplotly(p8)












