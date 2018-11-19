# By: James Tan

# Date: 4/30/2018

library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

source('~/.Rprofile')
setwd(datapath)

mydata <- read.csv('GameFeatureProvince_2018-05-01_1843.csv')

mydata$cohort_id <- as.factor(mydata$cohort_id)

mydata$activate_ts <- ifelse(mydata$activate_ts == 0, NA, mydata$activate_ts)
# mydata$deactivate_ts <- ifelse(mydata$deactivate_ts == 0, NA, mydata$deactivate_ts)

mydata$activate_ts <- as.POSIXct(mydata$activate_ts, origin='1970-01-01', tz='UTC')
# mydata$deactivate_ts  <- as.POSIXct(mydata$deactivate_ts, origin='1970-01-01', tz='UTC')

mydata <- arrange(mydata, cohort_id, activate_ts)

sapply(mydata, class)

kingdom_metadata <- read.csv('kingdom_metadata.csv')
kingdom_metadata <- filter(kingdom_metadata, game_id == 23)

sapply(kingdom_metadata, class)
kingdom_metadata$created_ts <- as.POSIXct(as.character(kingdom_metadata$created_ts), tz='UTC', format='%Y-%m-%d %H:%M:%S')

kingdom_metadata <- select(kingdom_metadata, c(kingdom, iso_group, created_ts))

feature_data <- merge(mydata, kingdom_metadata, by.x = 'cohort_id', by.y = 'kingdom')
feature_data$activate_age <- ifelse(difftime(feature_data$activate_ts, feature_data$created_ts, units='days') < 0, 0, difftime(feature_data$activate_ts, feature_data$created_ts, units='days'))
# feature_data$deactivate_age <- ifelse(difftime(feature_data$deactivate_ts,  feature_data$created_ts, units='days') < 0, 0, difftime(feature_data$deactivate_ts,  feature_data$created_ts, units='days'))

names(feature_data)
feature_isogroup_data <- feature_data %>%
  group_by(feature_name, iso_group) %>%
  summarise(
    activate_age_min = round(min(activate_age, na.rm=TRUE)),
    activate_age_max = round(max(activate_age, na.rm=TRUE)),
    # deactivate_age_min = round(min(deactivate_age, na.rm=TRUE)),
    # deactivate_age_max = round(max(deactivate_age, na.rm=TRUE)),
    avg_age = mean(activate_age, na.rm=TRUE),
    median_age = median(activate_age, na.rm=TRUE),
    num_provinces = n()
  ) %>%
  mutate(
    activate_age_min = ifelse(!is.finite(activate_age_min), NA, activate_age_min),
    activate_age_max = ifelse(!is.finite(activate_age_max), NA, activate_age_max),
    # deactivate_age_min = ifelse(!is.finite(deactivate_age_min), NA, deactivate_age_min),
    # deactivate_age_max = ifelse(!is.finite(deactivate_age_max), NA, deactivate_age_max)
  )

feature_all_data <- feature_data %>%
  group_by(feature_name) %>%
  summarise(
    activate_age_min = round(min(activate_age, na.rm=TRUE)),
    activate_age_max = round(max(activate_age, na.rm=TRUE)),
    # deactivate_age_min = round(min(deactivate_age, na.rm=TRUE)),
    # deactivate_age_max = round(max(deactivate_age, na.rm=TRUE)),
    avg_age = mean(activate_age, na.rm=TRUE),
    median_age = median(activate_age, na.rm=TRUE),
    num_provinces = n()
  ) %>%
  mutate(
    activate_age_min = ifelse(!is.finite(activate_age_min), NA, activate_age_min),
    activate_age_max = ifelse(!is.finite(activate_age_max), NA, activate_age_max),
    # deactivate_age_min = ifelse(!is.finite(deactivate_age_min), NA, deactivate_age_min),
    # deactivate_age_max = ifelse(!is.finite(deactivate_age_max), NA, deactivate_age_max),
  )

feature_data %>% filter(as.Date(activate_ts) == as.Date('2018-4-1'))

release_melt_data <- melt(feature_isogroup_data, id.vars = c('feature_name', 'iso_group'))

release_cast_data <- dcast(release_melt_data, feature_name ~ variable + iso_group)

all_data <- feature_all_data %>%
  left_join(release_cast_data, by = 'feature_name')

write.csv(all_data, 'feature_release_ages.csv', row.names=FALSE)

write.csv(feature_isogroup_data, 'feature_release_isogroup.csv', row.names=FALSE)


# create ts analysis

data_12 <- filter(kingdom_metadata, iso_group == 12)
ggplot(data_12, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 12 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

feature_12_data <- filter(feature_data, iso_group == 12, feature_name == 'AdvancedMarchSpeedupFeature')
ggplot(feature_12_data, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 12 AdvancedMarchSpeedupFeature Release Age') +
  theme(plot.title=element_text(hjust=.5))



feature_12_data <- filter(feature_data, iso_group == 12, feature_name == 'AdvancedMarchSpeedupFeature', created_ts > as.POSIXct('2018-01-01'))
ggplot(feature_12_data, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 12 AdvancedMarchSpeedupFeature Release Age') +
  theme(plot.title=element_text(hjust=.5))


# filter data 12

filterdata <- feature_data %>%
  filter(
    !(iso_group == 12 & (created_ts > as.Date('2017-12-1') | created_ts < as.Date('2017-6-1')))
  )

plotdata <- filter(filterdata, iso_group == 12)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 12, feature_name == 'AdvancedMarchSpeedupFeature') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 12 AdvancedMarchSpeedupFeature Release Age') +
  xlim(0, 250) +
  theme(plot.title=element_text(hjust=.5))

# filter data 15

plotdata <- filter(kingdom_metadata, iso_group == 15)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 15 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 15 & created_ts > as.Date('2017-10-1'))
  )

plotdata <- filter(filterdata, iso_group == 15)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 15, feature_name == 'AdvancedMarchSpeedupFeature') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 15 AdvancedMarchSpeedupFeature Release Age') +
  theme(plot.title=element_text(hjust=.5))


# filter data 17

plotdata <- filter(kingdom_metadata, iso_group == 17)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 17 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 17 & created_ts > as.Date('2017-11-1'))
  )

plotdata <- filter(filterdata, iso_group == 17)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 17, feature_name == 'AdvancedMarchSpeedupFeature') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 17 AdvancedMarchSpeedupFeature Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))


# filter data 19

plotdata <- filter(kingdom_metadata, iso_group == 19)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 19 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 19 & created_ts > as.Date('2017-12-1'))
  )

plotdata <- filter(filterdata, iso_group == 19)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 19, feature_name == 'AdvancedMarchSpeedupFeature') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 19 AdvancedMarchSpeedupFeature Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))



# filter data 21

plotdata <- filter(kingdom_metadata, iso_group == 21)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 21 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 21 & created_ts > as.Date('2018-1-1'))
  )

plotdata <- filter(filterdata, iso_group == 21)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 21, feature_name == 'AdvancedMarchSpeedupFeature') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 21 AdvancedMarchSpeedupFeature Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))


# filter data 23

plotdata <- filter(kingdom_metadata, iso_group == 23) %>% arrange(created_ts)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 23 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 23 & (created_ts > as.Date('2018-1-1') | created_ts < as.Date('2017-11-15')))
  )

plotdata <- filter(filterdata, iso_group == 23)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 23, feature_name == 'MarchPresets') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 23 MarchPresets Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))


# filter data 25

plotdata <- filter(kingdom_metadata, iso_group == 25) %>% arrange(created_ts)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 25 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 25 & (created_ts > as.Date('2018-2-5') | created_ts < as.Date('2017-12-15')))
  )

plotdata <- filter(filterdata, iso_group == 25)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 25, feature_name == 'MarchPresets') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 25 MarchPresets Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))

# filter data 27

plotdata <- filter(kingdom_metadata, iso_group == 27) %>% arrange(created_ts)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 27 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 27 & (created_ts < as.Date('2018-1-15')))
  )

plotdata <- filter(filterdata, iso_group == 27)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 27, feature_name == 'MarchPresets') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 27 MarchPresets Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))


# filter data 29

plotdata <- filter(kingdom_metadata, iso_group == 29) %>% arrange(created_ts)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 29 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 29 & (created_ts < as.Date('2018-2-15')))
  )

plotdata <- filter(filterdata, iso_group == 29)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 29, feature_name == 'MarchPresets') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 29 MarchPresets Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))


# filter data 31

plotdata <- filter(kingdom_metadata, iso_group == 31) %>% arrange(created_ts)
ggplot(plotdata, aes(created_ts)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 31 Province Created Date') +
  theme(plot.title=element_text(hjust=.5))

filterdata <- filterdata %>%
  filter(
    !(iso_group == 31 & (created_ts < as.Date('2018-4-1')))
  )

plotdata <- filter(filterdata, iso_group == 31)

ggplot(plotdata, aes(created_ts)) +
  geom_histogram()


plotdata <- filter(filterdata, iso_group == 31, feature_name == 'MarchPresets') %>% arrange(activate_age)

ggplot(plotdata, aes(activate_age)) +
  geom_histogram(bins = 60) +
  labs(title = 'Isogroup 31 MarchPresets Release Age') +
  expand_limits(x=0) +
  theme(plot.title=element_text(hjust=.5))




day <- as.POSIXct(seq(as.Date('2017-9-1'), as.Date('2018-5-31'), by='day'))
province <- filter(feature_data, iso_group == 12)
ggplot(province, aes(activate_ts)) + 
  geom_histogram(breaks=day)






