# By: James Tan

# Date: 4/30/2018

library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)

source('~/.Rprofile')
setwd(datapath)

mydata <- read.csv('jan_feb_18_2000_spend_daily_data.csv')

sapply(mydata, class)
mydata$date <- as.Date(mydata$metric_date_ts)
mydata$first_login_ts_test <- as.POSIXct(mydata$first_login_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC')

revdata <- select(mydata, game_id, user_id, rev_201801, rev_201802, rev_201803, rev_201804, date, revenue)
revdata <- filter(revdata, rev_201803 < 2000)

rev_pivot <- dcast(revdata, game_id + user_id + rev_201801 + rev_201802 + rev_201803 + rev_201804 ~ date, fun.aggregate = mean)


rev_pivot_2 <- do.call(data.frame, lapply(rev_pivot, function(x) {
  replace(x, is.infinite(x) | is.na(x), 0)
})
)

colnames(rev_pivot_2) <- colnames(rev_pivot)

rev_pivot <- rev_pivot_2

rm(rev_pivot_2)

rev_pivot <- rev_pivot %>%
  mutate(
    delta = rev_201801 + rev_201802 - rev_201803 - rev_201804
  ) %>%
  arrange(desc(delta)) %>%
  select(-delta)

write.csv(rev_pivot, 'rev_by_day.csv', row.names=FALSE)


leveldata <- mydata %>%
  filter(rev_201803 < 2000) %>%
  select(game_id, user_id, rev_201801, rev_201802, rev_201803, rev_201804, date, user_level)

rev_pivot <- dcast(leveldata, game_id + user_id + rev_201801 + rev_201802 + rev_201803 + rev_201804 ~ date, fun.aggregate = mean)

rev_pivot_2 <- do.call(data.frame, lapply(rev_pivot, function(x) {
  replace(x, !is.finite(x), NA)
})
)

colnames(rev_pivot_2) <- colnames(rev_pivot)

rev_pivot <- rev_pivot_2

rm(rev_pivot_2)

rev_pivot <- rev_pivot %>%
  mutate(
    delta = rev_201801 + rev_201802 - rev_201803 - rev_201804
  ) %>%
  arrange(desc(delta)) %>%
  select(-delta)

write.csv(rev_pivot, 'level_by_day.csv', row.names=FALSE)
