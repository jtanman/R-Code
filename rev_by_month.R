# By: James Tan

# Date: 4/30/2018

library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(plotly)

source('~/.Rprofile')
setwd(datapath)

mydata <- read.csv('rev_by_month.csv')

sapply(mydata, class)
mydata$purchase_month_ts <- as.POSIXct(mydata$purchase_month, format='%Y-%m-%d %H:%M:%S', tz='UTC')
mydata$purchase_month <- factor(strftime(mydata$purchase_month, format='%Y-%m-%d'))
mydata$game_id <- as.factor(mydata$game_id)
mydata$user_id <- as.factor(mydata$user_id)
mydata$first_login_ts <- as.POSIXct(mydata$first_login_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC')

users <- distinct(mydata, game_id, user_id, first_login_ts)
months <- head(distinct(mydata, purchase_month), -1)
alldata <- merge(users, months, all=TRUE) %>%
  arrange(game_id, user_id, purchase_month) %>%
  left_join(mydata, by=c('game_id', 'user_id', 'first_login_ts', 'purchase_month')) %>%
  mutate(
    purchase_month_ts = as.POSIXct(purchase_month, format='%Y-%m-%d', tz='UTC'),
    install_month_floor = floor_date(first_login_ts, unit='month'),
    revenue = coalesce(revenue, 0),
    rev_per_day = revenue / days_in_month(purchase_month_ts)
  ) %>%
  filter(
    purchase_month_ts >= install_month_floor
  )

nextdata <- alldata %>%
  group_by(user_id) %>%
  mutate(
    rev_day_month_1 = rev_per_day,
    rev_day_month_2 = lead(rev_per_day, 1, order_by = purchase_month),
    rev_day_month_3 = lead(rev_per_day, 2, order_by = purchase_month),
    rev_day_month_4 = lead(rev_per_day, 3, order_by = purchase_month),
    rev_month_1 = revenue,
    rev_month_2 = lead(revenue, 1, order_by = purchase_month),
    rev_month_3 = lead(revenue, 2, order_by = purchase_month),
    rev_month_4 = lead(revenue, 3, order_by = purchase_month),
  )


filterdata <- nextdata %>%
  filter(
    rev_day_month_1 >= 65,
    # rev_day_month_2 >= 65,
    # rev_day_month_3 < 65,
    # !is.na(rev_month_4)
  )

  # Number of Users Plot

countdata <- filterdata %>%
  group_by(purchase_month) %>%
  summarise(
    month_1 = n(),
    month_2 = length(which(rev_day_month_2 >= 65)),
    month_3 = length(which(rev_day_month_3 >= 65)),
    month_4 = length(which(rev_day_month_4 >= 65)),
  ) %>%
  filter(month_1 > 100) %>%
  mutate(
    n = month_1,
    month_1 = month_1 / n,
    month_2 = month_2 / n,
    month_3 = month_3 / n,
    month_4 = month_4 / n,
  ) %>%
  rename(
    first_month = purchase_month
  ) %>%
  select(
    -n
  )

countdata[countdata == 0] <- NA

aggdata <- countdata %>%
  summarise(
    month_1 = weighted.mean(month_1, n, na.rm=TRUE),
    month_2 = weighted.mean(month_2, n, na.rm=TRUE),
    month_3 = weighted.mean(month_3, n, na.rm=TRUE),
    month_4 = weighted.mean(month_4, n, na.rm=TRUE),
  )

meltdata <- melt(countdata, id.vars = 'first_month', variable.name='month', value.name='num_users')

p1 <- ggplot(meltdata, aes(month, num_users, group=first_month, color=first_month)) +
  geom_line() +
  labs(title='Number of Users with $65+ Rev/Day in Month 1') +
  theme(plot.title = element_text(hjust=.5))

ggplotly(p1)

# Rev Plots

revdata <- filterdata %>%
  group_by(purchase_month) %>%
  summarise(
    rev_day_month_1 = mean(rev_day_month_1),
    rev_day_month_2 = mean(rev_day_month_2),
    rev_day_month_3 = mean(rev_day_month_3),
    rev_day_month_4 = mean(rev_day_month_4),
    rev_month_1 = mean(rev_month_1),
    rev_month_2 = mean(rev_month_2),
    rev_month_3 = mean(rev_month_3),
    rev_month_4 = mean(rev_month_4),
    n = n()
  ) %>%
  filter(
    n > 100
  )



revdata <- revdata %>%
  # select(purchase_month, rev_day_month_1, rev_day_month_2, rev_day_month_3, rev_day_month_4) %>%
  select(purchase_month, rev_day_month_1, rev_day_month_2, rev_day_month_3, rev_day_month_4, n) %>%
  rename(
    first_month = purchase_month,
    month_1 = rev_day_month_1,
    month_2 = rev_day_month_2,
    month_3 = rev_day_month_3,
    month_4 = rev_day_month_4,
  )

aggdata <- revdata %>%
  summarise(
    month_1 = weighted.mean(month_1, n, na.rm=TRUE),
    month_2 = weighted.mean(month_2, n, na.rm=TRUE),
    month_3 = weighted.mean(month_3, n, na.rm=TRUE),
    month_4 = weighted.mean(month_4, n, na.rm=TRUE),
  )

meltdata <- melt(revdata, id.vars = 'first_month', variable.name='month', value.name='revenue_per_user_per_day')

p2 <- ggplot(meltdata, aes(month, revenue_per_user_per_day, group=first_month, color=first_month)) +
  geom_line() +
  labs(title='Revenue for Users with $65+ Rev/Day in Month 1 & 2 by Date') +
  theme(plot.title = element_text(hjust=.5))

ggplotly(p2)

# Rev compared to baseline plot

revdata <- filterdata %>%
  group_by(purchase_month) %>%
  summarise(
    rev_day_month_1 = mean(rev_day_month_1),
    rev_day_month_2 = mean(rev_day_month_2),
    rev_day_month_3 = mean(rev_day_month_3),
    rev_day_month_4 = mean(rev_day_month_4),
    n = n()
  ) %>%
  filter(
    n > 100
  ) %>%
  rename(
    first_month = purchase_month,
    month_1 = rev_day_month_1,
    month_2 = rev_day_month_2,
    month_3 = rev_day_month_3,
    month_4 = rev_day_month_4,
  ) %>%
  mutate(
    month_1_temp = month_1,
    month_1 = month_1 / month_1_temp,
    month_2 = month_2 / month_1_temp,
    month_3 = month_3 / month_1_temp,
    month_4 = month_4 / month_1_temp,
  )

%>%
  select(
    -month_1_temp, -n
  )

aggdata <- revdata %>%
  summarise(
    month_1 = weighted.mean(month_1, n, na.rm=TRUE),
    month_2 = weighted.mean(month_2, n, na.rm=TRUE),
    month_3 = weighted.mean(month_3, n, na.rm=TRUE),
    month_4 = weighted.mean(month_4, n, na.rm=TRUE),
  )

meltdata <- melt(revdata, id.vars = 'first_month', variable.name='month', value.name='revenue_percent_per_user_per_day')

p3 <- ggplot(meltdata, aes(month, revenue_percent_per_user_per_day, group=first_month, color=first_month)) +
  geom_line() +
  labs(title='Revenue Compared to Month 1 for Users with $65+ Rev/Day in Month 1 & 2 by Date') +
  theme(plot.title = element_text(hjust=.5))

ggplotly(p3)

