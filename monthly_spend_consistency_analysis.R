# By: James Tan

# Date: 5/9/2018

library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(plotly)

source('~/.Rprofile')
setwd(datapath)

daudata <- read.csv('monthly_dau.csv')
sapply(daudata, class)
names(daudata) <- c('purchase_month', 'dau')
daudata$purchase_month_ts <- as.POSIXct(daudata$purchase_month, format='%Y-%m-%d %H:%M:%S', tz='UTC')
daudata$purchase_month <- factor(strftime(daudata$purchase_month, format='%Y-%m-%d'))
daudata <- daudata %>% arrange(purchase_month) %>% select(purchase_month, dau)

mydata <- read.csv('wiso_monthly_rev.csv')

sapply(mydata, class)
mydata$purchase_month_ts <- as.POSIXct(mydata$purchase_month, format='%Y-%m-%d %H:%M:%S', tz='UTC')
mydata$purchase_month <- factor(strftime(mydata$purchase_month, format='%Y-%m-%d'))
mydata$game_id <- as.factor(mydata$game_id)
mydata$user_id <- as.factor(mydata$user_id)
mydata$first_login_ts <- as.POSIXct(mydata$first_login_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC')

users <- distinct(mydata, game_id, user_id, first_login_ts)
# months <- head(distinct(mydata, purchase_month), -1)
months <- distinct(mydata, purchase_month)
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

nextdata <- ungroup(nextdata)

saveRDS(nextdata, 'wiso_monthly_rev_lead.RData')

nextdata <- readRDS('wiso_monthly_rev_lead.RData')

quantile(nextdata$revenue, probs=seq(0, 1, .01))

data <- nextdata %>% ungroup()
column <- 'revenue'
num_quantiles <- 200

# breakpoints <- stats::quantile(data[, column], seq(0, 1, 1/num_quantiles), na.rm = TRUE)
# breakpoints <- unique(round(breakpoints, 9))
breakpoints <- c(0, 5, 25, 100, 200, 500, 1000, 1500, 2000, 5000, 221300)
num_bins <- length(breakpoints)
breakpoints[num_bins] <- breakpoints[num_bins] + 1e-09
data[, paste(column, "bin", sep = "_")] <- cut(data[, column, drop=TRUE], breakpoints, include.lowest = TRUE, right = FALSE,
                                               labels = NULL, dig.lab = 3)

nextdata <- data

meltdata <- nextdata %>%
  select(
    user_id, purchase_month, revenue_bin, rev_month_1, rev_month_2, rev_month_3, rev_month_4
  ) %>%
  rename(
    month_1 = rev_month_1,
    month_2 = rev_month_2,
    month_3 = rev_month_3,
    month_4 = rev_month_4,
  )
meltdata <- melt(meltdata, id.vars=c('user_id', 'purchase_month', 'revenue_bin'), variable.name='month', value.name='revenue')

percentiledata <- meltdata %>%
  group_by(revenue_bin, month) %>%
  summarise(
    n = n(),
    rev_mean = mean(revenue, na.rm=TRUE),
    rev_median = median(revenue, na.rm=TRUE),
    revenue.1 = quantile(revenue, .1, na.rm=TRUE),
    revenue.2 = quantile(revenue, .2, na.rm=TRUE),
    revenue.3 = quantile(revenue, .3, na.rm=TRUE),
    revenue.4 = quantile(revenue, .4, na.rm=TRUE),
    revenue.5 = quantile(revenue, .5, na.rm=TRUE),
    revenue.6 = quantile(revenue, .6, na.rm=TRUE),
    revenue.7 = quantile(revenue, .7, na.rm=TRUE),
    revenue.8 = quantile(revenue, .8, na.rm=TRUE),
    revenue.9 = quantile(revenue, .9, na.rm=TRUE)
  )

p1 <- ggplot(percentiledata, aes(month, rev_median, group=1)) +
  geom_ribbon(aes(ymin=revenue.1, ymax=revenue.9, fill='80%'), alpha=.8) +
  geom_ribbon(aes(ymin=revenue.2, ymax=revenue.8, fill='60%'), alpha=.8) +
  geom_ribbon(aes(ymin=revenue.3, ymax=revenue.7, fill='40%'), alpha=.8) +
  geom_ribbon(aes(ymin=revenue.4, ymax=revenue.6, fill='20%'), alpha=.8) +
  geom_line(aes(linetype='Median')) +
  geom_line(aes(y=rev_mean, linetype='Mean')) +
  facet_wrap(~ revenue_bin, scales='free_y', ncol=5) +
  scale_fill_brewer(type='seq', direction=-1, palette='Blues', guide = guide_legend(title='% of Players')) +
  scale_linetype_manual(values=c('Mean' = 1, 'Median' = 2), guide=guide_legend(title='Aggregate')) +
  # scale_x_continuous(limits=c(0, 50), breaks=seq(0,50,5)) +
  # scale_y_continuous(limits=c(0, 30), breaks=seq(0,30,2)) +
  labs(title = 'WISO Spend Continuity by Month by Spend in Month 1', subtitle='Only includes players who have spent at least once in their lifetime', y='Revenue / Month') +
  theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5))
p1

# ggplotly(p1)
























