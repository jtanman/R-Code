# By: James Tan

# Date: 6/12/2018

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)

source('~/.Rprofile')
setwd(datapath)

# A/B Test

# mydata <- read.csv('test_20_50_users.csv')
# sapply(mydata, class)
# mydata <- mydata %>%
#   mutate(
#     min_date = as.Date(min_date),
#     date = as.Date(date),
#     user_id = as.factor(user_id),
#     chest_type = factor(chest_type, levels=levels(mydata$chest_type)[c(3,1,2)]),
#     LTV_Lapsed = factor(LTV_Lapsed, levels=levels(LTV_Lapsed)[c(1, 3, 2)])
#   )
# 
# saveRDS(mydata, 'test_20_50_users.RData')
mydata <- readRDS('test_20_50_users.RData')
users <- mydata %>%
  filter(!is.na(LTV_Lapsed)) %>%
  group_by(user_id, min_date, LTV_Lapsed, test_group, chest_type, price_point) %>%
  summarise() %>%
  as.data.frame()
dates <- data.frame(date = sort(unique(mydata$date)))
crossdata <- merge(users, dates, all=TRUE) %>% mutate(cohortday = as.numeric(date - min_date))
mydata <- crossdata %>%
  left_join(mydata, by=c('user_id', 'min_date', 'date', 'cohortday', 'LTV_Lapsed', 'test_group', 'chest_type', 'price_point')) %>%
  filter(difftime(date, min_date, units='days') >= 0) %>%
  mutate(
    type = as.factor(paste(price_point, chest_type, sep=', '))
  ) %>%
  mutate(
    type = factor(type, levels=levels(type)[c(3, 1, 2, 6, 4, 5)])
  ) %>%
  arrange(user_id, date)

cohortdata <- mydata %>%
  group_by(test_group, cohortday) %>%
  summarise(
    users = length(unique(user_id)),
    revenue = sum(revenue, na.rm=TRUE),
    total_purchases = sum(total_purchases, na.rm=TRUE),
    purchases_100 = sum(purchases_100, na.rm=TRUE),
    step_up_100 = sum(step_up_100, na.rm=TRUE),
    arpu = revenue / users,
    total_purchases_user = total_purchases / users,
    purchases_100_user = purchases_100 / users,
    step_up_100_user = step_up_100 / users
  ) %>%
  group_by(test_group) %>%
  mutate(
    cumarpu = cumsum(arpu)
  )

ggplot(cohortdata, aes(cohortday, cumarpu, group=test_group, color=test_group)) +
  geom_line()

plotdata <- select(cohortdata, test_group, cohortday, users, arpu, total_purchases_user, purchases_100_user, step_up_100_user, cumarpu)
plotdata <- melt(plotdata, id.vars=c('test_group', 'cohortday'))
p1 <- ggplot(plotdata, aes(cohortday, value, group=test_group, color=test_group)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free', ncol=2) +
  labs(title='A/B Test Metrics\nAll Users\n')
p1
ggplotly(p1)



metricdata <- mydata %>%
  group_by(test_group, date) %>%
  summarise(
    users = length(unique(user_id)),
    revenue = sum(revenue, na.rm=TRUE),
    total_purchases = sum(total_purchases, na.rm=TRUE),
    purchases_100 = sum(purchases_100, na.rm=TRUE),
    step_up_100 = sum(step_up_100, na.rm=TRUE),
    arpu = revenue / users
  ) %>%
  group_by(test_group) %>%
  mutate(
    cumarpu = cumsum(arpu)
  )

ggplot(metricdata, aes(date, cumarpu, group=test_group, color=test_group)) +
  geom_line()

payers <- unique(mydata %>% filter(!is.na(revenue)) %>% pull(user_id))

userdata <- mydata %>%
  group_by(user_id) %>%
  arrange(date) %>%
  mutate(
    cumrev = cumsum(coalesce(revenue, 0))
  ) %>%
  arrange(user_id, date)

cumrev_control <- userdata %>% filter(test_group == 'control_kingdoms', cohortday == 7) %>% pull(cumrev)
cumrev_test <- userdata %>% filter(test_group == 'test_kingdoms', cohortday == 7) %>% pull(cumrev)

cumrev_control <- userdata %>% filter(test_group == 'control_kingdoms', cohortday == 19) %>% pull(cumrev)
cumrev_test <- userdata %>% filter(test_group == 'test_kingdoms', cohortday == 19) %>% pull(cumrev)

t.test(cumrev_test, cumrev_control)

ltvdata <- filter(mydata, LTV_Lapsed=='ltv: 1-19 / lapsed 7d+')
cohortdata <- ltvdata %>%
  group_by(test_group, cohortday) %>%
  summarise(
    users = length(unique(user_id)),
    revenue = sum(revenue, na.rm=TRUE),
    total_purchases = sum(total_purchases, na.rm=TRUE),
    purchases_100 = sum(purchases_100, na.rm=TRUE),
    step_up_100 = sum(step_up_100, na.rm=TRUE),
    arpu = revenue / users,
    total_purchases_user = total_purchases / users,
    purchases_100_user = purchases_100 / users,
    step_up_100_user = step_up_100 / users
  ) %>%
  group_by(test_group) %>%
  mutate(
    cumarpu = cumsum(arpu)
  )

plotdata <- select(cohortdata, test_group, cohortday, users, arpu, total_purchases_user, purchases_100_user, step_up_100_user, cumarpu)
plotdata <- melt(plotdata, id.vars=c('test_group', 'cohortday'))
p1 <- ggplot(plotdata, aes(cohortday, value, group=test_group, color=test_group)) +
  geom_line() +
  facet_wrap(~variable, scales = 'free_y', ncol=2) +
  labs(title='A/B Test Metrics', subtitle='LTV Lapsed 100+')
p1
ggplotly(p1)

cumrev_control <- userdata %>% filter(test_group == 'control_kingdoms', LTV_Lapsed == levels(LTV_Lapsed)[3], cohortday == 19) %>% pull(cumrev)
cumrev_test <- userdata %>% filter(test_group == 'test_kingdoms', LTV_Lapsed == levels(LTV_Lapsed)[3], cohortday == 19) %>% pull(cumrev)

t.test(cumrev_test, cumrev_control)


# Type graphs

cohortdata <- mydata %>%
  filter(test_group == 'test_kingdoms') %>%
  group_by(cohortday, LTV_Lapsed, type, chest_type, price_point) %>%
  summarise(
    users = length(unique(user_id)),
    revenue = sum(revenue, na.rm=TRUE),
    total_purchases = sum(total_purchases, na.rm=TRUE),
    purchases_100 = sum(purchases_100, na.rm=TRUE),
    step_up_100 = sum(step_up_100, na.rm=TRUE),
    arpu = revenue / users,
    total_purchases_user = total_purchases / users,
    purchases_100_user = purchases_100 / users,
    step_up_100_user = step_up_100 / users
  ) %>%
  group_by(LTV_Lapsed, type) %>%
  arrange(cohortday) %>%
  mutate(
    cumarpu = cumsum(arpu)
  ) %>%
  arrange(LTV_Lapsed, type, cohortday) %>%
  ungroup()

# colors <- c('#6baed6','#3182bd','#08519c', '#ffeda0','#feb24c','#f03b20')
# plotdata <- select(cohortdata, LTV_Lapsed, type, cohortday, cumarpu)
# plotdata <- melt(plotdata, id.vars=c('LTV_Lapsed', 'type', 'cohortday'))
p1 <- ggplot(cohortdata, aes(cohortday, cumarpu, group=type, color=chest_type, linetype=price_point)) +
  geom_line() +
  facet_wrap(~LTV_Lapsed, scales = 'free_y', ncol=1) +
  labs(title='Chest Type and Price Points') +
  scale_color_brewer(type='qual')
  # scale_color_manual(values = colors)
p1
ggplotly(p1)









userids <- mydata %>%
  group_by(user_id) %>%
  summarise(
    n = length(unique(test_group))
  ) %>%
  filter(n > 1) %>%
  pull(user_id)


mindate <- mydata %>%
  group_by(user_id) %>%
  filter(LTV_Lapsed != '(null)') %>%
  summarise(
    min_date = min(date)
  )

mydata2 <- mydata %>%
  filter(!user_id %in% userids) %>%
  left_join(mindate, by='user_id') %>%
  left_join(dates, by='user_id') %>%
  arrange(user_id, date)

testdata <- mydata %>%
  filter(date < as.Date("2018-6-7")) %>%
  group_by(user_id) %>%
  arrange(date) %>%
  mutate(
    cumrev = cumsum(revenue)
  ) %>%
  arrange(user_id, date)



ggplot(testdata, aes(date, total_users, color=test_group)) +
  geom_line() +
  facet_wrap(~LTV_Lapsed)

ggplot(testdata, aes(date, cumrev_user, color=test_group)) +
  geom_line() +
  facet_wrap(~LTV_Lapsed)

# LTV 1-19

test <- filter(testdata, LTV_Lapsed=='ltv: 1-19 / lapsed 7d+', test_group=='test_kingdoms') %>% pull(rev_user)
control <- filter(testdata, LTV_Lapsed=='ltv: 1-19 / lapsed 7d+', test_group=='control_kingdoms') %>% pull(rev_user)
t.test(test, control)

# LTV 20-99

test <- filter(testdata, LTV_Lapsed=='ltv: 20-99 / lapsed 30d+', test_group=='test_kingdoms') %>% pull(rev_user)
control <- filter(testdata, LTV_Lapsed=='ltv: 20-99 / lapsed 30d+', test_group=='control_kingdoms') %>% pull(rev_user)
t.test(test, control)

# LTV 100+

test <- filter(testdata, LTV_Lapsed=='ltv: 100+ / lapsed 30d+', test_group=='test_kingdoms') %>% pull(rev_user)
control <- filter(testdata, LTV_Lapsed=='ltv: 100+ / lapsed 30d+', test_group=='control_kingdoms') %>% pull(rev_user)
t.test(test, control)
