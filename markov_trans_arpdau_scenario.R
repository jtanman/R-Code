

i = 100
trans_2_20_100 <- tm_results$result_summary$arpi

i = 8
arpdau_2_20 <- tm_results$result_summary$arpi

i = 9
arpdau_2_100 <- tm_results$result_summary$arpi

cumarpi <- tibble(cohortday=seq(0,271), trans_2_20_100=trans_2_20_100, arpdau_2_20=arpdau_2_20, arpdau_2_100=arpdau_2_100)


plotdata <- pivot_longer(cumarpi, cols=c(trans, starts_with('arpdau')), names_to='type', values_to='cumarpi')
ggplot(plotdata, aes(cohortday, cumarpi, color=type)) +
  geom_line() +
  scale_x_continuous(limits=c(0, 10)) +
  scale_y_continuous(limits=c(0, 1))


# cohortdata <- readRDS('revenue_modeling_cohortdata_all_dates_niso_parsed.RData')
# installdata <- cohortdata %>%
#   group_by(install_date, date) %>%
#   summarise(installs = sum(installs)) %>%
#   group_by(install_date) %>%
#   summarise(
#     installs = mean(installs)
#   )

installdata <- read_csv('niso_install_data_distinct.csv', col_types = cols(
  game_id = col_factor(),
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  country = col_factor()
))
summary(installdata)

installdata <- installdata %>%
  filter(!is.na(install_platform), dup_account == 'nondup') %>%
  group_by(install_date) %>%
  summarise(installs = sum(installs))

# installdata <- merge(installdata, install_csv, by='install_date')

cohortdata <- merge(installdata, tibble(date = installdata$install_date)) %>%
  filter(date >= install_date) %>%
  mutate(
    cohortday = as.integer(date - install_date)
  ) %>%
  filter(date <= min(date) + 270) %>%
  left_join(cumarpi, by='cohortday') %>%
  pivot_longer(cols=c(trans_2_20_100, starts_with('arpdau')), names_to='type', values_to = 'arpi') %>%
  mutate(
    revenue = installs * arpi
  ) %>%
  group_by(date, type) %>%
  summarise(
    revenue = sum(revenue)
  ) %>%
  group_by(type) %>%
  arrange(date) %>%
  mutate(
    cumrev = cumsum(revenue)
  )

comparedata <- pivot_wider(cohortdata, id_cols=date, names_from=type, values_from = cumrev) %>%
  mutate(
    diff_20 = arpdau_2_20 - trans_2_20_100,
    diff_100 = arpdau_2_100 - trans_2_20_100
  )

ggplot(filter(comparedata, date <= min(comparedata$date) + 8), aes(date, diff_20)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=diff_20), fill='green') +
  labs(title='Difference Betweeen Improving ARPDAU vs Step Up Rate', caption='Using NISO data from launch', subtitle='$14k Surplus by Day 9') +
  theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5)) +
  scale_y_continuous(name='Cumulative Revenue Difference', labels=format.money) +
  scale_x_date(name='Date')

ggsave_default('d9_arpdau_vs_trans.png', width=12, height=9)


ggplot(comparedata, aes(date, diff_20)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=diff_20, fill=diff_20 > 0)) +
  labs(title='Difference Betweeen Improving ARPDAU vs Step Up Rate', caption='Using NISO data from launch', subtitle='>$1 million deficit by Day 270 ') +
  theme(plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5), legend.position='none') +
  scale_y_continuous(name='Cumulative Revenue Difference', labels=format.money) +
  scale_x_date(name='Date') +
  scale_fill_manual(values=c('red', 'green'))

ggsave_default('d270_arpdau_vs_trans.png', width=12, height=9)

ggplot(comparedata, aes(date, diff_20)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=diff_20))

max(comparedata$arpdau_2_20 - comparedata$trans_2_20_100)

which(comparedata$arpdau_2_100 - comparedata$trans_2_20_100 == max(comparedata$arpdau_2_100 - comparedata$trans_2_20_100))
which(comparedata$arpdau_2_20 - comparedata$trans_2_20_100 == max(comparedata$arpdau_2_20 - comparedata$trans_2_20_100))

ggplot(filter(cohortdata, date <= (min(cohortdata$date) + 9)), aes(date, cumrev, color=type)) +
  geom_line()

ggplot(filter(cohortdata, date <= (min(cohortdata$date) + 7)), aes(date, cumrev, color=type)) +
  geom_line()
  
ggplot(cohortdata, aes(date, cumrev, color=type)) +
  geom_line()
