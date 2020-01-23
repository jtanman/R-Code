# By: James Tan

# Date: 3/10/2019

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/revenue_impact_finance_helper.R')

# case when current_isogroup = 2 then ‘20190420’
# when current_isogroup in (12,17,21) then ‘20190421’
# when current_isogroup = 25 then ‘20190507’
# when current_isogroup = 27 then ‘20190514’
# else null
# dark world release date

game_id = data.frame(game_id=c('15', '19', '23', '27'), game=c('GoW', 'MS', 'FFXV', 'WWR'))

mydata <- read_csv('games_arpi.csv', col_types = cols(
  cohort = col_factor(),
  game = col_factor()
)) %>%
  rename(cohort_month = cohort) %>%
  pivot_longer(contains('ARPI'), names_to = c('cohortday'), names_pattern = 'D([[:alnum:]]+) ARPI', values_to='arpi', values_drop_na = TRUE) %>%
  extract(cohortday, 'cohortday', 'D([[:alnum:]]+) ARPI') %>%
  mutate(
    cohortday = as.integer(cohortday)
  )

mydata <- read_csv('games_arpi_month.csv', col_types = cols(
  game_id = col_factor()
)) %>%
  group_by(game_id) %>%
  arrange(year, month) %>%
  mutate(
    cohort_month = row_number() - 1
  ) %>%
  ungroup() %>%
  pivot_longer(contains('revenue'), names_to = c('cohortday'), names_pattern = 'd([[:alnum:]]+)revenue', values_to='arpi', values_drop_na = TRUE) %>%
  extract(cohortday, 'cohortday', 'd([[:alnum:]]+)revenue') %>%
  mutate(
    cohortday = as.integer(cohortday)
  ) %>%
  filter(cohortday <= 730) %>%
  left_join(game_id, by='game_id') %>%
  arrange(game_id, cohort_month, cohortday)

mydata <- read_csv('ARPX_crosstab.csv', col_types = cols(
  game = col_factor()
)) %>%
  group_by(game) %>%
  arrange(date) %>%
  mutate(cohort_month = row_number() - 1) %>%
  pivot_longer(contains('ARPX'), names_to = c('cohortday'), names_pattern = 'D([[:alnum:]]+)_ARPX', values_to='arpi', values_drop_na = TRUE) %>%
  extract(cohortday, 'cohortday', 'D([[:alnum:]]+)_ARPX') %>%
  mutate(
    cohortday = as.integer(cohortday),
    arpi = parse_number(arpi)
  ) %>%
  arrange(game, date)

mydata <- mydata %>%
  filter(
    !(game == 'GOW' & date >= as.Date('2017-07-01')),
    !(game == 'MS' & date >= as.Date('2017-07-01')),
  )

zero_cohort <- mydata %>%
  filter(cohort_month == 0) %>%
  select('game', 'cohortday', 'arpi')

percentdata <- mydata %>%
  left_join(zero_cohort, by=c('game', 'cohortday')) %>%
  mutate(
    arpi_percent = arpi.x / arpi.y
  )

outlierdata <- filter(percentdata, arpi_percent > 5) %>%
  distinct(game, date)

plotdata <- mydata
plotdata <- mydata %>%
  arrange(game, cohort_month) %>%
  mutate(
    cohort_month = as.integer(as.character(cohort_month)),
    cohort = fct_inorder(paste(game, cohort_month)),
    game_int = as.integer(game) - 1,
    hue_base = game_int / (max(game_int) + 1),
    light_base = (cohort_month / (max(cohort_month) + 1)),
    hue = floor(hue_base * 360) + 15,
    light = floor(light_base * 65) + 35,
    hex = hcl(h = hue, l = light, c=100)
  )

plotdata <- plotdata %>%
  anti_join(outlierdata, by=c('game', 'date'))

colors <- plotdata %>%
  distinct(cohort, hex)

ggplot(plotdata, aes(cohortday, arpi, color=cohort, group = cohort)) +
  geom_line() + 
  scale_color_manual(
    values = colors$hex
  ) +
  facet_wrap(~game)

ggsave_default('games_arpi_custom.png')

ggplot(plotdata, aes(cohortday, arpi, color=interaction(cohort_month, game))) +
  geom_line()

ggplot(plotdata, aes(cohortday, arpi, color=game, alpha=cohort_month, group=interaction(cohort_month, game))) +
  geom_line() +
  facet_wrap(~game) +
  scale_alpha_continuous(trans='reverse')

ggplot(plotdata, aes(cohortday, arpi, color=cohort_month, group=cohort_month)) +
  geom_line() +
  facet_wrap(~game) +
  scale_color_distiller(palette='YlGnBu', guide=guide_colorbar(reverse=TRUE)) +
  # scale_color_viridis(option = 'plasma') +
  # scale_color_gradientn(colors=rainbow(length(unique(plotdata$cohort_month)))) +
  labs(title='ARPI Curves by Cohort Month') +
  theme(plot.title = element_text(hjust=.5))
  
ggsave_default('games_arpi_marketing_data.png', height=9 , width=16)

plotdata <- percentdata %>%
  arrange(game, cohort_month) %>%
  mutate(
    cohort_month = as.integer(as.character(cohort_month)),
    cohort = fct_inorder(paste(game, cohort_month)),
    game_int = as.integer(game) - 1,
    hue_base = game_int / (max(game_int) + 1),
    light_base = (cohort_month / (max(cohort_month) + 1)),
    hue = floor(hue_base * 360) + 15,
    light = floor(light_base * 65) + 35,
    hex = hcl(h = hue, l = light, c=100)
  )

plotdata <- plotdata %>%
  anti_join(outlierdata, by=c('game', 'date'))

plotdata <- filter(plotdata, game != 'MS')

ggplot(plotdata, aes(cohortday, arpi_percent, color=cohort_month, group=cohort_month)) +
  geom_line() +
  facet_wrap(~game) +
  scale_color_distiller(palette='YlGnBu', guide=guide_colorbar(reverse=TRUE)) +
  # scale_color_viridis(option = 'plasma') +
  # scale_color_gradientn(colors=rainbow(length(unique(plotdata$cohort_month)))) +
  labs(title='ARPI Percentage Curves by Cohort Month') +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('games_arpi_percent.png', height=9 , width=16)

csv_data <- filter(percentdata, arpi_percent > 10) %>%
  rename(revenue = arpi.x, revenue_cohort_0 = arpi.y)
write_csv(csv_data, 'user_level_by_month.csv')

p2data <- extract(pivotdata, cohortday, 'cohortday', 'D([[:alnum:]]+) ARPI')

group_variables <- quos(group)

cohortdata <- regroupCohortData(mydata, group_variables)

write_csv(cohortdata, 'dark_world_metrics.csv')

comparedata <- cohortdata %>%
  select(cohortday, group, arppu, retention, arpdau, arpi, cumarpi, conversion, arprpdau, rpdau_ret) %>%
  mutate(group = recode(group, 'Dark World' = 'dark_world', 'Pre' = 'pre')) %>%
  pivot_wider(names_from=group, values_from=c(cumarpi, retention, arpdau, arpi, arppu, conversion, arprpdau, rpdau_ret))

write_csv(comparedata, 'dark_world_metrics_compare.csv')

plotdata <- cohortdata %>%
  gather(key='metric', value='value', cumarpi, retention, arpdau, arpi, arppu, conversion, arprpdau, rpdau_ret)
  

ggplot(plotdata, aes(cohortday, value, color=group)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~metric, nrow=2, scales='free_y') +
  labs(title = 'Dark World vs Pre Metrics', caption='4/21/19 Cutoff') +
  theme(plot.title = element_text(hjust=.5))

ggsave_default('dark_world_metrics.png')


plotdata <- cohortdata %>%
  group_by(cohortday) %>%
  summarise(installs = sum(installs))

ggplot(plotdata, aes(cohortday, installs)) +
  geom_bar(stat = 'identity')


library(dplyr)
df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")

# If no match, NA:
df %>% extract(x, c("A", "B"), "([a-d]+)-([a-d]+)")


relig_income
relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "count")

# Slightly more complex case where columns have common prefix,
# and missing missings are structural so should be dropped.
billboard
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

# Multiple variables stored in colum names
who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)

# Multiple observations per row
anscombe
anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )


ggplot(diamonds, aes(carat, price, color=color, alpha=cut)) +
  geom_point()

ggplot(diamonds, aes(carat, price, color=interaction(color, cut))) +
  geom_point()

ggplot(diamonds, aes(carat, price, color=color)) +
  geom_point() +
  facet_wrap(~cut)
