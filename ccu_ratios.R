# By: James Tan

# Date: 4/8/2019

p_load(tidyverse, reshape2, beepr, scales, zoo, tools)

source('~/.Rprofile')
setwd(datapath)

daudata <- read_csv('dau_ody_wiso_miso.csv') %>%
  rename(date = DATE)

ccudata <- read_csv('ccu_wiso_19.csv') %>%
  mutate(game_id = 19) %>%
  bind_rows(read_csv('ccu_ody_15.csv') %>% mutate(game_id = 15)) %>%
  bind_rows(read_csv('ccu_miso_27.csv') %>% mutate(game_id = 27)) %>%
  rename(
    date = '__timestamp',
    max_value = max__value,
    avg_value = avg__value
  )

retdata <- ccudata %>%
  left_join(daudata, by=c('game_id', 'date')) %>%
  group_by(game_id) %>%
  mutate(
    max_ratio = max_value / dau,
    avg_ratio = avg_value / dau,
    days_from_launch = as.numeric(date - min(date))
  ) %>%
  arrange(game_id, date)

write_csv(retdata, 'ody_wiso_miso_ccu_ratios.csv')

# plotdata <- melt(retdata, id.vars = c('game_id', 'date', 'days_from_launch'), variable.name = 'type', value.name = 'ratio') %>%
  # filter(type %in% c('max_ratio', 'avg_ratio'))

plotdata <- gather(retdata, key='type', max_ratio, avg_ratio, value='ratio')

ggplot(plotdata, aes(days_from_launch, ratio, color=type)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(~game_id, ncol=1) +
  scale_y_continuous(labels = scales::percent)
