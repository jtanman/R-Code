# By: James Tan

# Date: 6/19/2019

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis)

source('~/.Rprofile')
setwd(datapath)

source('~/MZ/R Code/generalized_markov_model_helper.R')

STATES <- c('$0 Churn', '$5 Churn', '$20 Churn', '$100+ Churn', '$0 DAU', '$5 DAU', '$20 DAU', '$100+ DAU')

markovmodel <- readRDS('niso_markov_data_2017-06-28_2019-02-10_markov_model.RData')
transitiondata <- markovmodel$transitiondata
install_transition <- markovmodel$install_transition
transition_matrix <- markovmodel$transition_matrix
arpdaudata <- markovmodel$arpdaudata
transitionMatrices <- markovmodel$transitionMatrices
state_probs <- markovmodel$state_probs
simulation_results <- markovmodel$simulation_results
result <- markovmodel$result
result_summary <- markovmodel$result_summary

statedata <- read_csv('revenue_impact_niso_30d_analysis.csv', col_types = cols(
  campaign_type = col_factor(),
  install_platform = col_factor(),
  dup_account = col_factor(),
  state = col_factor(levels=seq(0,7))
))

# summary(statedata)

statedata <- statedata %>%
  filter(!is.na(install_platform)) %>%
  group_by(state) %>%
  summarise(
    users = sum(users),
  ) %>%
  ungroup() %>%
  mutate(
    users = ifelse(as.numeric(as.character(state)) < 4, 0, users),
    prob = users / sum(users),
    cohortday = 30
  )

total_users <- sum(statedata$users)

summary(state_prob_30)
state_probs_30 <- getStateProbs(install_transition, transitionMatrices, max_cohortday=270, startday=30, state_probs=select(statedata, -users))

sim_results <- run_simulation(state_probs=state_probs_30, arpdaudata=arpdaudata, startday=30, max_cohortday=270)

result_summary <- sim_results$result_summary %>%
  mutate(revenue = arpi * total_users, cumrev = cumarpi * total_users)

plotdata <- result_summary %>%
  mutate(date = cohortday - 30 + as.Date('2019-06-16')) %>%
  filter(cohortday >= 30)

ggplot(plotdata, aes(date, revenue)) +
  geom_line() +
  labs(title='NISO Revenue Simulation (Current State -> 30d User)', subtitle='Applying d30 and onwards transition rates to current state of users', caption='Using only first 12 months of installs\nAssuming no new installs') +
  theme(plot.title=element_text(hjust=.5), plot.subtitle = element_text(hjust=.5)) +
  scale_y_continuous(labels=dollar)

ggsave_default('niso_d30_scenario_transitions.png')

plotdata %>% select(date, revenue) %>%
  write_csv(path='niso_d30_scenario_transitions.csv')
