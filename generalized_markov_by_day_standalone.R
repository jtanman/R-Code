# By: James Tan

# Date: 9/24/2018

p_load(ggplot2, reshape2, plotly, lubridate, zoo, expm, dplyr, beepr)

source('~/.Rprofile')
source('~/MZ/R Code/generalized_markov_model_helper.R')
setwd(datapath)

mydata <- readRDS('niso_state_data.RData')

max_cohortday <- 90

# group_variables <- quos(campaign_type, install_platform, dup_account)
group_variables <- quos()

# Train and run transition matrix

transitiondata <- filter(mydata, !is.na(next_state))

install_transition <- mydata %>%
  filter(cohortday == 0) %>%
  group_by(!!! group_variables, state) %>%
  summarise(
    trans_prob = sum(n)
  ) %>%
  mutate(
    trans_prob = trans_prob / sum(trans_prob)
  )

transition_matrix <- transitiondata %>%
  group_by(!!! group_variables, cohortday, state, next_state) %>%
  summarise(
    users = sum(n)
  ) %>%
  group_by(!!! group_variables, cohortday, state) %>%
  mutate(
    total_users = sum(users),
    trans_prob = users / total_users
  ) %>%
  ungroup() %>%
  arrange(!!! group_variables, cohortday, state, next_state)

arpdaudata <- mydata %>%
  group_by(!!! group_variables, cohortday, state) %>%
  summarise(
    arpdau = sum(revenue) / sum(n)
  )

state_probs <- getStateProbs(install_transition, transition_matrix, max_cohortday)
simulation_results <- run_simulation(state_probs=state_probs, arpdaudata=arpdaudata, max_cohortday=max_cohortday)

result <- simulation_results$result
result_summary <- simulation_results$result_summary

# Calculate Historical Variances

if(length(group_variables) > 0){
  transitions_cross <- distinct(transitiondata, !!! group_variables)
  transitions_cross <- merge(transitions_cross, data.frame(install_date = unique(transitiondata$install_date)))
}else{
  transitions_cross <- data.frame(install_date = unique(transitiondata$install_date))
}

transitions_cross <- merge(transitions_cross, data.frame(cohortday = unique(transitiondata$cohortday))) %>% mutate(date = install_date + cohortday)
transitions_cross <- merge(transitions_cross, data.frame(state = unique(transitiondata$state)))
transitions_cross <- merge(transitions_cross, data.frame(next_state = unique(transitiondata$state))) %>%
  filter(!(cohortday == 0 & state %in% c('1', '2', '3'))) %>%
  arrange(install_date, cohortday, state, next_state)

transition_summary <- transitiondata %>%
  group_by(!!! group_variables, install_date, date, cohortday, state, next_state) %>%
  summarise(
    n = sum(n)
  )

all_transitions <- left_join(transitions_cross, transition_summary, by=unlist(c(sapply(group_variables, quo_name), 'install_date', 'date', 'cohortday', 'state', 'next_state'))) %>%
  mutate(
    n = ifelse(is.na(n), 0, n)
  ) %>%
  group_by(!!! group_variables, install_date, date, cohortday, state) %>%
  mutate(
    state_count = sum(n),
    trans_prob = n / state_count
  )

all_transitions <- all_transitions[complete.cases(all_transitions),]

hist_transitions <- all_transitions %>%
  group_by(!!! group_variables, cohortday, state, next_state) %>%
  summarise(
    users = sum(n),
    users_all = sum(state_count),
    mean_weighted = weighted.mean(trans_prob, state_count),
    mean = mean(trans_prob),
    median = median(trans_prob),
    variance_weighted = weighted.var(trans_prob, state_count, na.rm=TRUE),
    variance = var(trans_prob, na.rm=TRUE),
    se_weighted = sqrt(variance_weighted) / sqrt(n()),
    se = sqrt(variance) / sqrt(n()),
    dec.05 = quantile(trans_prob, .05),
    dec.1 = quantile(trans_prob, .1),
    dec.2 = quantile(trans_prob, .2),
    dec.25 = quantile(trans_prob, .25),
    dec.3 = quantile(trans_prob, .3),
    dec.4 = quantile(trans_prob, .4),
    dec.5 = quantile(trans_prob, .5),
    dec.6 = quantile(trans_prob, .6),
    dec.7 = quantile(trans_prob, .7),
    dec.75 = quantile(trans_prob, .75),
    dec.8 = quantile(trans_prob, .8),
    dec.9 = quantile(trans_prob, .9),
    dec.95 = quantile(trans_prob, .95),
    error.95 = qt(.975, df=(n()-1)) * se_weighted,
    lower.95 = mean_weighted - error.95,
    upper.95 = mean_weighted + error.95
  ) %>%
  filter(users > 0)


hist_arpdau <- mydata %>%
  group_by(!!! group_variables, cohortday, state) %>%
  summarise(
    total_rev = sum(revenue),
    total_users = sum(n),
    mean_weighted = weighted.mean(arpdau, n),
    mean = mean(arpdau),
    median = median(arpdau),
    variance_weighted = weighted.var(arpdau, n, na.rm=TRUE),
    variance = var(arpdau, na.rm=TRUE),
    se_weighted = sqrt(variance_weighted) / sqrt(n()),
    se = sqrt(variance) / sqrt(n()),
    dec.05 = quantile(arpdau, .05),
    dec.1 = quantile(arpdau, .1),
    dec.2 = quantile(arpdau, .2),
    dec.25 = quantile(arpdau, .25),
    dec.3 = quantile(arpdau, .3),
    dec.4 = quantile(arpdau, .4),
    dec.5 = quantile(arpdau, .5),
    dec.6 = quantile(arpdau, .6),
    dec.7 = quantile(arpdau, .7),
    dec.75 = quantile(arpdau, .75),
    dec.8 = quantile(arpdau, .8),
    dec.9 = quantile(arpdau, .9),
    dec.95 = quantile(arpdau, .95),
    error.95 = qt(.975, df=(n()-1)) * se_weighted,
    lower.95 = mean_weighted - error.95,
    upper.95 = mean_weighted + error.95
  ) %>%
  filter(total_rev != 0)

# Perturb Transition Rates

type <- 2

dec.75.type2.cumarpi <- rep(NA, nrow(hist_transitions))

tm_time <- rep(NA, nrow(hist_transitions))
simulate_time <- rep(NA, nrow(hist_transitions))

for(i in 1:nrow(hist_transitions)){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_transitions[i,]
  tryCatch({
    a <- Sys.time()
    tm_model <- simulate_markov(install_transition=install_transition,
      transition_matrix=transition_matrix, rate_type='transition', c=transition_row$cohortday,
      change_states=transition_row$next_state, s=transition_row$state, p_new=transition_row$dec.75,
      type=type
    )
    # tm_model <- simulate_markov(install_transition, transition_matrix, state_probs, arpdaudata, c=transition_row$cohortday, s=transition_row$state, ns=transition_row$next_state, tp=transition_row$dec.75, type=type)
    b <- Sys.time()
    tm_results <- run_simulation(install_transition=tm_model$install_transition, transition_matrix=tm_model$transition_matrix, arpdaudata=arpdaudata, max_cohortday=max_cohortday)
    # tm_results <- run_simulation(tm_model$install_transition, tm_model$transition_matrix, tm_model$arpdaudata, max_cohortday = max_cohortday)
    c <- Sys.time()
    dec.75.type2.cumarpi[i] <- tm_results$cumarpi_max

    tm_time[i] <- b - a
    simulate_time[i] <- c - b
  }, warning = function(w){

  }, error = function(e){
    print(paste('Error on', i, e, sep=' '))
  }, finally = {

  })

}
dec.75.type2.cumarpi
hist_transitions$dec.75.type2.cumarpi <- dec.75.type2.cumarpi

saveRDS(hist_transitions, 'hist_transitions.RData')
# hist_transitions <- readRDS('hist_transitions.RData')
hist_sorted <- hist_transitions %>% arrange(desc(dec.75.type3.cumarpi))
# %>%
  # select(cohortday, state, next_state, mean_weighted, dec.75, dec.75.type3.cumarpi)


# Perturb ARPDAU

dec.75.cumarpi <- rep(NA, nrow(hist_arpdau))
for(i in 1:nrow(hist_arpdau)){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_arpdau[i,]
  tryCatch({
    a <- Sys.time()
    arpdau_changed <- simulate_arpdau(arpdaudata, transition_row$cohortday, transition_row$state, transition_row$dec.75)
    # tm_model <- simulate_markov(install_transition, transition_matrix, arpdaudata, c=transition_row$cohortday, s=transition_row$state, a=transition_row$dec.75)
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=state_probs, arpdaudata=tm_model$arpdaudata, max_cohortday = max_cohortday)
    c <- Sys.time()
    dec.75.cumarpi[i] <- tm_results$cumarpi_max

    tm_time[i] <- b - a
    simulate_time[i] <- c - b
  }, warning = function(w){

  }, error = function(e){
    print(paste('Error on', i, e, sep=' '))
  }, finally = {

  })

}
dec.75.cumarpi
hist_arpdau$dec.75.cumarpi <- dec.75.cumarpi

arpdau_sorted <- hist_arpdau %>% arrange(desc(dec.75.cumarpi))

%>%
  select(cohortday, state, mean_weighted, dec.75, dec.75.cumarpi)


beep(8)