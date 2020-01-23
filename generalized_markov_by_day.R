# By: James Tan

# Date: 9/24/2018

p_load(reshape2, lubridate, zoo, expm, beepr, scales, ggplot2, dplyr)

source('~/.Rprofile')
source('~/MZ/R Code/revenue_impact_finance_helper.R')
source('~/MZ/R Code/generalized_markov_model_helper.R')
setwd(datapath)

NISO_START_DATE <- as.Date('2017-06-28')
MISO_START_DATE <- as.Date('2018-11-01')

game <- 'niso'

if(game == 'miso'){
  startdate <- MISO_START_DATE
  enddate <- as.Date('2019-01-20')
}else if(game == 'niso'){
  startdate <- NISO_START_DATE
  enddate <- as.Date('2019-02-09')
}else{
  stop('game not recognized')
}

dataname <- paste0(game, '_markov_data')

daterange <- paste(startdate, enddate, sep='_')
filename <- paste0(dataname, '_', daterange)


mydata <- read.csv(paste0(filename, '.csv'))

# hive parsing

mydata <- mydata %>%
  select(-X) %>%
  filter(install_platform %in% c('android', 'iOS')) %>%
  rename(date = curr_date) %>%
  mutate(
    install_date = as.Date(install_date),
    date = as.Date(date),
    install_platform = factor(install_platform),
    state = as.factor(state),
    next_state = as.factor(next_state),
    trans_prob = n / state_count,
    arpdau_overall = revenue_overall / state_count,
    arpdau = revenue / n
  )

# vertica parsing

# mydata <- mydata %>%
#   filter(!is.na(install_platform)) %>%
#   rename(date = DATE) %>%
#   mutate(
#     install_date = as.Date(install_date),
#     date = as.Date(date),
#     state = as.factor(state),
#     next_state = as.factor(next_state),
#     trans_prob = n / state_count,
#     arpdau_overall = revenue_overall / state_count,
#     arpdau = revenue / n
#   )
#
# summary(mydata)
# sapply(mydata, class)

saveRDS(mydata, paste0(filename, '.RData'))
mydata <- readRDS(paste0(filename, '.RData'))

# Date filter

# mydata <- filter(mydata, date > max(date) - 8)
daterange <- paste0(as.Date(min(mydata$date)), '_', as.Date(max(mydata$date)))
filename <- paste0(dataname, '_', daterange)

max_cohortday <- max(mydata$cohortday)

# group_variables <- quos(campaign_type, install_platform, dup_account)
group_variables <- NULL

markovmodel <- buildModel(mydata, group_variables, max_cohortday)

saveRDS(markovmodel, sprintf('%s_markov_model.RData', filename))
markovmodel <- readRDS(sprintf('%s_markov_model.RData', filename))

transitiondata <- markovmodel$transitiondata
install_transition <- markovmodel$install_transition
transition_matrix <- markovmodel$transition_matrix
arpdaudata <- markovmodel$arpdaudata
transitionMatrices <- markovmodel$transitionMatrices
state_probs <- markovmodel$state_probs
simulation_results <- markovmodel$simulation_results
result <- markovmodel$result
result_summary <- markovmodel$result_summary



actual_results <- mydata %>%
  group_by(cohortday) %>%
  mutate(
    retention_n = ifelse(as.numeric(as.character(state)) >= 4, n, 0),
  ) %>%
  summarise(
    retention = sum(retention_n) / sum(n),
    arpi = sum(revenue) / sum(n)
  ) %>%
  mutate(
    cumarpi = cumsum(arpi)
  ) %>%
  rename(retention_actual = retention, arpi_actual = arpi, cumarpi_actual = cumarpi) %>%
  left_join(result_summary, by='cohortday')

actual_state_probs <- mydata %>%
  # filter(date != max(date)) %>%
  # filter(install_date == as.Date("2017-9-30")) %>%
  group_by(cohortday, state) %>%
  summarise(
    n = sum(n)
  ) %>%
  group_by(cohortday) %>%
  mutate(
    cohortday_total = sum(n),
    actual_prob = n / sum(n)
  ) %>% arrange(cohortday, state)

next_state_probs <- mydata %>%
  filter(date != max(date)) %>%
  # filter(install_date == as.Date("2017-9-30")) %>%
  filter(!is.na(next_state)) %>%
  group_by(cohortday, next_state) %>%
  summarise(
    n_next = sum(n)
  ) %>%
  group_by(cohortday) %>%
  mutate(
    cohortday_total_next = sum(n_next),
    actual_prob_next = n_next / sum(n_next)
  ) %>%
  ungroup() %>%
  mutate(
    cohortday = cohortday + 1
  ) %>%
  rename(state = next_state)

actual_probs <- left_join(actual_state_probs, next_state_probs, by=c('cohortday', 'state')) %>% left_join(result, by=c('cohortday', 'state')) %>% arrange(cohortday)

actual_probs %>% group_by(cohortday) %>% summarise(n = sum(n), n_next = sum(n_next), p = sum(actual_prob), p_next = sum(actual_prob_next))

testdata <- filter(mydata, install_date == as.Date('2017-7-31')) %>%
  filter(cohortday == 0 | cohortday == 1) %>%
  group_by(cohortday, state, next_state) %>%
  summarise(n = sum(n))

# Historical Variance

hist_variances <- getHistoricalVariance(transitiondata, mydata, group_variables)
hist_transitions <- hist_variances$hist_transitions
hist_arpdau <- hist_variances$hist_arpdau

saveRDS(hist_transitions, paste0(filename, '_hist_transitions.RData'))
saveRDS(hist_arpdau, paste0(filename, '_hist_arpdau.RData'))

hist_transitions <- readRDS(paste0(filename, '_hist_transitions.RData'))
hist_arpdau <- readRDS(paste0(filename, '_hist_arpdau.RData'))


# Simulate Perturbations

# it <- install_transition
# tm <- transition_matrix
# tms <- transitionMatrices
# ad <- arpdaudata
# sp <- state_probs
#
# install_transition <- it
# transition_matrix <- tm
# transitionMatrices <- tms
# arpdaudata <- ad
# state_probs <- sp
#
#
# rate_type <- 'transition'
# c <- 1
# change_states <- 4
# s <- 4
# p_new <- .4
# type <- 2
# affected_states <- c(0, 1, 2)
# weights <- c(.6, .3, .1)
# # a <- 6
#
# View(filter(transition_matrix, cohortday==c))
# filter(state_probs, cohortday == c)
# new_matrices <- simulate_markov(install_transition, transition_matrix, state_probs, arpdaudata, rate_type=rate_type, c=c, change_states=change_states, s=s, affected_states=affected_states, tp=tp, a=a, type=type, cs_weights=weights)
# # changeVector(x, change_states=change_states_index, tp, type=type, affected_states=opposite_state_index, weights=weights)
# filter(new_matrices$state_probs, cohortday == c)
# # new_matrices <- simulate_markov(it, tm, ad, sp, c=c, s=s, ns=ns, tp=tp, type=type, cs=cs, cs_weights=cs_weights, a=a)
#
# filter(transition_matrix, cohortday==c, state==s)
# filter(new_matrices$transition_matrix, cohortday==c, state==s)
#
# filter(arpdaudata, cohortday==c, state==s)
# filter(new_matrices$arpdaudata, cohortday==c, state==s)

type <- 2

dec.75.type2.cumarpi <- rep(NA, nrow(hist_transitions))

tm_time <- rep(NA, nrow(hist_transitions))
simulate_time <- rep(NA, nrow(hist_transitions))

max_cohortday <- max(hist_transitions$cohortday)
cumarpi_normal <- filter(result_summary, cohortday == max_cohortday) %>% pull(cumarpi)
total_installs <- sum(filter(hist_transitions, cohortday == 0)$users)

# state_probs <- getStateProbs(install_transition, transitionMatrices, max_cohortday)
# run_simulation(state_probs=state_probs, arpdaudata=arpdaudata, max_cohortday=max_cohortday)
# run_simulation(install_transition=install_transition, transition_data=transition_matrix, arpdaudata=arpdaudata, max_cohortday=max_cohortday)

for(i in 1:max(which(hist_transitions$cohortday <= max_cohortday))){
# for(i in c(seq(1,5, 1), seq(6, 1740, 100), seq(1741, 1747, 1))){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_transitions[i,]
  tryCatch({
    a <- Sys.time()
    tm_model <- simulate_markov(install_transition=install_transition,
      transition_data=transitionMatrices, rate_type='transition', c=transition_row$cohortday,
      change_states=transition_row$next_state, s=transition_row$state, p_new=transition_row$dec.75,
      type=type
    )
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=state_probs, install_transition=tm_model$install_transition,
                                          transition_data=tm_model$transition_data, arpdaudata=arpdaudata,
                                          startday=transition_row$cohortday, max_cohortday=max_cohortday)

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
tail(dec.75.type2.cumarpi, 20)
hist_transitions$dec.75.type2.cumarpi <- dec.75.type2.cumarpi

saveRDS(hist_transitions, paste0(filename, '_hist_transitions.RData'))
# hist_transitions <- readRDS(paste0(filename, '_hist_transitions.RData'))

hist_pretty <- hist_transitions %>%
  arrange(desc(dec.75.type2.cumarpi)) %>%
  mutate(
    state = factor(state, labels=STATES),
    next_state = factor(next_state, labels=STATES),
    cumarpi_lift = dec.75.type2.cumarpi - cumarpi_normal,
    cumarpi_percent_lift = cumarpi_lift / cumarpi_normal,
    total_installs = total_installs,
    total_rev_lift = cumarpi_lift * total_installs,
  ) %>%
  rename(age = cohortday, dec.75.cumarpi = dec.75.type2.cumarpi) %>%
  mutate(
    mean_weighted = percent(mean_weighted),
    mean = percent(mean),
    median = percent(median),
    variance_weighted = percent(variance_weighted),
    variance = percent(variance),
    se_weighted = percent(se_weighted),
    se = percent(se),
    dec.75.cumarpi = format(round(dec.75.cumarpi, 2), nsmall = 2),
    cumarpi_lift = format(round(cumarpi_lift, 4), nsmall = 4),
    cumarpi_percent_lift = percent(cumarpi_percent_lift),
    total_rev_lift = dollar(total_rev_lift),
    dec.25 = percent(dec.25),
    dec.75 = percent(dec.75)
  ) %>%
  select(age, state, next_state, mean_weighted, se_weighted, dec.25, dec.75, dec.75.cumarpi, cumarpi_lift, cumarpi_percent_lift, total_rev_lift, everything())

write.csv(hist_pretty, paste0(filename, '_hist_transitions_pretty.csv'), row.names=FALSE)
# hist_pretty <- read_csv(paste0(filename, '_hist_transitions_pretty.csv'))

# Perturb ARPDAU

max_cohortday <- max(hist_arpdau$cohortday)
cumarpi_normal <- filter(simulation_results$result_summary, cohortday == max_cohortday) %>% pull(cumarpi)
total_installs <- sum(filter(hist_transitions, cohortday == 0)$users)

dec.75.cumarpi <- rep(NA, nrow(hist_arpdau))
for(i in 1:max(which(hist_arpdau$cohortday <= max_cohortday))){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_arpdau[i,]
  tryCatch({
    a <- Sys.time()
    arpdau_changed <- simulate_arpdau(arpdaudata, transition_row$cohortday, transition_row$state, transition_row$dec.75)
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=state_probs, arpdaudata=arpdau_changed, max_cohortday=max_cohortday)
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

saveRDS(hist_arpdau, paste0(filename, '_hist_arpdau.RData'))
# hist_arpdau <- readRDS('hist_arpdau.RData')

arpdau_pretty <- hist_arpdau
levels(arpdau_pretty$state) <- STATES
arpdau_pretty <- arpdau_pretty %>%
  arrange(desc(dec.75.cumarpi)) %>%
  mutate(
    cumarpi_lift = dec.75.cumarpi - cumarpi_normal,
    cumarpi_percent_lift = cumarpi_lift / cumarpi_normal,
    total_installs = total_installs,
    total_rev_lift = cumarpi_lift * total_installs,
  ) %>%
  rename(age = cohortday) %>%
  mutate(
    mean_weighted = format(round(mean_weighted, 2), nsmall = 2),
    mean = format(round(mean, 2), nsmall = 2),
    median = format(round(median, 2), nsmall = 2),
    variance_weighted = format(round(variance_weighted, 2), nsmall = 2),
    variance = format(round(variance, 2), nsmall = 2),
    se_weighted = format(round(se_weighted, 2), nsmall = 2),
    se = format(round(se, 2), nsmall = 2),
    dec.75.cumarpi = format(round(dec.75.cumarpi, 2), nsmall = 2),
    cumarpi_lift = format(round(cumarpi_lift, 4), nsmall = 4),
    cumarpi_percent_lift = percent(cumarpi_percent_lift),
    total_rev_lift = dollar(total_rev_lift),
    total_rev = dollar(total_rev),
    dec.25 = format(round(dec.25, 2), nsmall = 2),
    dec.75 = format(round(dec.75, 2), nsmall = 2)
  ) %>%
  select(age, state, mean_weighted, se_weighted, dec.25, dec.75, dec.75.cumarpi, cumarpi_lift, cumarpi_percent_lift, total_rev_lift, everything())


write.csv(arpdau_pretty, paste0(filename, '_hist_arpdaus_pretty.csv'), row.names=FALSE)




# Compare two sets of transition rates and arpdaus

save(max_cohortday, install_transition, transition_matrix, arpdaudata, transitionMatrices, state_probs, simulation_results, file=paste0(filename, '_compare_data.RData'))
save(max_cohortday, install_transition, transition_matrix, arpdaudata, transitionMatrices, state_probs, simulation_results, hist_transitions, hist_arpdau, file=paste0(filename, '_all_data.RData'))

load('miso_markov_data_2018-12-29_2019-01-04_compare_data.RData')
tm_new <- transition_matrix
arpdau_new <- arpdaudata

load('miso_markov_data_2018-11-01_2018-12-28_all_data.RData')

# Compare current data to historical

transition_compare <- tm_new

hist_compare <- merge(hist_transitions, transition_compare, by=c('cohortday', 'state', 'next_state')) %>%
  mutate(
    tscore = (trans_prob - mean_weighted) / se_weighted
  ) %>%
  rename(current_rate = trans_prob)

max_cohortday <- max(hist_compare$cohortday)
cumarpi_normal <- filter(simulation_results$result_summary, cohortday == max_cohortday) %>% pull(cumarpi)
total_installs <- sum(filter(hist_transitions, cohortday == 0)$users)

type <- 2

current_rate.type2.cumarpi <- rep(NA, nrow(hist_compare))

tm_time <- rep(NA, nrow(hist_compare))
simulate_time <- rep(NA, nrow(hist_compare))

run_simulation(install_transition=install_transition, transition_data=transition_matrix, arpdaudata=arpdaudata, max_cohortday=max_cohortday)

for(i in 1:max(which(hist_compare$cohortday < max_cohortday))){
  # for(i in c(seq(1,5, 1), seq(6, 6506, 250), seq(7300, 7308, 1))){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_compare[i,]
  tryCatch({
    a <- Sys.time()
    tm_model <- simulate_markov(install_transition=install_transition,
                                transition_data=transition_matrix, rate_type='transition', c=transition_row$cohortday,
                                change_states=transition_row$next_state, s=transition_row$state, p_new=transition_row$current_rate,
                                type=type
    )
    b <- Sys.time()
    tm_results <- run_simulation(install_transition=tm_model$install_transition, transition_data=tm_model$transition_data, arpdaudata=arpdaudata, max_cohortday=max_cohortday)
    c <- Sys.time()

    current_rate.type2.cumarpi[i] <- tm_results$cumarpi_max

    tm_time[i] <- b - a
    simulate_time[i] <- c - b
  }, warning = function(w){

  }, error = function(e){
    print(paste('Error on', i, e, sep=' '))
  }, finally = {

  })

}
current_rate.type2.cumarpi
hist_compare$current_rate.type2.cumarpi <- current_rate.type2.cumarpi

hist_sorted <- hist_compare %>% arrange(desc(current_rate.type2.cumarpi))

hist_pretty <- hist_sorted %>%
  mutate(
    state = factor(state, labels=STATES),
    next_state = factor(next_state, labels=STATES),
    cumarpi_lift = current_rate.type2.cumarpi - cumarpi_normal,
    cumarpi_percent_lift = cumarpi_lift / cumarpi_normal,
    total_installs = total_installs,
    total_rev_lift_current_rate = cumarpi_lift * total_installs,
  ) %>%
  rename(age = cohortday, average_rate = mean_weighted, target_25th_percentile = dec.25, target_75th_percentile = dec.75) %>%
  mutate(
    average_rate = percent(average_rate),
    current_rate = percent(current_rate),
    target_25th_percentile = percent(target_25th_percentile),
    target_75th_percentile = percent(target_75th_percentile),
    se_weighted = percent(se_weighted),
    tscore = format(round(tscore, 2), nsmall = 2),
    cumarpi_lift = format(round(cumarpi_lift, 4), nsmall = 4),
    cumarpi_percent_lift = percent(cumarpi_percent_lift),
    total_rev_lift_current_rate = format(round(total_rev_lift_current_rate, 2), nsmall = 2)
  ) %>%
  select(age, state, next_state, average_rate, current_rate, target_25th_percentile, target_75th_percentile, se_weighted, tscore, cumarpi_lift, cumarpi_percent_lift, total_rev_lift_current_rate)

write.csv(hist_pretty, paste(filename, '_trans_compare.csv'), row.names=FALSE)
# saveRDS(hist_compare, paste0(filename, '_hist_transitions.RData'))


# Compare ARPDAU

arpdau_compare <- arpdau_new

hist_compare <- merge(hist_arpdau, arpdau_compare, by=c('cohortday', 'state')) %>%
  mutate(
    tscore = (arpdau - mean_weighted) / se_weighted
  ) %>%
  rename(current_arpdau = arpdau)

max_cohortday <- max(hist_compare$cohortday)
cumarpi_normal <- filter(simulation_results$result_summary, cohortday == max_cohortday) %>% pull(cumarpi)

run_simulation(state_probs=state_probs, arpdaudata=arpdaudata, max_cohortday=max_cohortday)

current_arpdau.cumarpi <- rep(NA, nrow(hist_arpdau))
for(i in 1:max(which(hist_arpdau$cohortday <= max_cohortday))){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_compare[i,]
  tryCatch({
    a <- Sys.time()
    arpdau_changed <- simulate_arpdau(arpdaudata, transition_row$cohortday, transition_row$state, transition_row$current_arpdau)
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=state_probs, arpdaudata=arpdau_changed, max_cohortday=max_cohortday)
    c <- Sys.time()
    current_arpdau.cumarpi[i] <- tm_results$cumarpi_max

    tm_time[i] <- b - a
    simulate_time[i] <- c - b
  }, warning = function(w){

  }, error = function(e){
    print(paste('Error on', i, e, sep=' '))
  }, finally = {

  })

}
current_arpdau.cumarpi
hist_compare$current_arpdau.cumarpi <- current_arpdau.cumarpi

# saveRDS(hist_arpdau, paste0(filename, '_hist_arpdau.RData'))
# hist_arpdau <- readRDS('hist_arpdau.RData')
arpdau_sorted <- hist_compare %>% arrange(desc(current_arpdau.cumarpi))

levels(arpdau_sorted$state) <- STATES
arpdau_pretty <- arpdau_sorted %>%
  mutate(
    cumarpi_lift = current_arpdau.cumarpi - cumarpi_normal,
    total_installs = total_installs,
    total_rev_lift_current_rate = cumarpi_lift * total_installs,
  ) %>%
  rename(age = cohortday, average_arpdau = mean_weighted, target_25th_percentile = dec.25, target_75th_percentile = dec.75) %>%
  mutate(
    average_arpdau = format(round(average_arpdau, 2), nsmall = 2),
    current_arpdau = format(round(current_arpdau, 2), nsmall = 2),
    target_25th_percentile = format(round(target_25th_percentile, 2), nsmall = 2),
    target_75th_percentile = format(round(target_75th_percentile, 2), nsmall = 2),
    se_weighted = format(round(se_weighted, 2), nsmall = 2),
    tscore = format(round(tscore, 2), nsmall = 2),
    cumarpi_lift = format(round(cumarpi_lift, 4), nsmall = 4),
    total_rev_lift_current_rate = format(round(total_rev_lift_current_rate, 2), nsmall = 2)
  ) %>%
  select(age, state, average_arpdau, current_arpdau, target_25th_percentile, target_75th_percentile, se_weighted, tscore, cumarpi_lift, total_rev_lift_current_rate)

write.csv(arpdau_pretty, paste(filename, '_arpdau_compare.csv'), row.names=FALSE)
