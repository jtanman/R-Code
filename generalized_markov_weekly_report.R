# By: James Tan

# Date: 1/15/2019

p_load(reshape2, plotly, zoo, expm, beepr, scales, tidyverse)

source('~/.Rprofile')
source('~/MZ/R Code/generalized_markov_model_helper.R')
setwd(datapath)

dataname <- 'niso_dark_world_markov'
startdate <- as.Date('2018-11-01')
enddate <- as.Date('2019-01-20')
daterange <- paste(startdate, enddate, sep='_')
filename <- paste0(dataname, '_', daterange)


mydata <- read.csv(paste0('revenue_impact_dark_world', '.csv'))

mydata <- read_csv(paste0('revenue_impact_dark_world', '.csv'), col_types = cols(
  campaign_type = col_factor(NULL),
  install_platform = col_factor(NULL),
  dup_account = col_factor(NULL),
  state = col_factor(NULL),
  next_state = col_factor(NULL)
))

# hive parsing

mydata <- mydata %>%
  filter(install_platform %in% c('android', 'iOS')) %>%
  rename(date = curr_date) %>%
  mutate(
    trans_prob = n / state_count,
    arpdau_overall = revenue_overall / state_count,
    arpdau = revenue / n
  )

saveRDS(mydata, paste0('revenue_impact_dark_world', '.RData'))
mydata <- readRDS(paste0(filename, '.RData'))

# Date filter

weekdata <- filter(mydata, date > max(date) - 8)
histdata <- filter(mydata, date <= max(date) - 8)

weekdata <- filter(mydata, date >= as.Date('2019-04-21'))
histdata <- filter(mydata, date < as.Date('2019-04-21'))

daterange <- paste0(as.Date(min(weekdata$date)), '_', as.Date(max(weekdata$date)))
filename <- paste0(dataname, '_', daterange)

# group_variables <- quos(campaign_type, install_platform, dup_account)
group_variables <- NULL

hist_markov <- buildModel(histdata, group_variables)
hist_max_cohortday <- hist_markov$max_cohortday
hist_transitiondata <- hist_markov$transitiondata
hist_install_transition <- hist_markov$install_transition
hist_transition_matrix <- hist_markov$transition_matrix
hist_arpdaudata <- hist_markov$arpdaudata
hist_transitionMatrices <- hist_markov$transitionMatrices
hist_state_probs <- hist_markov$state_probs
hist_simulation_results <- hist_markov$simulation_results
hist_result <- hist_markov$result
hist_result_summary <- hist_markov$result_summary

week_markov <- buildModel(weekdata, group_variables)
week_max_cohortday <- week_markov$max_cohortday
week_transitiondata <- week_markov$transitiondata
week_install_transition <- week_markov$install_transition
week_transition_matrix <- week_markov$transition_matrix
week_arpdaudata <- week_markov$arpdaudata
week_transitionMatries <- week_markov$transitionMatries
week_state_probs <- week_markov$state_probs
week_simulation_results <- week_markov$simulation_results
week_result <- week_markov$result
week_result_summary <- week_markov$result_summary

hist_variances <- getHistoricalVariance(hist_transitiondata, histdata, group_variables)
hist_transitions <- hist_variances$hist_transitions
hist_arpdau <- hist_variances$hist_arpdau

saveRDS(hist_transitions, paste0(filename, '_hist_transitions.RData'))
saveRDS(hist_arpdau, paste0(filename, '_hist_arpdau.RData'))

hist_transitions <- readRDS(paste0(filename, '_hist_transitions.RData'))
hist_arpdau <- readRDS(paste0(filename, '_hist_arpdau.RData'))

# Compare current data to historical

hist_compare <- merge(hist_transitions, week_transition_matrix, by=c('cohortday', 'state', 'next_state')) %>%
  mutate(
    tscore = (trans_prob - mean_weighted) / se_weighted
  ) %>%
  rename(current_rate = trans_prob)

max_cohortday <- max(hist_compare$cohortday)
cumarpi_normal <- filter(hist_result_summary, cohortday == max_cohortday) %>% pull(cumarpi)
total_installs <- sum(filter(hist_transitions, cohortday == 0)$users)

type <- 2

current_rate.type2.cumarpi <- rep(NA, nrow(hist_compare))

tm_time <- rep(NA, nrow(hist_compare))
simulate_time <- rep(NA, nrow(hist_compare))

test_indices <- c(seq(1,5, 1), seq(6, 1370, 100), seq(1375, 1379, 1))

for(i in 1:max(which(hist_compare$cohortday < max_cohortday))){
# for(i in test_indices){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_compare[i,]
  tryCatch({
    a <- Sys.time()
    tm_model <- simulate_markov(install_transition=hist_install_transition,
                                transition_data=hist_transitionMatrices, rate_type='transition', c=transition_row$cohortday,
                                change_states=transition_row$next_state, s=transition_row$state, p_new=transition_row$current_rate,
                                type=type
                               )
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=hist_state_probs, install_transition=tm_model$install_transition,
                                          transition_data=tm_model$transition_data, arpdaudata=hist_arpdaudata,
                                          startday=transition_row$cohortday, max_cohortday=max_cohortday)
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

write.csv(hist_pretty, paste0(filename, '_trans_compare.csv'), row.names=FALSE)

# Compare ARPDAU

hist_compare <- merge(hist_arpdau, week_arpdaudata, by=c('cohortday', 'state'), all.x=TRUE) %>%
  mutate(
    tscore = (arpdau - mean_weighted) / se_weighted
  ) %>%
  rename(current_arpdau = arpdau)

max_cohortday <- max(hist_compare$cohortday)
cumarpi_normal <- filter(hist_result_summary, cohortday == max_cohortday) %>% pull(cumarpi)

# run_simulation(state_probs=state_probs, arpdaudata=arpdaudata, max_cohortday=max_cohortday)

current_arpdau.cumarpi <- rep(NA, nrow(hist_compare))
for(i in 1:max(which(hist_compare$cohortday <= max_cohortday))){
  if(i %% 10 == 0){print(i)}
  transition_row <- hist_compare[i,]
  tryCatch({
    a <- Sys.time()
    arpdau_changed <- simulate_arpdau(hist_arpdaudata, transition_row$cohortday, transition_row$state, transition_row$current_arpdau)
    b <- Sys.time()
    tm_results <- run_simulation(state_probs=hist_state_probs, arpdaudata=arpdau_changed, max_cohortday=max_cohortday)
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
    cumarpi_percent_lift = cumarpi_lift / cumarpi_normal,
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
    cumarpi_percent_lift = percent(cumarpi_percent_lift),
    total_rev_lift_current_rate = format(round(total_rev_lift_current_rate, 2), nsmall = 2)
  ) %>%
  select(age, state, average_arpdau, current_arpdau, target_25th_percentile, target_75th_percentile, se_weighted, tscore, cumarpi_lift, cumarpi_percent_lift, total_rev_lift_current_rate)


write.csv(arpdau_pretty, paste0(filename, '_arpdau_compare.csv'), row.names=FALSE)
