# By: James Tan

# Date: 9/18/2018

STATES <- c('$0 Churn', '$5 Churn', '$20 Churn', '$100+ Churn', '$0 DAU', '$5 DAU', '$20 DAU', '$100+ DAU')

numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

monnb <- function(d){
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
}

getCohortdayMatrix <- function(tm, n){
  if(n == 'install'){
    return(tm['-1', colnames(tm)[which(floor(as.numeric(colnames(tm)) / 8) == 0)]])
  }
  row_states <- rownames(tm)[which(floor(as.numeric(rownames(tm)) / 8) == n)]
  col_states <- colnames(tm)[which(floor(as.numeric(colnames(tm)) / 8) == n + 1)]
  return(tm[row_states, col_states])
}

getStateName <- function(n){
  n <- as.numeric(n)
  day <- paste0('d', floor(n / 8))
  state <- case_when(
    n %% 8 == 0 ~ '0 Churn',
    n %% 8 == 1 ~ '5 Churn',
    n %% 8 == 2 ~ '20 Churn',
    n %% 8 == 3 ~ '100 Churn',
    n %% 8 == 4 ~ '0 DAU',
    n %% 8 == 5 ~ '5 DAU',
    n %% 8 == 6 ~ '20 DAU',
    n %% 8 == 7 ~ '100 DAU'
  )
  return(paste0(day,' ', state))
}

putCohortdayMatrix <- function(tm, n, cohortmatrix){
  if(n == 'install'){
    if(sum(cohortmatrix) != 1){
      stop('Row does not sum to 1')
    }
    tm['-1', colnames(tm)[which(floor(as.numeric(colnames(tm)) / 8) == 0)]] <- cohortmatrix
    return(tm)
  }
  if(!all(apply(cohortmatrix, 1, sum) == 1)){
    stop('All rows do not sum to 1')
  }
  row_states <- rownames(tm)[which(floor(as.numeric(rownames(tm)) / 8) == n)]
  col_states <- colnames(tm)[which(floor(as.numeric(colnames(tm)) / 8) == n + 1)]
  tm[row_states, col_states] <- cohortmatrix
  return(tm)
}

weighted.var <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    (sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2))
}

# types of perturbations
# 1. specify all transition probabilities for a given state to the next states
# 2. scale every other transition probability down so that sum is 1
# 3: change only the scale from the states specified by the cs (change states) parameter - so if s=4, ns=7, and cs=c(4, 5, 6), then increasing tp 4 to 7 will decrease evenly probability from 4 to 4, 5, and 6. This can be used to keep retention the same but increase conversion.
# 4: change only the scale from the states specified by the cs (change states) parameter and by the weight to each state in the change states - so if s=4, ns=7, and cs=c(4, 5, 6), and cs_weights=(.6, .3, .1) then increasing tp 4 to 7 will decrease probability from 4 to 4, 5, and 6 weighted by cs_weights.


changeVector <- function(vector, change_index, values, type, affected_index=NA, weights=NA){
  # change probability vector such that vector still sums to 1
  
  # browser()

  if(length(values) != length(change_index)){
    stop('Lengths of values and change_index do not match')
  }

  if(!isTRUE(all.equal(sum(vector), 1))){
    stop('Vector does not sum to 1')
  }

  n <- length(vector)

  if(type == 1 & length(values) == length(vector)){

    vector <- values

  }else if(type == 2 & length(values) == 1){

    prob_change <- 1 - values
    trans_prob_orig <- 1 - vector[change_index]
    if(trans_prob_orig == 0){
      vector = ifelse(index(vector) == change_index, values, prob_change / (length(vector) - 1))
    }else{
      vector <- ifelse(index(vector) == change_index, values, vector * prob_change / trans_prob_orig)  
    }

  }else if(type == 3 & length(values) == 1){

    if(change_index %in% affected_index){
      stop('Change index cannot be in affected indices')
    }

    prob_as_sum <- sum(vector[affected_index])
    trans_prob_orig <- vector[change_index]
    delta <- values - trans_prob_orig
    vector[change_index] <- values
    vector[affected_index] <- vector[affected_index] * (prob_as_sum - delta) / prob_as_sum

  }else if(type == 4 & length(values) == 1){


    if(any(weights < 0)){
      stop('Negative weight')
    }
    if(change_index %in% affected_index){
      stop('Change index cannot be in affected indices')
    }

    weights <- weights / sum(weights)
    weights <- weights[order(affected_index)]
    affected_index <- affected_index[order(affected_index)]

    prob_cs <- vector[affected_index]
    trans_prob_orig <- vector[change_index]
    prob_not_cs <- vector[! index(vector) %in% c(change_index, affected_index)]

    scalar <- ((1 - values - sum(prob_not_cs)) - sum(prob_cs)) / sum(weights * prob_cs)
    percent_change <- scalar * weights
    weights <- 1 + percent_change
    prob_cs_new <- weights * prob_cs

    vector[change_index] <- values
    vector[affected_index] <- prob_cs_new

  }else{
    stop('Incompatible type and dimension of values')
  }

  if(any(vector < 0)){
    stop('Vector value under 0')
  }

  if(!isTRUE(all.equal(sum(vector), 1))){
    stop('Vector values do not sum to 1')
  }

  return(vector)
}


simulate_markov <- function(install_transition=NA, transition_data=NA, state_probs=NA, rate_type='transition', c, change_states, s=NA, affected_states=NA, p_new=NA, type=2, weights=NA){
  # rate_type: whether to change transition rates or state probabilities on a given cohortday, 'transition' for transition rates, 'state' for state probabilities
  # c: cohortday
  # change_states: states to change (could either be install state or next states)
  # s: state to change transitions from
  # affected_states: states to change to accomodate change in trans_prob, but not changed directly
  # p_new: new probability to set
  # type: type of perturbation

  # types of perturbations
  # 1. specify all transition probabilities for a given state to the next states
  # 2. scale every other transition probability so that sum is 1
  # 3: change only the scale from the states specified by the cs (change states) parameter - so if s=4, ns=7, and cs=c(4, 5, 6), then increasing p_new 4 to 7 will decrease evenly probability from 4 to 4, 5, and 6. This can be used to keep retention the same but increase conversion.
  # 4: change only the scale from the states specified by the cs (change states) parameter and by the weight to each state in the change states - so if s=4, ns=7, and cs=c(4, 5, 6), and weights=(.6, .3, .1) then increasing p_new 4 to 7 will decrease probability from 4 to 4, 5, and 6 weighted by weights.
  # 5. change only the scale from the transition's opposite LTV group - so increasing a transition to 4 decreases the transition to 0 by the same amount, (1, 5), (2, 6), and (3, 7) are the other pairs

  # if(!is.numeric(s)){
  #   s <- as.numeric(as.character(s))
  # }
  # browser()
  
  if(length(p_new) != length(change_states)){
    stop('Lengths of p_new and change_states do not match')
  }

  if(!any(is.na(p_new))){
    # change probability

    if(rate_type == 'transition' & c == 'install'){
      # change install to cohortday 0 state transition

      matrix_change <- install_transition
      match_var <- quo(state)
      prob_var <- quo(trans_prob)

    }else if(rate_type == 'transition' & c %% 1 == 0){
      # change cohortday c transition rates

      if(inherits(transition_data, 'list')){
        # transition data given as list of transition matrices

        return(simulate_markov_matrix_list(install_transition, transition_data, state_probs, rate_type, c, change_states, s, affected_states, p_new, type, weights))

      }else if(inherits(transition_data, 'data.frame')){
        # transition data given as data frame

        transition_matrix <- transition_data
        matrix_change <- filter(transition_matrix, cohortday == c, state == s) %>% arrange(next_state)
        match_var <- quo(next_state)
        prob_var <- quo(trans_prob)

      }else{
        stop('Transition data not specified in acceptable format')
      }

    }else if(rate_type == 'state' & c %% 1 == 0){
      # change state probability directly

      matrix_change <- filter(state_probs, cohortday == c)
      match_var <- quo(state)
      prob_var <- quo(prob)

    }else{
      stop('rate_type and c do not match or c is not an integer')
    }

    prob_orig <- pull(matrix_change, !! prob_var)
    change_index <- which(pull(matrix_change, !! match_var) %in% change_states)
    affected_index <- which(pull(matrix_change, !! match_var) %in% affected_states)

    if(type == 5){
      opposite_state <- (change_states + 4)  %% 8
      affected_index <- which(pull(matrix_change, !! match_var) == opposite_state)
      prob_new <- changeVector(vector=prob_orig, change_index=change_index, values=p_new, type=3, affected_index=affected_index, weights=weights)
    }else{
      prob_new <- changeVector(vector=prob_orig, change_index=change_index, values=p_new, type=type, affected_index=affected_index, weights=weights)
    }

    if(rate_type == 'transition' & c == 'install'){

      install_transition$trans_prob <- prob_new

    }else if(rate_type == 'transition' & c %% 1 == 0){

      transition_matrix <- transition_matrix %>%
        mutate(
          trans_prob = replace(trans_prob, cohortday == c & state == s, prob_new)
        )

    }else if(rate_type == 'state' & c %% 1 == 0){

      state_probs <- state_probs %>%
        mutate(
          prob = replace(prob, cohortday == c, prob_new)
        )

    }else{
      stop('rate_type and c do not match or c is not an integer')
    }

  }

  return(list(install_transition=install_transition, transition_data=transition_matrix, state_probs=state_probs))

}

simulate_markov_matrix_list <- function(install_transition=NA, transitionMatrices=NA, state_probs=NA, rate_type='transition', c, change_states, s=NA, affected_states=NA, p_new=NA, type=2, weights=NA){
  # simulate markov using transition_matrix data frame

  # browser()
  
  prob_orig <- transitionMatrices[[as.character(c)]][as.character(s),]
  change_index <- which(names(prob_orig) %in% change_states)
  affected_index <- which(names(prob_orig) %in% affected_states)

  if(type == 5){
    opposite_state <- (change_states + 4)  %% 8
    affected_index <- which(names(prob_orig) == opposite_state)
    prob_new <- changeVector(vector=prob_orig, change_index=change_index, values=p_new, type=3, affected_index=affected_index, weights=weights)
  }else{
    prob_new <- changeVector(vector=prob_orig, change_index=change_index, values=p_new, type=type, affected_index=affected_index, weights=weights)
  }

  transitionMatrices[[as.character(c)]][as.character(s),] <- prob_new
  return(list(install_transition=install_transition, transition_data=transitionMatrices, state_probs=state_probs))

}


simulate_arpdau <- function(arpdaudata, c, s, a_new){
  # change arpdau matrix

  if(!is.na(a_new)){
    arpdaudata <- arpdaudata %>%
      mutate(
        arpdau = replace(arpdau, cohortday == c & state == s, a_new)
      )
  }
  return(arpdaudata)
}

getTransitionMatrices <- function(install_transition, transition_matrix, max_cohortday=270){

  state_probs <- tibble(cohortday=0, state=install_transition$state, prob=install_transition$trans_prob)

  transitionMatrices <- list()
  transitionMatrices[['install']] <- matrix(install_transition$trans_prob, nrow=1, dimnames=list('prob', install_transition$state))

  temp_state <- transitionMatrices[['install']]

  for(i in min(transition_matrix$cohortday):(max_cohortday - 1)){
    # create list of transition matrices

    cohortday_transition <- filter(transition_matrix, cohortday == i)
    cohortday_matrix <- dcast(cohortday_transition, state ~ next_state, mean, value.var='trans_prob', fill=0, drop=TRUE)
    rownames(cohortday_matrix) <- cohortday_matrix$state
    cohortday_matrix <- as.matrix(select(cohortday_matrix, -state))
    transitionMatrices[[as.character(i)]] <- cohortday_matrix
  }

  return(transitionMatrices)

}


getStateProbs <- function(install_transition, transition_data, max_cohortday=270, startday=NA, state_probs=NA){
  # get cohortday state probabilities from the transition matrix
  # transition matrix can be given as list of transition matrices or as one dataframe
  # if startday is given, start from that day and simulate forward using given state probs as truth before then

  if(isTRUE(startday > max_cohortday - 1)){
    return(filter(state_probs, cohortday <= max_cohortday))
  }
  
  if(isTRUE(startday < 0)){
    warning('startday is less than 0')
    startday <- 0
  }

  if(isTRUE(startday %% 1 == 0 & is.data.frame(state_probs))){

    state_probs <- filter(state_probs, cohortday <= startday)
    temp_day <- filter(state_probs, cohortday == startday)
    temp_state <- matrix(temp_day$prob, nrow=1, dimnames=list('prob', temp_day$state))
    start_index <- startday

  }else{

    state_probs <- tibble(cohortday=0, state=install_transition$state, prob=install_transition$trans_prob)
    temp_state <- matrix(install_transition$trans_prob, nrow=1, dimnames=list('prob', install_transition$state))
    start_index <- 0

  }

  if(inherits(transition_data, 'list')){
    # transition data given as list of transition matrices

    for(i in start_index:(max_cohortday - 1)){

      cohortday_matrix <- transition_data[[as.character(i)]]
      temp_state <- temp_state[,colnames(temp_state) %in% rownames(cohortday_matrix), drop=FALSE]
      temp_state <- temp_state %*% cohortday_matrix
      df_state <- tibble(cohortday=(i+1), state=colnames(temp_state), prob=temp_state[1,])
      state_probs <- rbind(state_probs, df_state)
    }

  }else if(inherits(transition_data, 'data.frame')){
    # transition data given as data frame

    for(i in start_index:(max_cohortday - 1)){
      # create list of transition matrices
      cohortday_transition <- filter(transition_data, cohortday == i)
      cohortday_matrix <- dcast(cohortday_transition, state ~ next_state, mean, value.var='trans_prob', fill=0, drop=TRUE)
      rownames <- cohortday_matrix$state
      cohortday_matrix <- as.matrix(select(cohortday_matrix, -state))
      rownames(cohortday_matrix) <- rownames

      temp_state <- temp_state[,colnames(temp_state) %in% rownames(cohortday_matrix), drop=FALSE]
      temp_state <- temp_state %*% cohortday_matrix
      df_state <- tibble(cohortday=(i+1), state=colnames(temp_state), prob=temp_state[1,])
      state_probs <- rbind(state_probs, df_state)
    }

  }else{
    stop('Transition data not specified in acceptable format')
  }

  return(state_probs)

}

run_simulation <- function(state_probs=NA, install_transition=NA, transition_data=NA, arpdaudata, startday=NA, max_cohortday=270){
  # run simulation given either the state probability matrix OR transition matrices then combined with the arpdau data
  # if startday is given, function will recalculate state probabilities starting from startday
  
  if(isTRUE(is.na(state_probs))){
    
    if(!is.na(startday)){
      # rebuild state probs from startday using transition data
      
      state_probs <- getStateProbs(install_transition, transition_data, startday=startday, max_cohortday=max_cohortday, state_probs=state_probs)
      
    }else{
      # rebuild state probs entirely from transition data
      
      state_probs <- getStateProbs(install_transition, transition_data, max_cohortday=max_cohortday)  
      
    }
    
  }

  result <- state_probs %>%
    left_join(arpdaudata, by=c('cohortday', 'state')) %>%
    mutate(
      revenue = prob * arpdau
    )

  result_summary <- result %>%
    group_by(cohortday) %>%
    summarise(
      retention = sum(ifelse(as.character(as.numeric(state)) >= 4, prob, 0)),
      arpi = sum(revenue)
    ) %>%
    mutate(
      cumarpi = cumsum(arpi)
    )

  ret_value <- list(result=result, result_summary=result_summary, cumarpi_max=filter(result_summary, cohortday == max_cohortday) %>% pull(cumarpi))
  return(ret_value)
}

buildModel <- function(mydata, group_variables=NULL, max_cohortday=NA){
  # build markov transition rate model

  if(is.na(max_cohortday)){
    max_cohortday <- max(mydata$cohortday)
  }

  # remove days with no data on next state
  transitiondata <- filter(mydata, date != max(date))


  install_transition <- transitiondata %>%
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

  checksum <- transition_matrix %>%
    group_by(cohortday, state) %>%
    summarise(
      prob = sum(trans_prob)
    )

  if(!isTRUE(all.equal(checksum$prob, rep(1, nrow(checksum))))){
    stop('Checksum does not sum to 1')
  }

  arpdaudata <- mydata %>%
    group_by(!!! group_variables, cohortday, state) %>%
    summarise(
      arpdau = sum(revenue) / sum(n)
    )
  
  transitionMatrices <- getTransitionMatrices(install_transition, transition_matrix, max_cohortday)
  
  result = tryCatch({
    state_probs <- getStateProbs(install_transition, transitionMatrices, max_cohortday)
    simulation_results <- run_simulation(state_probs=state_probs, arpdaudata=arpdaudata, max_cohortday=max_cohortday)  
  }, warning = function(w) {
    
  }, error = function(e) {
    state_probs <- NULL
    simulation_results <- NULL
    warning('Error when calculating state probs and simulation results')
  }, finally = {
    
  })
  
  result <- simulation_results$result
  result_summary <- simulation_results$result_summary

  return(list(max_cohortday=max_cohortday, transitiondata=transitiondata, install_transition=install_transition,
              transition_matrix=transition_matrix, arpdaudata=arpdaudata, transitionMatrices=transitionMatrices,
              state_probs=state_probs, simulation_results=simulation_results, result=result,
              result_summary=result_summary))
}

getHistoricalVariance <- function(transitiondata, mydata, group_variables=NULL, max_cohortday=NA){
  # Get historical variances

  if(is.na(max_cohortday)){
    max_cohortday <- max(mydata$cohortday)
  }

  if(length(group_variables) > 0){
    transitions_cross <- distinct(transitiondata, !!! group_variables)
    transitions_cross <- merge(transitions_cross, data.frame(install_date = unique(transitiondata$install_date)))
  }else{
    transitions_cross <- data.frame(install_date = unique(transitiondata$install_date))
  }

  transitions_cross <- merge(transitions_cross, data.frame(cohortday = unique(transitiondata$cohortday))) %>% filter(cohortday < max_cohortday) %>% mutate(date = install_date + cohortday)
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

  # statedata <- filter(all_transitions, state == 4, next_state == 4, cohortday == 0)
  # hist(statedata$trans_prob)

  # ggplot(statedata, aes(trans_prob)) +
  #   geom_density()

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
    filter(users > 0) %>%
    ungroup()


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
    filter(state %in% c('5', '6', '7')) %>%
    ungroup()

  return(list(hist_transitions=hist_transitions, hist_arpdau=hist_arpdau))
}
