# By: James Tan

# Date: 9/18/2018

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
    vector <- ifelse(index(vector) == change_index, values, vector * prob_change / trans_prob_orig)

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

simulate_markov <- function(install_transition=NA, transition_matrix=NA, state_probs=NA, rate_type='transition', c, change_states, s=NA, affected_states=NA, p_new=NA, type=2, weights=NA){

  # rate_type: whether to change transition rates or state probabilities on a given cohortday, 'transition' for transition rates, 'state' for state probabilities
  # c: cohortday
  # change_states: states to change (could either be install state or next states)
  # s: state to change transitions from
  # affected_states: states to change to accomodate change in trans_prob, but not changed directly
  # p_new: new probability to set
  # type: type of perturbation

  # types of perturbations
  # 1. specify all transition probabilities for a given state to the next states
  # 2. scale every other transition probability down so that sum is 1
  # 3: change only the scale from the states specified by the cs (change states) parameter - so if s=4, ns=7, and cs=c(4, 5, 6), then increasing p_new 4 to 7 will decrease evenly probability from 4 to 4, 5, and 6. This can be used to keep retention the same but increase conversion.
  # 4: change only the scale from the states specified by the cs (change states) parameter and by the weight to each state in the change states - so if s=4, ns=7, and cs=c(4, 5, 6), and weights=(.6, .3, .1) then increasing p_new 4 to 7 will decrease probability from 4 to 4, 5, and 6 weighted by weights.
  # 5. change only the scale from the transition's opposite LTV group - so increasing a transition to 4 decreases the transition to 0 by the same amount, (1, 5), (2, 6), and (3, 7) are the other pairs

  # if(!is.numeric(s)){
  #   s <- as.numeric(as.character(s))
  # }

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

      matrix_change <- filter(transition_matrix, cohortday == c, state == s) %>% arrange(next_state)
      match_var <- quo(next_state)
      prob_var <- quo(trans_prob)

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

  return(list(install_transition=install_transition, transition_matrix=transition_matrix, state_probs=state_probs))
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

getStateProbs <- function(install_transition, transition_matrix, max_cohortday=270){
  # get cohortday state probabilities from the transition matrix

  initialState <- matrix(1)
  state_probs <- tibble(cohortday=0, state=install_transition$state, prob=install_transition$trans_prob)

  transitionMatrices <- list()
  transitionMatrices[['install']] <- matrix(install_transition$trans_prob, nrow=1, dimnames=list('prob', install_transition$state))

  temp_state <- transitionMatrices[['install']]

  for(i in 0:(max_cohortday - 1)){
    # create list of transition matrices
    cohortday_transition <- filter(transition_matrix, cohortday == i)
    cohortday_matrix <- dcast(cohortday_transition, state ~ next_state, mean, value.var='trans_prob', fill=0, drop=TRUE)
    rownames(cohortday_matrix) <- cohortday_matrix$state
    cohortday_matrix <- as.matrix(select(cohortday_matrix, -state))
    transitionMatrices[[as.character(i)]] <- cohortday_matrix

    temp_state <- temp_state %*% cohortday_matrix
    df_state <- tibble(cohortday=(i+1), state=colnames(temp_state), prob=temp_state[1,])
    state_probs <- rbind(state_probs, df_state)
  }

  return(state_probs)

}

run_simulation <- function(state_probs=NA, install_transition=NA, transition_matrix=NA, arpdaudata, max_cohortday=270){
  # run simulation given either the state probability matrix OR transition matrices then combined with the arpdau data

  if(isTRUE(is.na(state_probs))){
    state_probs <- getStateProbs(install_transition, transition_matrix, max_cohortday=max_cohortday)
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
