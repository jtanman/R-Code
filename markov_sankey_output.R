# By: James Tan

# Date: 6/13/2019

p_load(reshape2, beepr, scales, zoo, tidyverse, RColorBrewer, viridis)

source('~/.Rprofile')
setwd(datapath)

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

levels(install_transition$state) <- STATES
levels(transition_matrix$state) <- STATES
levels(transition_matrix$next_state) <- STATES
levels(state_probs$state) <- STATES
output <- ''

for(i in 1:nrow(install_transition)){
  line <- sprintf('install [%f] D0 %s', install_transition$trans_prob[i], install_transition$state[i])
  print(line)
  output <- str_c(output, line, '\n')
}

tp <- transition_matrix$trans_prob
c <- transition_matrix$cohortday
s <- transition_matrix$state
ns <- transition_matrix$next_state

for(i in 1:max(which(cohortday <= 3))){
  
  flow <-  tp[i] * filter(state_probs, state == s[i], cohortday == c[i]) %>% pull(prob)
  if(!isTRUE(all.equal(flow, 0))){
    line <- sprintf('D%d %s [%f] D%d %s', c[i], s[i], flow, c[i] + 1, ns[i])
    print(str_c(i,' ', line))
    output <- str_c(output, line, '\n')
  }
}

cat(output)
