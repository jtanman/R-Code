
testdata <- mydata %>%
  filter(install_date == as.Date("2017-6-28"), (cohortday == 0 | cohortday == 1)) %>%
  group_by(cohortday, state, next_state) %>%
  summarise(
    n = sum(n)
  )
%>%
  left_join(state_probs, by=c('cohortday', 'state'))



getUserState <- function(tms, cohortday){
  if(cohortday == 'install' | cohortday == 0){
    return(tms$install)
  }else{
    state <- tms$install
    for(i in 0:(cohortday - 1)){
      state <- state %*% tms[[as.character(i)]]
    }
    return(state)
  }
}

users <- transition_matrix %>%
  filter(cohortday == 0) %>%
  group_by(next_state) %>%
  summarise(
    users = sum(users)
  ) %>%
  mutate(
    prob = users / sum(users)
  )



actual_state_probs <- mydata %>%
  # filter(install_date == as.Date("2017-9-30")) %>%
  group_by(install_date, cohortday, state) %>%
  summarise(
    n = sum(n)
  ) %>%
 arrange(install_date, cohortday, state) %>%
  filter(cohortday != 0)

next_state_probs <- mydata %>%
  # filter(install_date == as.Date("2017-9-30")) %>%
  filter(!is.na(next_state)) %>%
  group_by(install_date, cohortday, next_state) %>%
  summarise(
    n_next = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    cohortday = cohortday + 1
  ) %>%
  rename(state = next_state)

actual_probs <- left_join(actual_state_probs, next_state_probs, by=c('install_date', 'cohortday', 'state')) %>%
  filter(is.na(n_next)) %>%
  # group_by(cohortday, state) %>%
  # summarise(
  #   n = sum(n),
  #   n_next = sum(n_next, na.rm=TRUE)
  # )
  arrange(install_date)


testdata <- filter(mydata, install_date == as.Date('2017-7-31')) %>%
  filter(cohortday == 0 | cohortday == 1) %>%
  group_by(cohortday, state, next_state) %>%
  summarise(n = sum(n))

filter(mydata, install_date == as.Date('2017-7-31')) %>%
  filter(cohortday == 0)


filter(mydata, install_date == as.Date('2017-7-31'))

