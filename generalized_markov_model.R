# By: James Tan

# Date: 9/12/2018

library(ggplot2)
library(reshape2)
library(plotly)
library(lubridate)
library(markovchain)
library(dplyr)
library(zoo)
library(expm)

source('~/.Rprofile')
source('~/MZ/R Code/generalized_markov_model_helper.R')
setwd(datapath)

mydata <- read.csv('generalized_model_user_cohortdays_saved.csv')

filter(mydata,
  !(rev_lifetime_revenue == dau_lifetime_revenue),
  !(is.na(rev_lifetime_revenue) & is.na(dau_lifetime_revenue))
)

filter(mydata,
   (is.na(rev_lifetime_revenue) & !is.na(dau_lifetime_revenue)),
)


mydata <- mydata %>%
  mutate(
    user_id = as.factor(user_id),
    install_event_date = as.Date(install_event_date),
    date = as.Date(date),
    install_platform = as.factor(ifelse(install_platform == 1, 'iOS', ifelse(install_platform == 2, 'android', 'Other'))),
    dup_account = as.factor(ifelse(dup_account == 1, 'dup', 'non-dup')),
    session = ifelse(is.na(dau_lifetime_revenue), 0, 1),
    rev = ifelse(is.na(rev), 0, rev),
    purchase_count = ifelse(is.na(purchase_count), 0, purchase_count)
  )

mydata <- mydata %>%
  group_by(user_id) %>%
  arrange(user_id, cohortday) %>%
  mutate(
    dau_lifetime_revenue = ifelse(cohortday == 0 & is.na(dau_lifetime_revenue), 0, dau_lifetime_revenue),
    lifetime_rev = na.locf(dau_lifetime_revenue, na.rm = FALSE),
    rev_state = case_when(
      lifetime_rev == 0 ~ 0,
      lifetime_rev >= 5 & lifetime_rev < 20 ~ 1,
      lifetime_rev >= 20 & lifetime_rev < 100 ~ 2,
      lifetime_rev >= 100 ~ 3,
      TRUE ~ NA_real_
    ),
    state = cohortday * 8 + session * 4 + rev_state
  ) %>%
  select(
    user_id, install_event_date, date, cohortday, campaign_type, install_platform, dup_account, session, purchase_count, rev, lifetime_rev, state
  ) %>%
  ungroup()

saveRDS(mydata, 'markovdata_parsed.RData')
mydata <- readRDS('markovdata_parsed.RData')

ndays <- 7

arpdaudata <- mydata %>%
  group_by(state) %>%
  summarise(
    arpdau = mean(rev)
  )

filterdata <- mydata %>% filter(cohortday <= ndays)

markovdata <- select(filterdata, user_id, cohortday, state)
states <- as.character(c(-1, sort(unique(markovdata$state))))
markovdata <- dcast(markovdata, user_id ~ cohortday, value.var='state')

# randomly sample 100 rows
markovdata <- sample_n(markovdata, 100, replace=FALSE)

markovdata$install <- -1
markovdata <- markovdata %>% select(install, everything()) %>% select(-user_id)

myfit <- markovchainFit(data=markovdata, name='Generalized Markov Model')
saveRDS(myfit, 'markovchainfit.RData')

mychain <- myfit$estimate
slot(mychain, 'transitionMatrix') <- mychain[states, states]
slot(mychain, 'states') <- states
tm <- slot(mychain, 'transitionMatrix')

cm <- c(.05, .94, .02)


x <- getCohortdayMatrix(tm, 'install')
colnames(x) <- sapply(colnames(x), getStateName)
rownames(x) <- sapply(rownames(x), getStateName)

cm <- c(.05, .94, .01)


tm <- putCohortdayMatrix(tm, 'install', cm)

initialState <- c(1, rep(0, length(states) - 1))
initialState <- matrix(initialState, nrow=1)
# initialState %*% (tm %^% 3)

arpi <- rep(NA, ndays)
retention <- rep(NA, ndays)

result <- tibble(cohortday=NA, state=NA, prob=NA, arpdau=NA, rev=NA)
# result <- as_tibble(matrix(nrow=0, ncol=4))
# colnames(result) <- c('cohortday', 'state', 'prob', 'arpdau')
currentstate <- initialState
for(i in 1:ndays){
  currentstate <- currentstate %*% tm
  df_state <- tibble(state = as.numeric(colnames(currentstate)), prob = currentstate[1,]) %>%
    left_join(arpdaudata, by='state') %>%
    filter(floor(state / 8) == i - 1) %>%
    mutate(
      cohortday = i - 1,
      state = state %% 8,
      rev = prob * arpdau
    )
   result <- rbind(result, df_state)
}

result <- slice(result, -1)

result_summary <- result %>%
  group_by(cohortday) %>%
  summarise(
    retention = sum(ifelse(state >= 4, prob, 0)),
    arpi = sum(rev)
  ) %>%
  mutate(
    cumarpi = cumsum(arpi)
  )


actual_results <- filterdata %>%
  group_by(cohortday) %>%
  summarise(
    retention = sum(session) / n(),
    arpi = sum(rev) / n()
  ) %>%
  mutate(
    cumarpi = cumsum(arpi)
  )
