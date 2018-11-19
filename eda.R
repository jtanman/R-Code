# By: James Tan

# Date: 8/13/2018

library(dplyr)
library(ggplot2)
library(reshape2)

source('~/.Rprofile')
setwd(datapath)

mydata <- read.csv('20180806_bb_niso_install_cohort_user_list_v2.csv')
summary(mydata)

mydata <- mydata %>%
  mutate(
    user_id = factor(user_id)
  )
