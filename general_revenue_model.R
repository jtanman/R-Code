# By: James Tan

# Date: 8/13/2018

library(dplyr)
library(ggplot2)
library(reshape2)

source('~/.Rprofile')
setwd(datapath)

mydata <- readRDS('revenue_modeling_cohortday_all_dates.RData')