# By: James Tan

# Date: 4/11/2018

library(dplyr)
library(readr)
library(ggplot2)

source('~/.Rprofile')
setwd(datapath)

is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}


mydata <- read.csv('arprpdau.csv', stringsAsFactors = FALSE)
sapply(mydata, class)

mydata <- rename(mydata, current_isogroup = current_isogroup..group.)
mydata$current_isogroup <- as.factor(mydata$current_isogroup)
mydata$purchase_days_last_bin <- as.factor(mydata$purchase_days_last_bin)
mydata$date <- as.Date(mydata$date, format='%m/%d/%y', tz = 'UTC')
mydata$revenue <- parse_number(mydata$revenue)


d1comp <- mydata %>%
  group_by(current_isogroup, date) %>%
  mutate(rpdau_percent = rpdau / sum(rpdau)) %>%
  filter(date == '2018-1-1') %>%
  ungroup %>%
  select(current_isogroup, purchase_days_last_bin, rpdau_percent)

mydata <- mydata %>%
  mutate(arprpdau = revenue / rpdau)

mydata <- merge(mydata, d1comp, by=c('current_isogroup', 'purchase_days_last_bin'))

arprpdau <- mydata %>%
  group_by(current_isogroup, date) %>%
  summarise(
    arprpdau = weighted.mean(arprpdau, rpdau_percent)
  )

ggplot(arprpdau, aes(date, arprpdau, group=current_isogroup, color=current_isogroup)) + 
  geom_line()

ggplot(mydata, aes(date, arprpdau, color=current_isogroup)) +
  geom_line()
