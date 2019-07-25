setwd(datapath)

library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)

read_data <- read.delim('input000.txt', sep='\t', header=FALSE)
mydata <- read_data %>%
  rename(
    date = V1,
    temp = V2
  ) %>%
  mutate(
    date = as.Date(date, '%m/%d/%Y %H:%M:%S', tz=""),
    temp = as.double(as.character(temp)),
  )

mydata <- mydata %>%
  mutate(
    temp_avg = rollapply(temp, width=5, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"),
    temp_avg1 = na.spline(temp, xout=date),
    temp_avg2 = na.approx(temp),
    temp_avg = ifelse(temp_avg >= 400, 400, temp_avg)
  )

missing_dates <- mydata %>% filter(is.na(temp)) %>% pull(date)
dates <- seq(min(mydata$date), max(mydata$date), 1)
alldates <- data.frame(date = dates) %>%
  left_join(mydata, by='date') %>%
  mutate(
    temp_spline = na.spline(temp)
  )

temps <- zoo(alldates$temp)

values <- alldates %>%
  filter(date %in% missing_dates) %>%
  pull(temp_spline)

na.spline(mydata$temp) == na.spline(mydata$temp, xout=mydata$date)

ggplot(mydata, aes(time, temp_avg1)) +
  geom_point() +
  geom_line(aes(y=temp_avg), alpha=.5, color='red')

# 
# na.spline(mydata$temp)
# 
# fit<-auto.arima(pull(mydata, temp), seasonal=TRUE)
# missing <- which(is.na(mydata$temp))
# for(i in missing){
#   subset <- filter(mydata, row_number() < i) %>% pull(temp)
#   predict(fit, n.ahead=1, newxreg=subset)
#   
# }


ggplot(data, aes(time, temp)) +
  geom_line() +
  geom_line(aes(y=temp_avg), alpha=.5, color='red')

