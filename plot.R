source('~/.Rprofile')
setwd(datapath)

is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}


mydata <- read.csv('arprpdau')
sapply(mydata, class)

mydata$current_isogroup <- as.factor(mydata$current_isogroup)
mydata$date <- as.Date(mydata$date, tz = 'UTC')
mydata <- rename(mydata, arprpdau = X.column.)
mydata$arprpdau <- as.numeric(as.character(mydata$arprpdau))
mydata$arprpdau[!is.finite(mydata$arprpdau)] <- NA

ggplot(mydata, aes(date, arprpdau, color=current_isogroup)) +
  geom_line()
