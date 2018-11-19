# By: James Tan

# Date: 5/31/2018

library(dplyr)
library(lubridate)
library(zoo)
library(vtreat)
library(gbm)
library(plotly)
library(ggplot2)
library(reshape2)
library(glmnet)
library(timeDate);library(chron)
library(plotmo)

last_plot <- ggplot2::last_plot

source('~/.Rprofile')
setwd(datapath)

hlist <- c('USNewYearsDay', 'USPresidentsDay', 'USMLKingsBirthday', 'USLincolnsBirthday', 'USWashingtonsBirthday', 'USMemorialDay', 'USIndependenceDay', 'USLaborDay', 'USColumbusDay', 'USElectionDay', 'USVeteransDay', 'USThanksgivingDay', 'USChristmasDay', 'USGoodFriday')
myholidays  <- dates(as.character(holiday(2018,hlist)),format="Y-M-D")
names(myholidays) <- hlist
myholidays <- sort(myholidays)

hlist <- c('USNewYearsDay', 'USPresidentsDay', 'USMLKingsBirthday', 'USMemorialDay', 'USIndependenceDay', 'USLaborDay', 'USColumbusDay', 'USElectionDay', 'USVeteransDay', 'USThanksgivingDay', 'USChristmasDay')
premierholidays  <- dates(as.character(holiday(2018,hlist)),format="Y-M-D")
names(premierholidays) <- hlist
premierholidays <- sort(premierholidays)


biweeklypayday <- chron(format(seq(as.Date('2018-1-12'), as.Date('2018-12-28'), by='2 weeks'), '%m/%d/%Y'))
semimonthlypayday <- dates(format(as.Date(c('2018-1-15', '2018-1-31', '2018-2-15', '2018-2-28', '2018-3-15', '2018-3-30', '2018-4-13', '2018-4-30', '2018-5-15', '2018-5-31', '2018-6-15', '2018-6-29', '2018-7-13', '2018-7-31', '2018-8-15', '2018-8-31', '2018-9-14', '2018-9-28', '2018-10-15', '2018-10-31', '2018-11-15', '2018-11-30', '2018-12-14', '2018-12-31')), '%m/%d/%Y'))


# mydata <- readRDS('jan_may_purchases.RData')
#
# sapply(mydata, class)
# mydata <- mydata %>%
#   mutate(
#     game_id = as.factor(game_id),
#     user_id = as.factor(user_id),
#     first_login_ts = as.POSIXct(first_login_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC'),
#     purchase_ts = as.POSIXct(purchase_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC'),
#     purchase_ts_PDT = as.POSIXct(format(purchase_ts, tz="America/Los_Angeles",usetz=TRUE), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles'),
#     purchase_min = floor_date(purchase_ts, 'min'),
#     purchase_min_PDT = floor_date(purchase_ts_PDT, 'min'),
#     purchase_date = as.Date(purchase_date),
#     first_purchase_ts = as.POSIXct(first_purchase_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC'),
#     prev_purchase_ts = as.POSIXct(prev_purchase_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC'),
#     kingdom = as.factor(kingdom),
#     dup_account = as.factor(dup_account),
#     weekday = factor(weekdays(purchase_ts), levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
#     holiday = is.holiday(purchase_ts, myholidays),
#     holiday_premier = is.holiday(purchase_date, premierholidays),
#     biweekly = is.holiday(purchase_ts, biweeklypayday),
#     semimonthly = is.holiday(purchase_ts, semimonthlypayday),
#     day_of_month = as.factor(strftime(purchase_ts, '%d')),
#     time = strftime(purchase_ts, '%H:%M'),
#     time_PDT = strftime(purchase_ts_PDT, '%H:%M', tz='America/Los_Angeles'),
#     minute = as.numeric(strftime(purchase_ts, '%M')),
#     hour = as.numeric(strftime(purchase_ts, '%H')),
#     flash_sale = (minute >= 1 & minute <= 14),
#     flash_sale_15 = minute == 15
#   )
#
#
#
#
# saveRDS(mydata, 'jan_may_purchases_parsed.RData')
mydata <- readRDS('jan_may_purchases_parsed.RData')

minutes <- head(strftime(seq(as.POSIXct('2018-01-01', format='%Y-%m-%d'), as.POSIXct('2018-01-2', format='%Y-%m-%d'), by='min'), '%H:%M'), -1)
weekdays <- as.character(sort(unique(mydata$weekday)))

weekday_time_df <- merge(minutes, weekdays) %>% rename(time = x, weekday = y) %>% mutate(weekday_time = paste(weekday, time))
weekday_time <- pull(weekday_time_df, weekday_time)
weekday_time <- factor(weekday_time, levels=c(weekday_time))
#
# head(mydata, 100) %>%
#   mutate(
#     weekday = weekdays(purchase_ts),
#     holiday = is.holiday(purchase_ts),
#     biweekly = is.holiday(purchase_ts, biweeklypayday),
#     semimonthly = is.holiday(purchase_ts, semimonthlypayday),
#     day_of_month = as.numeric(strftime(purchase_ts, '%d')),
#     time = strftime(purchase_ts, '%H:%M'),
#     purchase_min = floor_date(purchase_ts_PDT, 'min')
#     minute = as.numeric(strftime(purchase_ts, '%M')),
#     flash_sale = (minute >= 1 & minute <= 14),
#   )

dayofweek <- mydata %>%
  group_by(weekday, time) %>%
  summarise(
    instances = length(unique(purchase_min)),
    purchases = n(),
    purchases_per_min = purchases / instances
  ) %>%
  ungroup() %>%
  mutate(
    weekday_time = factor(paste(weekday, time), levels=weekday_time)
  )

dayofweek_PDT <- mydata %>%
  group_by(weekday, time_PDT) %>%
  summarise(
    instances = length(unique(purchase_min_PDT)),
    purchases = n(),
    purchases_per_min = purchases / instances
  ) %>%
  ungroup() %>%
  mutate(
    weekday_time = factor(paste(weekday, time_PDT), levels=weekday_time)
  )

plotname <- 'Purchases by Day of Week and Time (PDT)'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(weekday_time), length.out = 57)+1
labels <- weekday_time[breaks]
labels

ggplot(dayofweek_PDT, aes(weekday_time, purchases_per_min, fill=weekday)) +
  geom_col() +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname) +
  theme(axis.text.x = element_text(angle=75, vjust=.7))

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')


plotname <- 'Purchases by Day of Week (PDT)'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(minutes), length.out = 9)+1
labels <- minutes[breaks]
labels

ggplot(dayofweek_PDT, aes(time_PDT, purchases_per_min)) +
  geom_col() +
  facet_wrap(~ weekday) +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname) +
  theme(axis.text.x = element_text(angle=75, vjust=.7))

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# DAY OF MONTH

dayofmonth <- mydata %>%
  group_by(day_of_month) %>%
  summarise(
    days = length(unique(purchase_date)),
    purchases = n(),
    purchases_per_day = purchases / days
  )

plotname <- 'Purchases by Day of Month'
filename <-paste0(plotname, '.png')

ggplot(dayofmonth, aes(day_of_month, purchases_per_day)) +
  geom_col() +
  scale_x_continuous(breaks=seq(1, 31, 1)) +
  labs(title=plotname)

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# Holiday

holidaydata <- mydata %>%
  group_by(holiday, time_PDT) %>%
  summarise(
    days = length(unique(purchase_date)),
    purchases = n(),
    purchases_per_min = purchases / days,
  )

plotname <- 'Purchases by Holiday'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(minutes), length.out = 9)+1
labels <- minutes[breaks]
labels

ggplot(holidaydata, aes(time_PDT, purchases_per_min)) +
  geom_col() +
  facet_wrap(~ holiday) +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname, caption="New Years, MLK Bday, Lincoln's Bday, Presidents Day, Washington's Bday, Good Friday, Memorial Day")

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

holidaydata <- mydata %>%
  group_by(holiday_premier, time_PDT) %>%
  summarise(
    days = length(unique(purchase_date)),
    purchases = n(),
    purchases_per_min = purchases / days,
  )

plotname <- 'Purchases by Premium Holidays'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(minutes), length.out = 9)+1
labels <- minutes[breaks]
labels

ggplot(holidaydata, aes(time_PDT, purchases_per_min)) +
  geom_col() +
  facet_wrap(~ holiday_premier) +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname, caption='New Years, MLK Bday, Presidents Day, Memorial Day')

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# Payday

paydata <- mydata %>%
  group_by(biweekly, time_PDT) %>%
  summarise(
    days = length(unique(purchase_date)),
    purchases = n(),
    purchases_per_min = purchases / days,
  )

plotname <- 'Purchases by Biweekly Payday'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(minutes), length.out = 9)+1
labels <- minutes[breaks]
labels

ggplot(paydata, aes(time_PDT, purchases_per_min)) +
  geom_col() +
  facet_wrap(~ biweekly) +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname, caption='Biweekly Payday')

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

paydata <- mydata %>%
  group_by(semimonthly, time_PDT) %>%
  summarise(
    days = length(unique(purchase_date)),
    purchases = n(),
    purchases_per_min = purchases / days,
  )

plotname <- 'Purchases by Semimonthly Payday'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(minutes), length.out = 9)+1
labels <- minutes[breaks]
labels

ggplot(paydata, aes(time_PDT, purchases_per_min)) +
  geom_col() +
  facet_wrap(~ semimonthly) +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname, caption='Semimonthly Payday')

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')


# Modeling CCP

minutedata <- mydata %>%
  group_by(purchase_min, weekday, holiday, holiday_premier, biweekly, semimonthly, day_of_month, time, minute, hour, flash_sale, flash_sale_15) %>%
  summarize(
    purchases = n(),
    rev = sum(revenue)
  ) %>%
  ungroup()

allminutes <- data.frame(purchase_min = seq(min(mydata$purchase_min), max(mydata$purchase_min), by='min'))
allminutes <- filter(allminutes, as.Date(purchase_min) != as.Date('2018-1-17') & as.Date(purchase_min) != as.Date('2018-1-18'))
# mydata <- merge(allminutes, mydata, by='purchase_min', all.x=TRUE)
allminutes <- allminutes %>%
  mutate(
    purchase_date = as.Date(purchase_min),
    weekday = factor(weekdays(purchase_min), levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
    holiday = is.holiday(purchase_date, myholidays),
    holiday_premier = is.holiday(purchase_date, premierholidays),
    biweekly = is.holiday(purchase_min, biweeklypayday),
    semimonthly = is.holiday(purchase_min, semimonthlypayday),
    day_of_month = as.numeric(strftime(purchase_min, '%d')),
    time = strftime(purchase_min, '%H:%M'),
    minute = as.numeric(strftime(purchase_min, '%M')),
    hour = as.numeric(strftime(purchase_min, '%H')),
    flash_sale = (minute >= 1 & minute <= 14),
    flash_sale_15 = minute == 15
  )

minutedata <- allminutes %>%
  left_join(minutedata, by=c('purchase_min', 'weekday', 'holiday', 'holiday_premier', 'biweekly', 'semimonthly', 'day_of_month', 'time', 'minute', 'hour', 'flash_sale', 'flash_sale_15')) %>%
  mutate(
    purchases = ifelse(is.na(purchases), 0, purchases),
    rev = ifelse(is.na(rev), 0, rev)
  )



modeldata <- select(minutedata,
    weekday, holiday, holiday_premier, biweekly, semimonthly, day_of_month, hour, minute, purchases, rev
    )
pred_vars <- c('purchases', 'rev')
pred_var <- 'purchases'

ind_vars <- names(modeldata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_vars))

var_names <- paste(ind_vars, collapse=' + ')

data_name <- 'purchases_minutedata_hour_minute'

# imputedata <- mydata[,!(colnames(mydata) %in% c('trophy'))]
# aggr_plot <- aggr(imputedata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata),
#   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# installdata <- ddply(mydata, .(install_date), summarise,
#                      d3churn = mean(d3churn)
# )

model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))

# Partition into training, validation, testing set

# spec = c(treat = .2, train = .7, test = .09, validate = 0.01)
# spec = c(train = .9, test = .09, validate = .01)
spec = c(treat = .4, train = .6)

g = sample(cut(
  seq(nrow(modeldata)),
  nrow(modeldata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(modeldata, g)

treatdata <- res$treat
traindata <- res$train
validdata <- res$validate
testdata <- res$test

saveData(res, name='tow_d2ret_day0_95_parsed_labeleddata_v1', overwrite=TRUE)
saveData(traindata, name='tow_d2ret_day0_95_parsed_traindata_v1', overwrite=TRUE)
saveData(validdata, name='tow_d2ret_day0_95_parsed_validdata_v1', overwrite=TRUE)
saveData(testdata, name='tow_d2ret_day0_95_parsed_testdata_v1', overwrite=TRUE)

res <- loadData(name='tow_d2ret_day0_95_parsed_labeleddata_v1')
traindata <- loadData(name='tow_d2ret_day0_95_parsed_traindata_v1')
validdata <- loadData(name='tow_d2ret_day0_95_parsed_validdata_v1')
testdata <- loadData(name='tow_d2ret_day0_95_parsed_testdata_v1')


# TREAT DATA

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameNExperiment(traindata, varlist = ind_vars, outcomename = pred_var, outcometarget = 1,
                               scale=TRUE,
                               rareCount=5,  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)
treatment_plan_factor <- cfe$treatments
sf <- treatment_plan_factor$scoreFrame
var_names <- sf$varName[sf$sig <= 1/nrow(sf)]
train.treat <- cfe$crossFrame

sf %>% arrange(sig)

PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame))
# use separate data to treat and train

treatment_plan_factor <- designTreatmentsN(treatdata, varlist = ind_vars, outcomename = pred_var,
                                        rareCount=5,  # Note set this to something larger, like 5
                                        rareSig=.3 # Note set this to something like 0.3
)

treatment_name <- paste0(data_name, '_treatment_plan.RData')
saveRDS(treatment_plan_factor, treatment_name)
treatment_plan_factor <- readRDS(treatment_name)

train.treat <- prepare(treatment_plan_factor, traindata, scale=TRUE, pruneSig=.05)
var_names <- setdiff(colnames(train.treat), c(pred_vars))

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))

#
# # treat validation and testing set
#
# valid.treat <- vtreat::prepare(treatment_plan_factor, validdata, scale=TRUE, pruneSig=PRUNE_SIG)
# test.treat <- vtreat::prepare(treatment_plan_factor, testdata, scale=TRUE, pruneSig=PRUNE_SIG)
# train.treat[,pred_var_num] <- traindata[,pred_var_num]
# valid.treat[,pred_var_num] <- validdata[,pred_var_num]
# test.treat[,pred_var_num] <- testdata[,pred_var_num]



# GBM

model_name <- paste(data_name, 'gbm', 'v1.RData', sep='_')

n.trees = 4000
gbm1 <- gbm(factor_formula, data=train.treat, distribution='gaussian', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
best.iter <- gbm.perf(gbm1, plot.it=FALSE)
if(gbm1$n.trees < best.iter*1.2){
  gbm1 <- gbm.more(gbm1, n.new.trees=(best.iter*1.2 - gbm1$n.trees))
  best.iter <- gbm.perf(gbm1, plot.it=FALSE)
}
# gbm1 <- gbm.more(gbm1, n.new.trees=1000)
saveRDS(gbm1, file=model_name)
gbm1 <- readRDS(model_name)

gbm.perf(gbm1)
best.iter <- gbm.perf(gbm1, method='test', plot.it=TRUE)
best.iter
par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance', plotit=TRUE)
var_imp <- subset(var_imp, rel.inf != 0)
var_imp <- var_imp %>% arrange(desc(rel.inf))
var_imp$var <- factor(var_imp$var, levels=rev(var_imp$var))
ggplot(var_imp, aes(var, rel.inf)) +
  geom_bar(stat='identity', fill='blue') +
  coord_flip() +
  ggtitle('GBM Variable Importance') +
  labs(x ='Variable', y = 'Relative Influence') +
  theme(plot.title = element_text(hjust=.5))

ggplot2::ggsave(paste0(model_name, '.png'),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# new data

newminutes <- data.frame(purchase_min = seq(as.POSIXct('2018-06-01 00:00:00', tz='UTC'), as.POSIXct('2018-12-31 23:59:00'), by='min'))
newminutes <- newminutes %>%
  mutate(
    purchase_min_PDT = as.POSIXct(format(purchase_min, tz="America/Los_Angeles",usetz=TRUE), format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles'),
    purchase_date = as.Date(purchase_min),
    weekday = factor(weekdays(purchase_min), levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
    holiday = is.holiday(purchase_date, myholidays),
    holiday_premier = is.holiday(purchase_date, premierholidays),
    biweekly = is.holiday(purchase_min, biweeklypayday),
    semimonthly = is.holiday(purchase_min, semimonthlypayday),
    day_of_month = as.numeric(strftime(purchase_min, '%d')),
    time = strftime(purchase_min, '%H:%M'),
    minute = as.numeric(strftime(purchase_min, '%M')),
    hour = as.numeric(strftime(purchase_min, '%H')),
    hour_PDT = as.numeric(strftime(purchase_min, '%H', tz='America/Los_Angeles')),
    flash_sale = (minute >= 1 & minute <= 14),
    flash_sale_15 = minute == 15
  )

newminutes.treat <- prepare(treatment_plan_factor, newminutes, scale=TRUE, pruneSig=.05)

gbm_predict <- predict(gbm1, newminutes.treat, n.trees=best.iter, type='response')
newminutes$predict <- gbm_predict

plotdata <- filter(newminutes, purchase_min >= as.POSIXct('2018-06-04 00:00:00', tz='UTC')  & purchase_min < as.POSIXct('2018-06-11 00:00:00', tz='UTC'))
ggplot(plotdata, aes(purchase_min, predict, fill=weekday)) +
  geom_col()


hourdata <- newminutes %>%
  # group_by(hour) %>%
  group_by(hour_PDT) %>%
  summarise(
    predicted_sales = mean(predict)
  )

# plotname <- 'Average Hourly Prediction (UTC)'
plotname <- 'Average Hourly Prediction (PDT)'
filename <-paste0(plotname, '.png')

# ggplot(hourdata, aes(hour, predicted_sales)) +
ggplot(hourdata, aes(hour_PDT, predicted_sales)) +
  geom_col() +
  scale_x_continuous(breaks=seq(0,23,1)) +
  labs(title=plotname)

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

weekdata <- newminutes %>%
  group_by(weekday, time) %>%
  summarise(
    predicted_sales = mean(predict)
  ) %>% mutate(
    weekday_time = factor(paste(weekday, time), levels=weekday_time),
  )

plotname <- 'Average Week Prediction (UTC)'
filename <-paste0(plotname, '.png')

breaks <- seq(0, length(weekday_time), length.out = 57)+1
labels <- weekday_time[breaks]
labels

ggplot(weekdata, aes(weekday_time, predicted_sales, fill=weekday)) +
  geom_col() +
  scale_x_discrete(breaks=labels) +
  labs(title=plotname) +
  theme(axis.text.x = element_text(angle=75, vjust=.7))

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

monthdata  <- newminutes %>%
  group_by(day_of_month) %>%
  summarise(
    predicted_sales = mean(predict)
  )

plotname <- 'Average Day of Month Prediction'
filename <-paste0(plotname, '.png')

ggplot(monthdata, aes(day_of_month, predicted_sales)) +
  geom_col() +
  labs(title=plotname)# +
  # theme(axis.text.x = element_text(angle=75, vjust=.7))

ggsave(filename,
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')


# Prediction graphs

dayofweekdata <- newminutes %>%
  group_by(purchase_date, weekday) %>%
  summarise(
    predict = sum(predict)
  ) %>%
  group_by(weekday) %>%
  summarise(
    weekday_median = median(predict),
    weekday_average = mean(predict)
  )

holidays <- myholidays
# holidays <- premierholidays
daydata <- newminutes %>%
  group_by(purchase_date, weekday, holiday, holiday_premier, biweekly, semimonthly) %>%
  summarise(
    predicted_sales = sum(predict)
  ) %>%
  left_join(dayofweekdata, by='weekday') %>%
  mutate(
    delta_over_weekday = predicted_sales - weekday_average,
    delta_over_median = predicted_sales - weekday_median,
    label = ifelse(semimonthly, 'Payday', NA),
  ) %>%
  select(
    -weekday_average, -weekday_median
  ) %>%
  ungroup() %>%
  mutate(
    holiday_label = ifelse(purchase_date %in% holidays, names(holidays)[match(purchase_date, holidays)], NA),
    label = ifelse(is.na(holiday_label), ifelse(is.na(label), NA, label), holiday_label)
  )

plotdata <- filter(daydata, purchase_date <= as.Date('2018-8-31'))

ggplot(plotdata, aes(purchase_date, predicted_sales, fill=weekday)) +
  geom_col() +
  labs(title='Predicted Purchases by Date', caption='Predictions made using only weekday/day of month/payday/holiday')


p1 <- ggplot(plotdata, aes(purchase_date, delta_over_weekday, fill=weekday, label=label)) +
  geom_col() +
  geom_text(nudge_y = ifelse(plotdata$delta_over_weekday > 0, 30, -60), angle = 45) +
  # geom_text(data=filter(daydata, delta_over_weekday > 50, purchase_date < as.Date('2018-08-31')), aes(purchase_date, delta_over_weekday, label=interaction(semimonthly, holiday_premier))) +
  labs(title='Delta over Weekday Prediction by Date', caption='Predictions made using only weekday/day of month/payday/holiday')
p1
ggplotly(p1, tooltip = c('x', 'y', 'fill'))

p2 <- ggplot(plotdata, aes(purchase_date, delta_over_median, fill=weekday, label=label)) +
  geom_col() +
  geom_text(nudge_y = ifelse(plotdata$delta_over_median > 0, 30, -15), angle = 45) +
  # geom_text(data=filter(daydata, delta_over_weekday > 50, purchase_date < as.Date('2018-08-31')), aes(purchase_date, delta_over_weekday, label=interaction(semimonthly, holiday_premier))) +
  labs(title='Delta over Weekday Median by Date', caption='Predictions made using only weekday/day of month/payday/holiday')
p2
ggplotly(p2, tooltip = c('x', 'y', 'fill'))

csvdata <- select(daydata, -label)
write.csv(csvdata, 'predicted_sales.csv', row.names=FALSE)

printdata <- filter(csvdata, purchase_date <= as.Date('2018-8-31')) %>% select(-holiday_label)
printdata <- arrange(printdata, desc(predicted_sales))
print(head(printdata, 10))


# daily ccu, purchasers data
#
# dailydata <- read.csv('niso_regression_statistics_full.csv')
# sapply(dailydata, class)
#
# semimonthlypayday <- dates(format(as.Date(c('2017-6-15', '2017-6-30', '2017-7-14', '2017-7-31', '2017-8-15', '2017-8-31', '2017-9-15', '2017-9-29', '2017-10-13', '2017-10-31', '2017-11-15', '2017-11-30', '2017-12-15', '2017-12-29',
#   '2018-1-15', '2018-1-31', '2018-2-15', '2018-2-28', '2018-3-15', '2018-3-30', '2018-4-13', '2018-4-30', '2018-5-15', '2018-5-31', '2018-6-15', '2018-6-29', '2018-7-13', '2018-7-31', '2018-8-15', '2018-8-31', '2018-9-14', '2018-9-28', '2018-10-15', '2018-10-31', '2018-11-15', '2018-11-30', '2018-12-14', '2018-12-31')), '%m/%d/%Y'))
#
#
# dailydata <- dailydata %>%
#   mutate(
#     game_id = as.factor(game_id),
#     payday = as.factor(payday),
#     date = as.Date(dt),
#     ts = as.POSIXct(date, '%Y-%m-%d', tz='UTC'),
#     weekday = factor(weekdays(date), levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
#     holiday_US = ifelse(holiday_region == 'US' | holiday_region == 'Global', TRUE, FALSE),
#     holiday_JP = ifelse(holiday_region == 'JP' | holiday_region == 'Global', TRUE, FALSE),
#     semimonthly = is.holiday(date, semimonthlypayday),
#     day_of_month = as.factor(as.numeric(strftime(ts, '%d'))),
#     ccu = as.numeric(as.character(ccu)),
#     unique_conversion_dau = unique_purchasers / dau,
#     streak_conversion_dau = (unique_purchasers - purch_ct_1_users) / dau,
#     unique_conversion_rpdau = unique_purchasers / rpdau,
#     streak_conversion_rpdau = (unique_purchasers - purch_ct_1_users) / rpdau,
#   ) %>%
#   select(
#     -dt, -dayofweek
#   ) %>%
#   select(
#     date, everything()
#   ) %>% arrange(
#     date
#   ) %>%
#   filter(date < as.Date('2018-6-1')) %>%
#   slice(-(1:3))
#
# saveRDS(dailydata, 'niso_regression_metrics.RData')
dailydata <- readRDS('niso_regression_metrics.RData')

plotdata <- dailydata %>%
  group_by(weekday) %>%
  summarise(
    n = n(),
    ccu = mean(ccu),
    dau_1 = mean(dau),
    rpdau_1 = mean(rpdau),
    ltv_100p_rpdau = mean(ltv_100p_rpdau),
    unique_purchasers = mean(unique_purchasers),
    purch_ct_1_users = mean(purch_ct_1_users),
    purch_ct_2p_users = unique_purchasers - purch_ct_1_users,
    playminutes = mean(playminutes),
    session_ct = mean(session_ct),
    unique_conversion_dau = weighted.mean(unique_conversion_dau, dau),
    streak_conversion_dau = weighted.mean(streak_conversion_dau, dau),
    unique_conversion_rpdau = weighted.mean(unique_conversion_rpdau, rpdau),
    streak_conversion_rpdau = weighted.mean(streak_conversion_rpdau, rpdau),
    # unique_conversion_dau = mean(unique_conversion_dau),
    # streak_conversion_dau = mean(streak_conversion_dau),
    # unique_conversion_rpdau = mean(unique_conversion_rpdau),
    # streak_conversion_rpdau = mean(streak_conversion_rpdau),
  ) %>%
  rename(
    dau = dau_1, rpdau = rpdau_1
  )

plotdata <- melt(plotdata, id.vars='weekday') %>% filter(variable != 'n' & variable != 'ltv_100p_rpdau')
ggplot(plotdata, aes(weekday, value, fill=weekday)) +
  geom_col() +
  facet_wrap(~ variable, scales='free_y') +
  labs(title='Metrics by Weekday') +
  theme(axis.text.x = element_text(angle = 75),
        strip.text.x = element_text(size = 5))

# DAY OF MONTH

plotdata <- dailydata %>%
  group_by(day_of_month) %>%
  summarise(
    n = n(),
    ccu = mean(ccu),
    dau_1 = mean(dau),
    rpdau_1 = mean(rpdau),
    ltv_100p_rpdau = mean(ltv_100p_rpdau),
    unique_purchasers = mean(unique_purchasers),
    purch_ct_1_users = mean(purch_ct_1_users),
    purch_ct_2p_users = unique_purchasers - purch_ct_1_users,
    playminutes = mean(playminutes),
    session_ct = mean(session_ct),
    unique_conversion_dau = weighted.mean(unique_conversion_dau, dau),
    streak_conversion_dau = weighted.mean(streak_conversion_dau, dau),
    unique_conversion_rpdau = weighted.mean(unique_conversion_rpdau, rpdau),
    streak_conversion_rpdau = weighted.mean(streak_conversion_rpdau, rpdau),
    # unique_conversion_dau = mean(unique_conversion_dau),
    # streak_conversion_dau = mean(streak_conversion_dau),
    # unique_conversion_rpdau = mean(unique_conversion_rpdau),
    # streak_conversion_rpdau = mean(streak_conversion_rpdau),
  ) %>%
  rename(
    dau = dau_1, rpdau = rpdau_1
  )

breaks = seq(0, 30, 5)
plotdata <- melt(plotdata, id.vars='day_of_month') %>% filter(variable != 'n' & variable != 'ltv_100p_rpdau')
ggplot(plotdata, aes(day_of_month, value, fill=day_of_month)) +
  geom_col() +
  facet_wrap(~ variable, scales='free_y') +
  scale_x_discrete(breaks=breaks) +
  labs(title='Metrics by Day of Month') +
  theme(strip.text.x = element_text(size = 5))

# HOLIDAYS

plotdata <- dailydata %>%
  group_by(holiday_region) %>%
  summarise(
    n = n(),
    ccu = mean(ccu),
    dau_1 = mean(dau),
    rpdau_1 = mean(rpdau),
    ltv_100p_rpdau = mean(ltv_100p_rpdau),
    unique_purchasers = mean(unique_purchasers),
    purch_ct_1_users = mean(purch_ct_1_users),
    purch_ct_2p_users = unique_purchasers - purch_ct_1_users,
    playminutes = mean(playminutes),
    session_ct = mean(session_ct),
    unique_conversion_dau = weighted.mean(unique_conversion_dau, dau),
    streak_conversion_dau = weighted.mean(streak_conversion_dau, dau),
    unique_conversion_rpdau = weighted.mean(unique_conversion_rpdau, rpdau),
    streak_conversion_rpdau = weighted.mean(streak_conversion_rpdau, rpdau),
    # unique_conversion_dau = mean(unique_conversion_dau),
    # streak_conversion_dau = mean(streak_conversion_dau),
    # unique_conversion_rpdau = mean(unique_conversion_rpdau),
    # streak_conversion_rpdau = mean(streak_conversion_rpdau),
  ) %>%
  rename(
    dau = dau_1, rpdau = rpdau_1
  )

plotdata <- melt(plotdata, id.vars='holiday_region') %>% filter(variable != 'n' & variable != 'ltv_100p_rpdau')
ggplot(plotdata, aes(holiday_region, value, fill=holiday_region)) +
  geom_col() +
  facet_wrap(~ variable, scales='free_y') +
  labs(title='Metrics by Holiday Region') +
  theme(axis.text.x = element_text(angle = 75),
        strip.text.x = element_text(size = 5))

# SEMIMONTHLY PAYDAY

plotdata <- dailydata %>%
  group_by(semimonthly) %>%
  summarise(
    n = n(),
    ccu = mean(ccu),
    dau_1 = mean(dau),
    rpdau_1 = mean(rpdau),
    ltv_100p_rpdau = mean(ltv_100p_rpdau),
    unique_purchasers = mean(unique_purchasers),
    purch_ct_1_users = mean(purch_ct_1_users),
    purch_ct_2p_users = unique_purchasers - purch_ct_1_users,
    playminutes = mean(playminutes),
    session_ct = mean(session_ct),
    unique_conversion_dau = weighted.mean(unique_conversion_dau, dau),
    streak_conversion_dau = weighted.mean(streak_conversion_dau, dau),
    unique_conversion_rpdau = weighted.mean(unique_conversion_rpdau, rpdau),
    streak_conversion_rpdau = weighted.mean(streak_conversion_rpdau, rpdau),
    # unique_conversion_dau = mean(unique_conversion_dau),
    # streak_conversion_dau = mean(streak_conversion_dau),
    # unique_conversion_rpdau = mean(unique_conversion_rpdau),
    # streak_conversion_rpdau = mean(streak_conversion_rpdau),
  ) %>%
  rename(
    dau = dau_1, rpdau = rpdau_1
  )

plotdata <- melt(plotdata, id.vars='semimonthly') %>% filter(variable != 'n' & variable != 'ltv_100p_rpdau')
ggplot(plotdata, aes(semimonthly, value, fill=semimonthly)) +
  geom_col() +
  facet_wrap(~ variable, scales='free_y') +
  labs(title='Metrics by SemiMonthly Payday') +
  theme(strip.text.x = element_text(size = 5))


# Modeling Conversion and CCU

modeldata <- select(dailydata,
    weekday, day_of_month, holiday_region, semimonthly, ccu, rpdau, unique_conversion_dau, streak_conversion_dau
    )
pred_vars <- c('ccu', 'rpdau', 'unique_conversion_dau', 'streak_conversion_dau')
pred_var <- 'ccu'

ind_vars <- names(modeldata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_vars))

var_names <- paste(ind_vars, collapse=' + ')

data_name <- paste('dailydata', pred_var, sep='_')


# Partition into training, validation, testing set

traindata <- modeldata

# spec = c(treat = .2, train = .7, test = .09, validate = 0.01)
# spec = c(train = .9, test = .09, validate = .01)
# spec = c(treat = .4, train = .6)
spec = c(train=.8, test=.2)

g = sample(cut(
  seq(nrow(modeldata)),
  nrow(modeldata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(modeldata, g)

treatdata <- res$treat
traindata <- res$train
testdata <- res$test

# TREAT DATA

try <- 1

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameNExperiment(traindata, varlist = ind_vars, outcomename = pred_var,
                               scale=TRUE,
                               rareCount=5,  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)
treatment_plan_factor <- cfe$treatments

treatment_name <- paste0(data_name, '_treatment_plan_', try, '.RData')
saveRDS(treatment_plan_factor, treatment_name)
treatment_plan_factor <- readRDS(treatment_name)

PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame))
PRUNE_SIG
sf <- treatment_plan_factor$scoreFrame
# ind_vars <- sf$varName[sf$sig <= PRUNE_SIG]
train.treat <- cfe$crossFrame
ind_vars <- setdiff(colnames(train.treat), c(pred_vars))

sf %>% arrange(sig)

# Model Formula

model_formula <- as.formula(paste(pred_var, ' ~ ', paste(ind_vars, collapse=" + "), sep=''))

test.treat <- vtreat::prepare(treatment_plan_factor, testdata, scale=TRUE)

x.train <- as.matrix(train.treat[,ind_vars])
y.train <- as.matrix(train.treat[,pred_var])
x.test <- as.matrix(test.treat[,ind_vars])
y.test <- as.matrix(test.treat[,pred_var])

# Regularized Regressions

fit.lasso <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=.5)

reg_results <- data.frame(name=character(), rmse=numeric())
for (i in 0:10) {
  model_name <- paste0('fit', i)
  assign(model_name, cv.glmnet(x.train, y.train, type.measure="mse",
                               alpha=i/10,family="gaussian"))
  regression_model <- get(model_name)
  # pred <- predict(model, s=model$lambda.1se, newx=x.test)
  # rmse <- sqrt(mean((y.test - pred)^2))
  lambda_index <- which(regression_model$lambda == regression_model$lambda.1se)
  cvm <- regression_model$cvm[lambda_index]
  results_temp <- data.frame(model_name, cvm)
  reg_results <- rbind(reg_results, results_temp)
  print(model_name)
  print(coef(regression_model, s=regression_model$lambda.1se))
}
rm(results_temp)
reg_results

# Plot solution paths:
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

par(par.default)

coef(fit10, s=fit10$lambda.1se)

# plot residuals
model <- fit0
pred <- predict(model, s=model$lambda.1se, new=x.train)
# pred2 <- predict(fit.ridge, newx=x.train, s=fit0$lambda.1se)
resid <- y.train - pred
plotres(model)
residdata <- traindata %>%
  mutate(
    resid = resid
  )


# GBM

model_name <- paste(data_name, 'gbm', try, 'v1.RData', sep='_')

n.trees = 1000
gbm1 <- gbm(model_formula, data=train.treat, distribution='gaussian', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
best.iter <- gbm.perf(gbm1, plot.it=TRUE)
if(gbm1$n.trees < best.iter*1.2){
  gbm1 <- gbm.more(gbm1, n.new.trees=(best.iter*1.2 - gbm1$n.trees))
  best.iter <- gbm.perf(gbm1, plot.it=FALSE)
}

saveRDS(gbm1, file=model_name)
gbm1 <- readRDS(model_name)

gbm.perf(gbm1)
best.iter <- gbm.perf(gbm1, method='test', plot.it=TRUE)
best.iter
par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance', plotit=TRUE)
var_imp <- subset(var_imp, rel.inf != 0)
var_imp <- var_imp %>% arrange(desc(rel.inf))
var_imp$var <- factor(var_imp$var, levels=rev(var_imp$var))
ggplot(var_imp, aes(var, rel.inf)) +
  geom_bar(stat='identity', fill='blue') +
  coord_flip() +
  ggtitle('GBM Variable Importance') +
  labs(x ='Variable', y = 'Relative Influence') +
  theme(plot.title = element_text(hjust=.5))
#
# ggplot2::ggsave(paste0(model_name, '.png'),
#                 path = '../Graphs',
#                 plot = last_plot(), # or give ggplot object name as in myPlot,
#                 width = 16, height = 9,
#                 units = "in", # other options c("in", "cm", "mm"),
#                 dpi = 'retina')

# PREDICTING ON NEW DATA

# biweeklypayday <- chron(format(seq(as.Date('2018-1-12'), as.Date('2018-12-28'), by='2 weeks'), '%m/%d/%Y'))
# semimonthlypayday <- dates(format(as.Date(c('2018-1-15', '2018-1-31', '2018-2-15', '2018-2-28', '2018-3-15', '2018-3-30', '2018-4-13', '2018-4-30', '2018-5-15', '2018-5-31', '2018-6-15', '2018-6-29', '2018-7-13', '2018-7-31', '2018-8-15', '2018-8-31', '2018-9-14', '2018-9-28', '2018-10-15', '2018-10-31', '2018-11-15', '2018-11-30', '2018-12-14', '2018-12-31')), '%m/%d/%Y'))
#
# newdays <- data.frame(date = seq(as.Date('2018-6-1'), as.Date('2018-12-31'), by='day'))
# holidays_2018 <- readRDS('holidays_2018.RData')
#
# months.levels <- c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# holiday_region.levels <- c('', 'Global', 'JP', 'US')
# newdays <- newdays %>%
#   left_join(holidays_2018, by='date')
# newdays <- newdays %>%
#   mutate(
#     holiday_region = as.factor(ifelse(is.na(holiday_region), '', as.character(holiday_region))),
#     ts = as.POSIXct(format(date), '%Y-%m-%d', tz='UTC'),
#     month = factor(format(ts, '%b'), levels=months.levels),
#     week = as.factor(format(ts, '%U')),
#     weekday = factor(weekdays(date), levels=c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday')),
#     holiday_US = ifelse(holiday_region == 'US' | holiday_region == 'Global', TRUE, FALSE),
#     holiday_JP = ifelse(holiday_region == 'JP' | holiday_region == 'Global', TRUE, FALSE),
#     semimonthly = is.holiday(date, semimonthlypayday),
#     day_of_month = as.factor(as.numeric(format(ts, '%d')))
#   )
#
# saveRDS(newdays, 'predictdays_june_dec_18.RData')
newdays <- readRDS('predictdays_june_dec_18.RData')

newdays.treat <- prepare(treatment_plan_factor, newdays, scale=TRUE)

# # gbm predict
#
yhat <- predict(gbm1, newdays.treat, n.trees=best.iter, type='response')
results[,pred_var] <- yhat

# regression predict
model <- fit9
yhat <- predict(model, s=model$lambda.1se, newx=as.matrix(newdays.treat))

# results <- data.frame(date = newdays$date)
results[,pred_var] <- yhat

newdays <- newdays %>%
  left_join(results, by='date')

# PLOT PREDICTIONS

var <- as.name(pred_var)
name <- quo_name(var)

plotdata <- newdays %>%
  group_by(weekday) %>%
  summarise(
    n = n(),
    metric = mean(!! var)
  )

ggplot(plotdata, aes(weekday, metric, fill=weekday)) +
  geom_col()

plotdata <- newdays %>%
  group_by(day_of_month) %>%
  summarise(
    n = n(),
    metric = mean(!! var)
  )

ggplot(plotdata, aes(day_of_month, metric, fill=day_of_month)) +
  geom_col()

plotdata <- newdays %>%
  group_by(holiday_region) %>%
  summarise(
    n = n(),
    metric = mean(!! var)
  )

ggplot(plotdata, aes(holiday_region, metric, fill=holiday_region)) +
  geom_col()

plotdata <- newdays %>%
  group_by(semimonthly) %>%
  summarise(
    n = n(),
    metric = mean(!! var)
  )

ggplot(plotdata, aes(semimonthly, metric, fill=semimonthly)) +
  geom_col()



dayofweekdata <- newdays %>%
  group_by(weekday) %>%
  summarise(
    ccu_median = median(ccu),
    unique_conversion_median = median(unique_conversion_dau),
    streak_conversion_median = median(streak_conversion_dau),
    ccu_mean = mean(ccu),
    unique_conversion_mean = mean(unique_conversion_dau),
    streak_conversion_mean = mean(streak_conversion_dau),
  )

plotdata <- melt(dayofweekdata, id.vars='weekday') %>% filter(variable != 'n' & variable != 'ltv_100p_rpdau')
ggplot(plotdata, aes(weekday, value, fill=weekday)) +
  geom_col() +
  facet_wrap(~ variable, scales='free_y') +
  labs(title='Metrics by Weekday') +
  theme(axis.text.x = element_text(angle = 75),
        strip.text.x = element_text(size = 5))

deltadata <- newdays %>%
  left_join(dayofweekdata, by='weekday') %>%
  mutate(
    ccu_delta_median = ccu - ccu_median,
    unique_conversion_delta_median = unique_conversion_dau - unique_conversion_median,
    streak_conversion_delta_median = streak_conversion_dau - streak_conversion_median,
    ccu_delta_mean = ccu - ccu_mean,
    unique_conversion_delta_mean = unique_conversion_dau - unique_conversion_mean,
    streak_conversion_delta_mean = streak_conversion_dau - streak_conversion_mean,
    label = ifelse(semimonthly, 'Payday', NA),
  ) %>%
  select(
    -ccu_median, -unique_conversion_median, -streak_conversion_median, -ccu_mean, -unique_conversion_mean, -streak_conversion_mean
  ) %>%
  ungroup() %>%
  mutate(
    holiday_label = ifelse(holiday_region != '', as.character(holiday_name), NA),
    label = ifelse(is.na(holiday_label), ifelse(is.na(label), NA, label), holiday_label)
  )

# saveRDS(deltadata, 'predictions_ccu_conversion.RData')
deltadata <- readRDS('predictions_ccu_conversion.RData')

# write.csv(deltadata, 'predictions_ccu_conversion_new.csv', row.names=FALSE)

ninetydays <- filter(deltadata, date <= as.Date('2018-8-31'))

# CCU

# ggplot(ninetydays, aes(date, ccu, fill=weekday)) +
#   geom_col() +
#   labs(title='Predicted CCU by Date', caption='Predictions made using only weekday/day of month/payday/holiday')
#
# p2 <- ggplot(ninetydays, aes(date, ccu_delta_median, fill=weekday, label=label)) +
#   geom_col() +
#   geom_text(nudge_y = ifelse(deltadata$ccu_delta_median > 0, 30, -15), angle = 45) +
#   # geom_text(data=filter(daydata, delta_over_weekday > 50, purchase_date < as.Date('2018-08-31')), aes(purchase_date, delta_over_weekday, label=interaction(semimonthly, holiday_premier))) +
#   labs(title='Delta over Weekday Median', caption='Predictions made using only weekday/day of month/payday/holiday')
# p2
# ggplotly(p2, tooltip = c('x', 'y', 'fill'))

# Unique Conversion RPDAU

# ggplot(ninetydays, aes(date, unique_conversion_dau, fill=weekday)) +
#   geom_col() +
#   labs(title='Predicted Unique Conversion/RPDAU by Date', caption='Predictions made using only weekday/day of month/payday/holiday') +
#   coord_cartesian(ylim = c(min(ninetydays$unique_conversion_dau)-.0001, max(ninetydays$unique_conversion_dau)+.0001))
#
# p2 <- ggplot(ninetydays, aes(date, unique_conversion_delta_median, fill=weekday, label=label)) +
#   geom_col() +
#   geom_text(angle = 45) +
#   # geom_text(data=filter(daydata, delta_over_weekday > 50, purchase_date < as.Date('2018-08-31')), aes(purchase_date, delta_over_weekday, label=interaction(semimonthly, holiday_premier))) +
#   labs(title='Delta over Weekday Median', caption='Predictions made using only weekday/day of month/payday/holiday')
# p2
# ggplotly(p2, tooltip = c('x', 'y', 'fill'))

# Streak Conversion

ggplot(ninetydays, aes(date, streak_conversion_dau, fill=weekday)) +
  geom_col() +
  labs(title='Predicted Streak Conversion/RPDAU by Date', caption='Predictions made using only weekday/day of month/payday/holiday') +
  coord_cartesian(ylim = c(min(ninetydays$streak_conversion_dau)-.0001, max(ninetydays$streak_conversion_dau)+.0001))

p2 <- ggplot(ninetydays, aes(date, streak_conversion_delta_median, fill=weekday, label=label)) +
  geom_col() +
  geom_text(angle = 45) +
  # geom_text(data=filter(daydata, delta_over_weekday > 50, purchase_date < as.Date('2018-08-31')), aes(purchase_date, delta_over_weekday, label=interaction(semimonthly, holiday_premier))) +
  labs(title='Delta over Weekday Median', caption='Predictions made using only weekday/day of month/payday/holiday')
p2
ggplotly(p2, tooltip = c('x', 'y', 'fill'))


View(arrange(ninetydays, desc(unique_conversion_dau)))




# rank ordering for content/inflation by month

deltadata <- readRDS('predictions_ccu_conversion.RData')

rankdata <- deltadata %>%
  group_by(month) %>%
  mutate(
    unique_payers = ccu * unique_conversion_dau,
    streak_payers = ccu * streak_conversion_dau,
    # ccu_score = min_rank(desc(ccu)),
    # unique_conversion_dau_score = min_rank(desc(unique_conversion_dau)),
    # streak_conversion_dau_score = min_rank(desc(streak_conversion_dau)),
    # ccu_delta_median_score = min_rank(desc(ccu_delta_median)),
    # unique_conversion_delta_median_score = min_rank(desc(unique_conversion_delta_median)),
    # streak_conversion_delta_median_score = min_rank(desc(streak_conversion_delta_median)),
    # unique_payers_score = min_rank(desc(unique_payers)),
    # streak_payers_score = min_rank(desc(streak_payers)),
    content_release_score = (unique_payers - min(unique_payers)) / (max(unique_payers) - min(unique_payers)),
    inflation_release_score = (streak_payers - min(streak_payers)) / (max(streak_payers) - min(streak_payers)),
  ) %>% 
  ungroup() %>%
  select(date, content_release_score, inflation_release_score, everything()) %>%
  # select(-c(month, day_of_month, week, day_of_month, ts, label, holiday_label, holiday_US, holiday_JP)) %>%
  # select(date, month, week, weekday, day_of_month, semimonthly, holiday_name, holiday_region, unique_payers_score, streak_payers_score, ccu_score, unique_conversion_dau_score, streak_conversion_dau_score, ccu_delta_median_score, unique_conversion_delta_median_score, streak_conversion_delta_median_score) %>%
  arrange(date)

# write.csv(rankdata, 'predictions_content_inflation_release.csv', row.names=FALSE)

plot_month <- 'Aug'
plotdata <- rankdata %>%
  mutate(
    label = ifelse(is.na(label), '', label)
  ) %>%
  mutate(week = factor(week, rev(levels(week)))) %>%
  arrange(date)

p1 <- ggplot(plotdata, aes(x = weekday, y = week, fill = content_release_score, label=paste(day_of_month, label, sep='\n'),
                     text = ifelse(label == '',
                                   paste('Content Release Score:', format(round(content_release_score, 2), nsmall=2), '<br>',
                                         'CCU:', round(ccu, 0), '<br>',
                                         'Unique Conversion/DAU:', percent(unique_conversion_dau, 2),'<br>',
                                         'Streak Conversion/DAU:', percent(streak_conversion_dau, 2)),
                                   paste('Content Release Score:', format(round(content_release_score, 2), nsmall=2), '<br>',
                                         'CCU:', round(ccu, 0), '<br>',
                                         'Unique Conversion/DAU:', percent(unique_conversion_dau, 2),'<br>',
                                         'Streak Conversion/DAU:', percent(streak_conversion_dau, 2),'<br>',
                                         'Label:', label)))) +
  geom_tile(colour = "black") +
  geom_text(size = 4) +
  facet_wrap(~ month, scales = 'free_y', ncol=2) +
  labs(title='Content Release') +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', midpoint = .5) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.background = element_rect(fill = "transparent"))+
  # theme(legend.position = "none") +
  theme(plot.title = element_text(hjust=.5))
p1

ggplotly(p1, tooltip='text')


p2 <- ggplot(plotdata, aes(x = weekday, y = week, fill = inflation_release_score, label=paste(day_of_month, label, sep='\n'),
                     text = ifelse(label == '',
                                   paste('Inflation Release Score:', format(round(inflation_release_score, 2), nsmall=2), '<br>',
                                  'CCU:', round(ccu, 0), '<br>',
                                  'Unique Conversion/DAU:', percent(unique_conversion_dau, 2),'<br>',
                                  'Streak Conversion/DAU:', percent(streak_conversion_dau, 2)),
                                  paste('Inflation Release Score:', format(round(inflation_release_score, 2), nsmall=2), '<br>',
                                        'CCU:', round(ccu, 0), '<br>',
                                        'Unique Conversion/DAU:', percent(unique_conversion_dau, 2),'<br>',
                                        'Streak Conversion/DAU:', percent(streak_conversion_dau, 2),'<br>',
                                        'Label:', label)))) +
  geom_tile(colour = "black") +
  geom_text(size = 4) +
  facet_wrap(~ month, scales = 'free_y', ncol=2) +
  labs(title='Inflation') +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', midpoint = .5) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.background = element_rect(fill = "transparent"))+
  # theme(legend.position = "none") +
  theme(plot.title = element_text(hjust=.5))
p2

ggplotly(p2, tooltip='text')




