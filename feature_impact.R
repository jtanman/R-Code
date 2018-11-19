# By: James Tan

# Date: 6/5/2018

library(plotly)
library(ggplot2)
library(feather)
library(reshape2)
library(glmnet)
library(ggbiplot)
library(forecast)
library(vtreat)
library(gbm)
library(DMwR)
library(LiblineaR)
library(plotmo)
library(dplyr)
summarise <- dplyr::summarise

source('~/.Rprofile')
library(rutils)
setwd(datapath)

par.default <- par()

# revdata <- readRDS('metric_dau_isogroup_timeseries.RData')
# revdata <- revdata %>%
#   mutate(
#     date = as.Date(date),
#   )
# revdata[is.na(revdata)] <- 0
# revdata <- select(revdata, -c(step_up_10, step_up_120, step_up_5_to_10, step_up_10_to_20, step_up_100_to_120, step_up_50_to_120, dau_step_up_10, dau_step_up_120))
# 
# ggplot(revdata, aes(date, revenue, color=iso_group, group=iso_group)) + geom_line()
# 
# featuredata <- read.csv("feature_calendar_isogroup.csv", sep=',')
# sapply(featuredata, class)
# featuredata <- featuredata %>%
#   mutate(
#     release_dt = as.Date(release_dt, format='%m/%d/%y'),
#   )
# 
# dplyr::summarise(
#   category = paste(category, collapse='/'),
#   feature = paste(feature, collapse='/')
# ) %>%
#   mutate(
#     category = ifelse(category == 'NA', NA, category),
#     feature = ifelse(feature == 'NA', NA, feature),
#   ) %>% ungroup() %>%
#   select(
#     date, category, feature, everything()
#   )
# 
# singlefeature <- featuredata %>%
#   group_by(iso_group, release_dt) %>%
#   filter(!is.na(release_dt)) %>%
#   dplyr::summarise(
#     category = paste(category, collapse='/'),
#     feature = paste(feature, collapse='/')
#   ) %>%
#   mutate(
#     category = ifelse(category == 'NA', NA, category),
#     feature = ifelse(feature == 'NA', NA, feature),
#   ) %>% ungroup() %>%
#   select(
#     release_dt, category, feature, everything()
#   )
# 
# holidaydata <- read.csv('niso_regression_metrics.csv') %>%
#   mutate(
#     game_id = as.factor(game_id),
#     date = as.Date(dt),
#   ) %>%
#   select(
#     date, holiday, holiday_region, holiday_name
#   ) %>% arrange(date)
# 
# tsdata <- revdata %>%
#   left_join(singlefeature, by=c('date' = 'release_dt', 'iso_group')) %>%
#   filter(iso_group >= 12) %>%
#   mutate(iso_group = as.factor(iso_group)) %>%
#   left_join(holidaydata, by='date') %>%
#   mutate(
#     holiday = ifelse(is.na(holiday), FALSE, ifelse(holiday_region == 'US' | holiday_region == 'Global', TRUE, FALSE))
#   )
# 
# mydata <- revdata %>%
#   left_join(featuredata, by=c('date' = 'release_dt', 'iso_group')) %>%
#   filter(iso_group >= 12) %>%
#   mutate(iso_group = as.factor(iso_group)) %>%
#   left_join(holidaydata, by='date') %>%
#   mutate(
#     holiday = ifelse(is.na(holiday), FALSE, ifelse(holiday_region == 'US' | holiday_region == 'Global', TRUE, FALSE))
#   )
# 
# saveRDS(mydata, 'rev_feature_data_parsed.RData')
# saveRDS(tsdata, 'rev_feature_data_single_parsed.RData')

installsdata <- read.csv('isogroup_installs.csv')



mydata <- readRDS('rev_feature_data_parsed.RData')
tsdata <- readRDS('rev_feature_data_single_parsed.RData')

isodata <- filter(tsdata, iso_group == 12) %>% arrange(date)

plotdata <- isodata %>%
  select(
    date, iso_group, revenue, dau, pdau, rpdau, purchases, purchasers_unique, play_mins, logins_chat
  )
plotdata <- melt(plotdata, id.vars = c('date', 'iso_group')) %>% filter(!variable %in% c('category', 'feature'))

ggplot(plotdata, aes(date, value, group=variable)) +
  geom_line() +
  facet_wrap(~ variable, scales = 'free_y')

# PCA

pcadata <- select(isodata, -date, -iso_group, -category, -feature)
pca <- prcomp(pcadata, center=TRUE, scale=TRUE, na.action= na.omit)

print(pca)
plot(pca, type='l')
summary(pca)

ggbiplot(pca)
rot <- pca$rotation
rot <- data.frame(names=rownames(rot), rot)
ggplot(rot, aes(x=names, y=PC1, group=1)) +
  geom_line() +
  labs(title='Principle Component 1') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=75, vjust=.7))

# Stationarity

ndiffs(isodata[, "revenue"], alpha = 0.05, test = c("adf"))


# Clean data

modeldata <- isodata %>%
  filter(date > as.Date('2017-9-1')) %>%
  mutate(
    feature_bool = ifelse(is.na(feature), FALSE, TRUE),
    tmpG = cumsum(c(FALSE, as.logical(diff(feature_bool)))),
    tmpA = as.numeric(c(0, diff(date)) * !feature_bool),
    tmpB = as.numeric(c(diff(date), 0) * !feature_bool),
  ) %>%
  dplyr::group_by(tmpG) %>%
  dplyr::mutate(days_since_feature = cumsum(tmpA),
         days_before_feature = rev(cumsum(rev(tmpB)))) %>%
  ungroup() %>%
  select(-c(feature_bool, tmpA, tmpB, tmpG)) %>%
  filter(!holiday) %>%
  select(-c(holiday, holiday_region, holiday_name)) %>%
  as.data.frame

ind_vars <- names(modeldata)[!names(modeldata) %in% c('date', 'iso_group', 'category', 'days_since_feature', 'days_before_feature')]

lagdata <- modeldata
for(var in ind_vars){
  name <- paste(var, 'lag1', sep='_')
  lagdata[,name] <- lag(lagdata[,var], 1)
}

lagdata <- filter(lagdata, is.na(feature), is.na(feature_lag1))

drop_vars <- base::setdiff(ind_vars, 'revenue')

keep_vars <- setdiff(names(lagdata), drop_vars)

lagdata <- select(lagdata, !! quo(keep_vars))

modeldata <- lagdata %>%
  mutate(
    weekday = weekdays(date)
  ) %>%
  select(-c(iso_group, feature_lag1, category, days_before_feature)) %>%
  slice(-1)

# modeldata <- modeldata %>%
#   select(-c(date, iso_group, category_lag1, feature_lag1)) %>%
#   slice(-1)

# Choose model variables

pred_vars <- c('revenue')
pred_var <- 'revenue'

ind_vars <- names(modeldata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_vars))

model_formula <- as.formula(paste(pred_var, ' ~ ', paste(ind_vars, collapse=" + "), sep=''))

data_name <- 'metric_feature_ts_with_date_days_since_feature'

# Partition into training, validation, testing set

# spec = c(treat = .2, train = .7, test = .09, validate = 0.01)
# spec = c(train = .9, test = .09, validate = .01)
# spec = c(treat = .4, train = .6)
spec = c(train = .8, test = .2)

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

# TREAT DATA

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameNExperiment(traindata, varlist = ind_vars, outcomename = pred_var,
                               scale=TRUE,
                               rareCount=5,  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)
treatment_plan_factor <- cfe$treatments
PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame))
sf <- treatment_plan_factor$scoreFrame
ind_vars <- sf$varName[sf$sig <= PRUNE_SIG]
train.treat <- cfe$crossFrame

sf %>% arrange(sig)

model_formula <- as.formula(paste(pred_var, ' ~ ', paste(ind_vars, collapse=" + "), sep=''))

test.treat <- vtreat::prepare(treatment_plan_factor, testdata, scale=TRUE, pruneSig=PRUNE_SIG)

x.train <- as.matrix(train.treat[,ind_vars])
y.train <- as.matrix(train.treat[,pred_var])
x.test <- as.matrix(test.treat[,ind_vars])
y.test <- as.matrix(test.treat[,pred_var])

# Regressions

fit.lasso <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x=x.train, y=y.train, family="gaussian", alpha=.5)

results <- data.frame(name=character(), rmse=numeric())
for (i in 0:10) {
  model_name <- paste0('fit', i)
  assign(model_name, cv.glmnet(x.train, y.train, type.measure="mse",
                                            alpha=i/10,family="gaussian"))
  pred <- predict(get(model_name), s=get(model_name)$lambda.1se, newx=x.test)
  rmse <- sqrt(mean((y.test - pred)^2))
  results_temp <- data.frame(model_name, rmse)
  results <- rbind(results, results_temp)
  print(model_name)
  print(coef(get(model_name), s=get(model_name)$lambda.1se))
}
rm(results_temp)
results


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
model <- fit10
pred <- predict(model, s=model$lambda.1se, new=x.train)
# pred2 <- predict(fit.ridge, newx=x.train, s=fit0$lambda.1se)
resid <- y.train - pred
plotres(model)
residdata <- traindata %>%
  mutate(
    resid = resid
  )

View(filter(traindata, row_number() %in% c(7, 53, 60)))

ggplot(residdata, aes(date, resid)) +
  geom_point() +
  scale_x_date(date_breaks='1 month', date_labels='%b %y')

# GBM

model_name <- paste(data_name, 'gbm', 'v1.RData', sep='_')

n.trees = 2000
gbm1 <- gbm(model_formula, data=train.treat, distribution='gaussian', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
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

gbm_predict <- predict(gbm1, test.treat, n.trees=best.iter, type='response')
results_gbm <- data.frame(gbm_predict, test.treat[,pred_var])
colnames(results_gbm) <- c('prob', 'actual')

gbm_error <- regr.eval(results_gbm[,'actual'], results_gbm[,'prob'], stats = c('mae', 'rmse', 'mape'))

# Prediction Delta


preddata <- isodata %>%
  filter(date > as.Date('2017-9-1')) %>%
  mutate(
    feature_bool = ifelse(is.na(feature), FALSE, TRUE),
    tmpG = cumsum(c(FALSE, as.logical(diff(feature_bool)))),
    tmpA = as.numeric(c(0, diff(date)) * !feature_bool),
    tmpB = as.numeric(c(diff(date), 0) * !feature_bool),
  ) %>%
  dplyr::group_by(tmpG) %>%
  dplyr::mutate(days_since_feature = cumsum(tmpA),
                days_before_feature = rev(cumsum(rev(tmpB)))) %>%
  ungroup() %>%
  select(-c(feature_bool, tmpA, tmpB, tmpG)) %>%
  left_join(holidaydata, by='date') %>%
  # filter(!holiday) %>%
  select(-c(holiday, holiday_region, holiday_name)) %>%
  as.data.frame

ind_vars <- names(preddata)[!names(preddata) %in% c('date', 'iso_group', 'category', 'days_since_feature', 'days_before_feature')]

lagdata <- preddata
for(var in ind_vars){
  name <- paste(var, 'lag1', sep='_')
  lagdata[,name] <- lag(lagdata[,var], 1)
}

# lagdata <- filter(lagdata, is.na(feature), is.na(feature_lag1))

drop_vars <- base::setdiff(ind_vars, 'revenue')

keep_vars <- setdiff(names(lagdata), drop_vars)

lagdata <- select(lagdata, !! quo(keep_vars))

preddata <- lagdata %>%
  mutate(
    weekday = weekdays(date)
  ) %>%
  select(-c(iso_group, feature_lag1, category, days_before_feature)) %>%
  slice(-1)

# treat new data

pred.treat <- vtreat::prepare(treatment_plan_factor, preddata, scale=TRUE, pruneSig=PRUNE_SIG)

sf <- treatment_plan_factor$scoreFrame
ind_vars <- sf$varName[sf$sig <= PRUNE_SIG]
x.pred <- as.matrix(pred.treat[,ind_vars])

pred.treat$date <- preddata$date

filter(pred.treat, date==as.Date("2018-5-28"))
filter(pred.treat, date==as.Date("2018-5-27"))
filter(pred.treat, date==as.Date("2018-5-29"))
predict(gbm1, filter(pred.treat, date==as.Date("2018-5-29")), n.trees=best.iter)

model <- fit10
pred <- data.frame(date=preddata$date, pred=as.vector(predict(model, s=model$lambda.1se, new=x.pred)))
pred <- data.frame(date=preddata$date, pred=as.vector(predict(gbm1, pred.treat, n.trees=best.iter)))

alldata <- tsdata %>% filter(iso_group == 12) %>%
  ungroup() %>%
  arrange(date) %>%
  filter(date > as.Date('2017-9-1')) %>%
  slice(-1) %>%
  left_join(pred, by='date') %>%
  mutate(
    feature_lag = lag(feature),
    valid = ifelse(is.na(lag(feature)), ifelse(holiday, FALSE, ifelse(lag(holiday), FALSE, TRUE)), FALSE),
    delta = revenue - pred
  ) %>%
  select(
    date, category, feature, holiday, revenue, pred, valid, delta
  ) %>%
  arrange(date)

validdata <- alldata %>%
  filter(!holiday, is.na(feature)) %>%
  arrange(desc(delta))

categorydata <- alldata %>%
  filter(date < as.Date('2018-3-1')) %>%
  group_by(category) %>%
  summarise(
    n = n(),
    delta = mean(delta)
  ) %>%
  arrange(desc(delta))

write.csv(alldata, 'feature_impact_deltas.csv', row.names=FALSE)
View(alldata %>% arrange(desc(delta)) %>% filter(!is.na(feature)))


top10 <- arrange(alldata, desc(delta)) %>% filter(!is.na(feature)) %>% dplyr::top_n(15, delta) %>% pull(delta)
ggplot(alldata, aes(date)) +
  geom_line(aes(y=revenue, color='revenue')) +
  geom_line(aes(y=pred, color='pred')) +
  geom_point(aes(y=ifelse(!is.na(feature), revenue, NA))) +
  geom_text(aes(y=ifelse(!is.na(feature) & (delta %in% top10), revenue, NA), label=feature), nudge_y=50000) +
  scale_x_date(date_breaks='1 month', date_labels='%b %y') +
  labs(title='Revenue vs Prediction', subtitle='Using lasso regression', caption='All features indicated by points, top 15 deltas labeled')


ggplot(alldata, aes(date, delta, group=1, label=feature)) +
  geom_col() +
  geom_text()

plotdata <- alldata %>%
  select(date, feature, revenue, pred)
plotdata <- melt(plotdata, id.vars=c('date', 'feature'))
ggplot(plotdata, aes(date, value, color=variable)) +
  geom_line() +
  geom_point(data=plotdata, aes(label=ifelse(variable=='revenue', feature, NA)))




featuredata <- alldata %>%
  filter(!is.na(feature))
















