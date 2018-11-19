# James Tan

# Churn prediction analysis

library(scales)
library(ggplot2)
library(plyr)
library(randomForest)
library(caret)
library(ROCR)
library(AUC)
library(gbm)
library(rpart)
library(PRROC)
library(VIM)
library(dplyr)
library(vtreat)

set.seed(21)
par.default <- par()

source('~/.Rprofile')
library(rutils)
setwd(datapath)

cohortdaydata <- loadData('tow_cohortday_data_2018.3.15')
cohortdaydata$run_date <- as.Date(cohortdaydata$run_date)
userdata <- loadData('tow_users_2018.3.15')
userdata$install_date <- as.Date(userdata$install_date)
saveData(userdata, name='tow_user_data_99', overwrite=TRUE)

mydata <- merge(userdata, cohortdaydata, by=c('udid', 'install_ts', 'install_date'))

nacolumns <- apply(mydata, 2, function(x) length(which(is.na(x))))
names(which(nacolumns > 0))

mydata <- mydata[which(mydata$hw_type != 'post-gamestate-tool'),]
mydata$install_date <- as.Date(mydata$install_date)
mydata$lang <- as.factor(mydata$lang)
mydata$ip_country <- as.factor(mydata$ip_country)
mydata$hw_type <- as.factor(mydata$hw_type)
mydata$hw_ver <- as.factor(mydata$hw_ver)
mydata$os_ver <- as.factor(mydata$os_ver)
mydata$device_type <- as.factor(mydata$device_type)
mydata$device_lang <- as.factor(mydata$device_lang)

# NAcolumns <- c('food', 'piercing', 'ruby', 'breedingtoken', 'guildchatcount', 'attacks', 'follows', 'defenses',
#                'defenseassists', 'breedertaskcompleted', 'dailytokenscollected', 'dragonexpertreached',
#                'dragonupgraded', 'eggincubationcomplete', 'fogcleared', 'foliagecleared', 'guildapplied',
#                'guildexited', 'researchcompleted', 'towercomplete', 'towerupgraded',  'numfoodgiftsent',
#                'numfoodgiftrec', 'numpiercgiftsent', 'numpiercgiftrec', 'sumfoodgiftsent', 'sumfoodgiftrec',
#                'sumpiercgiftsent', 'sumpiercgiftrec', 'numgiftrec', 'numgiftsent')
# for(c in NAcolumns){
#   mydata[is.na(mydata[,c]),c] <- 0
# }

keepNAcolumns <- c('elo_start', 'elo_end', 'elo_min', 'elo_max', 'elo_change', 'elo_diff', 'elo_abs_diff', 'startup_times', 'first_startup')
keepData <- mydata[,keepNAcolumns]
mydata[is.na(mydata)] <- 0
for(c in keepNAcolumns){
    mydata[,c] <- keepData[,c]
}
rm(keepData)

mydata <- mydata %>%
    select(-c(chat_global, chat_guild, chats))
mydata <- mydata %>%
    mutate(
        guild_store_coins_earned = -1 * guild_store_coins_earned
    )

mydata[which(mydata$gametime < 0), 'gametime'] <- NA

alldata <- mydata
saveData(alldata, name='tow_all_data_by_cohortday_99_parsed', overwrite=TRUE)

alldata <- loadData(name='tow_all_data_by_cohortday_99_parsed')
userdata <- loadData('tow_user_data_99')

measure_day <- 7
data_day <- 3

alldata <- alldata %>%
    filter(
        install_ts >= '2017-11-17',
        udid %in% filter(., cohortday == (measure_day + 1))$'udid',
        cohortday <= measure_day
    )

measuredata <- alldata %>%
    filter(cohortday == measure_day) %>%
    mutate(
        ret = ifelse(sessions > 0, 1, 0),
        ret.factor = as.factor(ret)
    ) %>%
    select(udid, ret, ret.factor)

sessiondata <- alldata %>%
    group_by(udid) %>%
    summarise(
        sessions = sum(sessions)
    ) %>%
    arrange(sessions)

daydata <- alldata %>%
    filter(cohortday <= data_day) %>%
    group_by(udid) %>%
    summarise(
        battles = sum(player_wins + bot_wins + player_losses + bot_losses),
        win_percent = sum(player_wins + bot_wins) / battles,
        player_battles_percent = sum(player_wins + player_losses) / battles,
        battles_percent_2v2 = sum(battles2v2) / battles,
        win_percent_2v2 = sum(battles2v2_wins) / sum(battles2v2),
        player_battles_percent_2v2 = sum(playerbattles2v2) / sum(playerbattles2v2 + botbattles2v2),
        avgstartup = sum(startup_times, na.rm=TRUE) / sum(startup_count),
        first_startup = mean(first_startup, na.rm=TRUE),
        elo_diff = sum(elo_diff) / sum(elo_battles),
        elo_abs_diff = sum(elo_abs_diff) / sum(elo_battles),
        max_cohortday = ifelse(is.finite(max(cohortday[which(sessions > 0)])), max(cohortday[which(sessions > 0)]), 0),
        session_days = sum(sessions > 0),
        sessions = sum(sessions),
        rev = sum(rev),
        rev_count = sum(rev_count),
        player_wins = sum(player_wins),
        player_losses = sum(player_losses),
        battles2v2_allyjoined = sum(battles2v2_allyjoined),
        battles2v2_joinedally = sum(battles2v2_joinedally),
        battles_pvp_free = sum(battles_pvp_free) / battles,
        battles_campaign = sum(battles_campaign) / battles,
        battles_prize_fights = sum(battles_prize_fights) / battles,
        battles_boss_wars = sum(battles_boss_wars) / battles,
        time_per_session = sum(gametime) / sessions,
        # guild_events = sum(guild_events),
        # elo_end = ifelse(is.finite(max_cohortday), elo_end[which(cohortday == max_cohortday)], NA),
        elo_change = sum(elo_change, na.rm=TRUE),
        # chats = sum(chats),


        diamond_spend = sum(diamond_spend),
        diamond_spend_count = sum(diamond_spend_count),
        diamond_expedite_percent = sum(diamond_expedite) / diamond_spend,
        diamond_gacha_percent = sum(diamond_gacha) / diamond_spend,
        diamond_energy_percent = sum(diamond_energy) / diamond_spend,
        diamond_coins_on_crafting_percent = sum(diamond_coins_on_crafting) / diamond_spend,
        diamond_coins_purchased_percent = sum(diamond_coins_purchased) / diamond_spend,

        energy_spend = sum(energy_spend),
        crafting_coins_spend = sum(crafting_coins_spend),
        crafting_coins_spend_count = sum(crafting_coins_spend_count),
        guild_store_coins_spend = sum(guild_store_coins_spend),
        guild_store_coins_spend_count = sum(guild_store_coins_spend_count),
        guild_store_coins_earned = sum(guild_store_coins_earned),
        guild_store_coins_earned_count = sum(guild_store_coins_earned_count),
        unit_upgrades = sum(unit_upgrades),
        timed_chests_opened = sum(timed_chests_opened),
        diamond_earned = sum(diamond_earned),
        energy_earned = sum(energy_earned),
        coins_earned = sum(coins_earned),
        dailyquests = sum(dailyquests),
        numcrashes = sum(numcrashes)
    ) %>%
    ungroup()

mydata <- merge(userdata, daydata, by='udid')
mydata <- merge(mydata, measuredata, by = 'udid')

parsed_name <- paste0('tow_d', measure_day, 'ret_day', data_day, '_99_parsed_v1')

# saveData(mydata,name=parsed_name, overwrite=TRUE)

mydata <- loadData(name=parsed_name)
# mydata <- mydata %>%
#     select(-c(udid, install_ts, max_cohortday, session_days))

mydata <- mydata %>%
    select(-c(udid, install_ts, country, game))

ind_vars <- names(mydata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c('ret', 'ret.factor'))

# imputedata <- mydata[,!(colnames(mydata) %in% c('trophy'))]
# aggr_plot <- aggr(imputedata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata),
#   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# installdata <- ddply(mydata, .(install_date), summarise,
#                      d3churn = mean(d3churn)
# )

pred_var <- 'ret.factor'
pred_var_num <- 'ret'
var_names <- paste(ind_vars, collapse=' + ')

model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))
gbm_formula <- as.formula(paste(pred_var_num, ' ~ ', var_names, sep=''))


corr_vars <- c(pred_var, ind_vars)
corr_vars <- names(mydata)[sapply(mydata, class) == 'numeric' | sapply(mydata, class) == 'integer']
corr_vars <- corr_vars[corr_vars != 'max_cohortday']
corr_vars <- corr_vars[c(length(corr_vars), seq(2, length(corr_vars) - 1, 1))]
corrmatrix <- cor(mydata[,corr_vars], use='pairwise.complete.obs')
corrmatrix <- corrmatrix[order(corrmatrix[,1], decreasing=TRUE),]
corrmatrix <- corrmatrix[,rownames(corrmatrix)]

ggplot(melt(corrmatrix), aes(x=Var1, y=Var2, fill=value, label=format(round(value, 2), nsmall=2))) +
    geom_tile() +
    geom_text(size=2, alpha=.3) +
    scale_fill_gradientn(colours=c('#ef8a62','#f7f7f7','#67a9cf'), limits=c(-1,1), values=c(0, .5, 1)) +
    theme(axis.text.x = element_text(angle=75, vjust=.7),
          axis.title.x=element_text(margin=ggplot2::margin(-30,0,0,0)),
          axis.title.y=element_text(margin=ggplot2::margin(0,-10,0,0)))




# Partition into training, validation, testing set

# spec = c(treat = .2, train = .7, test = .09, validate = 0.01)
spec = c(train = .9, test = .09, validate = .01)

g = sample(cut(
  seq(nrow(mydata)),
  nrow(mydata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(mydata, g)

# treatdata <- res$treat
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

cfe <- mkCrossFrameCExperiment(traindata, varlist = ind_vars, outcomename = pred_var, outcometarget = 1,
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

treatment_plan_factor <- designTreatmentsC(treatdata, varlist = ind_vars, outcomename = pred_var, outcometarget = 1,
                                        rareCount=5,  # Note set this to something larger, like 5
                                        rareSig=.3 # Note set this to something like 0.3
)

train.treat <- prepare(treatment_plan_factor, traindata, scale=TRUE, pruneSig=.05)
var_names <- setdiff(colnames(train.treat), c(pred_var, pred_var_num))

# treat validation and testing set

valid.treat <- vtreat::prepare(treatment_plan_factor, validdata, scale=TRUE, pruneSig=PRUNE_SIG)
test.treat <- vtreat::prepare(treatment_plan_factor, testdata, scale=TRUE, pruneSig=PRUNE_SIG)
train.treat[,pred_var_num] <- traindata[,pred_var_num]
valid.treat[,pred_var_num] <- validdata[,pred_var_num]
test.treat[,pred_var_num] <- testdata[,pred_var_num]

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))
num_formula <- as.formula(paste(pred_var_num, ' ~ ', paste(var_names, collapse=" + "), sep=''))


set.seed(21)
smalltrain <- traindata[sample(1:nrow(traindata), 100000),]


# LOG REGRESSION WITH REGULARIZATION

tryTypes <- c(0, 6, 7)
tryCosts <- c(10000, 1000, 100, 10, 1, .01)
lr_matrix <- data.frame()

for(ty in tryTypes){
  for(co in tryCosts){
    log.regression <- LiblineaR(data=train.treat[,var_names], target=train.treat[,pred_var], type=ty, cost=co)
    fitted_values <- predict(log.regression, valid.treat[var_names], proba=TRUE)
    results <- data.frame(fitted_values[[2]][,2], valid.treat[,pred_var], valid.treat[,pred_var_num])
    colnames(results) <- c('prob', 'actual', 'actual_num')
    pred <- prediction(results[,'prob'], results[,'actual'])
    RP.perf <- performance(pred, "prec", "rec")
    pr.auc <- pr.curve(scores.class0 = results[,'prob'], weights.class0 = results[,'actual_num'])

    row <- data.frame(type=ty, cost=co, pr.auc=pr.auc$auc.integral)
    lr_matrix <- rbind(lr_matrix, row)
  }
}
names(lr_matrix) <- c('type', 'cost', 'pr.auc')

# BEST LOGISTIC REGRESSION

log.regression <- LiblineaR(data=train.treat[,var_names], target=train.treat[,pred_var], type=0, cost=100)
fitted_values <- predict(log.regression, valid.treat[var_names], proba=TRUE)
results <- data.frame(fitted_values[[2]][,2], valid.treat[,pred_var], valid.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')
table(fitted_values[[1]], valid.treat[,pred_var])

# SUPPORT VECTOR MACHINES

tryTypes <- c(1:5)
tryCosts <- c(1000, 100, 10, 1, .01)
svm_matrix <- data.frame()

for(ty in tryTypes){
  for(co in tryCosts){
    log.regression <- LiblineaR(data=train.treat[,var_names], target=train.treat[,pred_var], type=ty, cost=co)
    fitted_values <- predict(log.regression, valid.treat[var_names], proba=FALSE)
    results <- table(fitted_values[[1]], valid.treat[,pred_var])
    pr <- results[4] / (results[4] + results[3])
    recall <- results[4] / (results[4] + results[2])
    f_measure <- 2* pr * recall / (pr + recall)
    row <- c(ty, co, pr, recall, f_measure)
    svm_matrix <- rbind(svm_matrix, row)
  }
}
names(svm_matrix) <- c('type', 'cost', 'pr', 'recall', 'f_measure')

# BEST SVM

log.regression <- LiblineaR(data=train.treat[,var_names], target=train.treat[,pred_var], type=1, cost=1)
fitted_values <- predict(log.regression, valid.treat[var_names], proba=FALSE)
results <- data.frame(fitted_values[[1]], valid.treat[,pred_var], valid.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')
table(fitted_values[[1]], valid.treat[,pred_var])

# RUN RANDOM FOREST

train.treat$install_date <- as.Date(train.treat$install_date, origin = "1970-01-01")
valid.treat$install_date <- as.Date(valid.treat$install_date, origin = "1970-01-01")
test.treat$install_date <- as.Date(test.treat$install_date, origin = "1970-01-01")

set.seed(21)
n.positive <- length(which(train.treat[,pred_var]==1))
n.negative <- length(which(train.treat[,pred_var]==0))
print(n.positive/nrow(train.treat))

model_name <- paste(parsed_name, 'rf', 'v1', sep='_')

rf1 <- randomForest(factor_formula, train.treat, importance=TRUE, ntree=200, proximity=FALSE, do.trace = TRUE)
rf2 <- randomForest(factor_formula, train.treat, importance=TRUE, ntree=200, proximity = FALSE, do.trace = TRUE, sampsize = c(n.positive, n.positive), strata=train.treat[,pred_var])
rf3 <- randomForest(factor_formula, train.treat, importance=TRUE, ntree=200, proximity = FALSE, do.trace = TRUE, sampsize = c(n.positive*2, n.positive), strata=train.treat[,pred_var])
rf4 <- randomForest(factor_formula, train.treat, importance=TRUE, ntree=200, proximity = FALSE, do.trace = TRUE, sampsize = c(n.positive*3, n.positive), strata=train.treat[,pred_var])
# rf5 <- randomForest(model_formula, train.treat, importance=TRUE, ntree=200, proximity = FALSE, do.trace = TRUE, sampsize = c(n.ret, n.ret), strata=ret)
nrow(train.treat)
rf1
saveModel(rf1, name=model_name, overwrite=TRUE)
# rf1 <- loadModel(name=model_name)
varImpPlot(rf1)
varImp(rf1)
# partialPlot(rf1, train.treat, x.var='sessioncountpast', which.class=1)

# OOB RESULTS

results <- data.frame(rf1$votes[,2], train.treat[,pred_var], train.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')

# CV Results

fitted_values <- predict(rf1, valid.treat, type = 'prob')
results <- data.frame(fitted_values[,2], valid.treat[,pred_var], valid.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')

# TEST Results

fitted_values <- predict(rf1, test.treat, type = 'prob')
results <- data.frame(fitted_values[,2], test.treat[,pred_var], test.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')

# Graphs

# par(mfrow=c(2,2),mar=c(4,4,3,3))
# plot.separation(rf1,main="no stratification")
# plot.separation(rf2,main="1:5")
# plot.separation(rf3,main="1:2")
# plot.separation(rf4,main="1:1")

results$pred <- ifelse(results$prob >= .1, 1, 0)
table(results$actual, results$pred)

par(mfrow=c(1,1))

# normal roc curve
plot_auc(results)

plot(roc(rf2$votes[,2],results[,'actual']),col=2,add=T)
plot(roc(rf3$votes[,2],results[,'actual']),col=3,add=T)
plot(roc(rf4$votes[,2],results[,'actual']),col=4,add=T)
plot(roc(rf5$votes[,2],results[,'actual']),col=5,add=T)

# normal pr curve
plot_pr(results)

plot(performance(prediction(rf2$votes[,2], results[,'actual']), 'prec', 'rec'), col=2, add=TRUE)
plot(performance(prediction(rf3$votes[,2], results[,'actual']), 'prec', 'rec'), col=3, add=TRUE)
plot(performance(prediction(rf4$votes[,2], results[,'actual']), 'prec', 'rec'), col=4, add=TRUE)
plot(performance(prediction(rf5$votes[,2], results[,'actual']), 'prec', 'rec'), col=5, add=TRUE)

# cutoff colored graphs
plot_auc_color(results, ggplot=FALSE)
plot_auc_color(results, ggplot=TRUE)

plot_pr_color(results, ggplot=FALSE)
plot_pr_color(results, ggplot=TRUE)



# recall min and cutoff

recall.min <- .8
min.index <- min(which(slot(RP.perf, 'x.values')[[1]] > recall.min))
cutoff <- slot(RP.perf, 'alpha.values')[[1]][min.index]
cutoff
print(paste0('Precision per ', recall.min, ' recall: ', format(slot(RP.perf, 'y.values')[[1]][min.index], digits=4)))

# MAE, RMSE, MAPE

regr.eval(results[,'actual_num'], results[,'prob'], stats = c('mae', 'rmse', 'mape'))

# GBM

train.treat$install_date <- as.numeric(train.treat$install_date)
valid.treat$install_date <- as.numeric(valid.treat$install_date)
test.treat$install_date <- as.numeric(test.treat$install_date)

model_name <- paste(parsed_name, 'gbm', 'v1', sep='_')

n.trees = 2000
gbm1 <- gbm(num_formula, data=train.treat, distribution='bernoulli', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
if(n.trees < gbm.perf(gbm1)*1.2){
    gbm.sessions <- gbm.more(gbm1, n.new.trees=(gbm.perf(gbm1)*1.2 - n.trees))
    n.trees = gbm.perf(gbm1)*1.2
}
# gbm1 <- gbm.more(gbm1, n.new.trees=1000)
saveModel(gbm1, name=model_name, overwrite=FALSE)

best.iter <- gbm.perf(gbm1, method='OOB')
best.iter
par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance')
var_imp <- subset(var_imp, rel.inf != 0)
var_imp <- var_imp %>% arrange(desc(rel.inf))
var_imp$var <- factor(var_imp$var, levels=rev(var_imp$var))
ggplot(var_imp, aes(var, rel.inf)) +
    geom_bar(stat='identity', fill='blue') +
    coord_flip() +
    ggtitle('GBM Variable Importance') +
    labs(x ='Variable', y = 'Relative Influence') +
    theme(plot.title = element_text(hjust=.5))

print(best.iter)
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))

par(par.default)
for(i in 1:length(gbm1$var.names)){
  plot(gbm1, i.var=i, n.trees=best.iter, type='response')
}

# In Sample Results

gbm_predict <- predict(gbm1, train.treat, n.trees=best.iter, type='response')
results <- data.frame(gbm_predict, train.treat[,pred_var], train.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')
results[,pred_var] <- ifelse(results$prob > .2, 1, 0)
table(results$actual, results[,pred_var])

roc2 <- roc(results[,'prob'], results[,'actual'])
plot(roc2, main="CV ROC curves for Churn")
mtext(paste('AUC: ', format(AUC::auc(roc2), digits=4), sep=''))
grid()
AUC::auc(roc2)

pred <- prediction(results[,'prob'], results[,'actual'])
PR.d7churn <- performance(pred, "prec", "rec")
pr.auc <- pr.curve(scores.class0 = results[,'prob'], weights.class0 = results[,'actual_num'])
pr.auc
plot(PR.d7churn, main = 'CV Precision-Recall Curve')
mtext(paste('PR AUC: ', format(pr.auc$auc.integral, digits=4), sep=''))
grid()

# CV Results
par(par.default)

gbm_predict <- predict(gbm1, valid.treat, n.trees=best.iter, type='response')
results <- data.frame(gbm_predict, valid.treat[,pred_var], valid.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')
results[,pred_var] <- ifelse(results$prob > .2, 1, 0)
table(results$actual, results[,pred_var])

roc2 <- roc(results[,'prob'], results[,'actual'])
plot(roc2, main="CV ROC curves for Churn")
mtext(paste('AUC: ', format(AUC::auc(roc2), digits=4), sep=''))
grid()
AUC::auc(roc2)

pred <- prediction(results[,'prob'], results[,'actual'])
RP.perf <- performance(pred, "prec", "rec")
pr.auc <- pr.curve(scores.class0 = results[,'prob'], weights.class0 = results[,'actual_num'])
pr.auc
plot(RP.perf, main = 'CV Precision-Recall Curve')
mtext(paste('PR AUC: ', format(pr.auc$auc.integral, digits=4), sep=''))
grid()

recall.min <- .8
min.index <- min(which(slot(RP.perf, 'x.values')[[1]] > recall.min))
cutoff <- slot(RP.perf, 'alpha.values')[[1]][min.index]
cutoff
print(paste0('Precision per ', recall.min, ' recall: ', format(slot(RP.perf, 'y.values')[[1]][min.index], digits=4)))

regr.eval(results[,'actual_num'], results[,'prob'], stats = c('mae', 'rmse', 'mape'))

# Logistic Regression

ret.fit <- glm(model_formula, family=binomial(link = 'logit'), data=train.treat)
saveModel(ret.fit, name='d7churn_prediction_glm_v1')
summary(ret.fit)
anova(ret.fit, test='Chisq')

glm_predict <- predict(ret.fit, valid.treat, type='response')
results <- data.frame(glm_predict, valid.treat[,pred_var], valid.treat[,pred_var_num])
colnames(results) <- c('prob', 'actual', 'actual_num')

results[,pred_var] <- ifelse(results$prob > .2, 1, 0)
table(results$actual, results[,pred_var])

roc2 <- roc(results[,'prob'], results[,'actual'])
plot(roc2, main="CV ROC curves for Churn")
mtext(paste('AUC: ', format(AUC::auc(roc2), digits=4), sep=''))
grid()
AUC::auc(roc2)

pred <- prediction(results[,'prob'], results[,'actual'])
RP.perf <- performance(pred, "prec", "rec")
pr.auc <- pr.curve(scores.class0 = results[,'prob'], weights.class0 = results[,'actual_num'])
pr.auc
plot(RP.perf, main = 'CV Precision-Recall Curve')
mtext(paste('PR AUC: ', format(pr.auc$auc.integral, digits=4), sep=''))
grid()


recall.min <- .5
min.index <- min(which(slot(RP.perf, 'x.values')[[1]] > recall.min))
cutoff <- slot(RP.perf, 'alpha.values')[[1]][min.index]
cutoff
print(paste0('Precision per ', recall.min, ' recall: ', format(slot(RP.perf, 'y.values')[[1]][min.index], digits=4)))

regr.eval(results[,'actual_num'], results[,'prob'], stats = c('mae', 'rmse', 'mape'))








