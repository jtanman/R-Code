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
library(mlbench)

set.seed(21)
par.default <- par()

source('~/.Rprofile')
library(rutils)
setwd(datapath)

# mydata <- loadData('dragons_churn_d2_d7_data_2017.12.5')
# mydata$install_date <- as.Date(mydata$install_date)
# mydata$run_date <- as.Date(mydata$run_date)
# userdata <- loadData('dragons_user_data_2017.12.5')
# userdata$install_date <- as.Date(userdata$install_date)

# mydata <- merge(userdata, mydata, by=c('udid', 'install_ts', 'install_date'))

# nacolumns <- apply(mydata, 2, function(x) length(which(is.na(x))))
# names(which(nacolumns > 0))

# mydata <- mydata[which(mydata$hw_type != 'post-gamestate-tool'),]
# # mydata$install_date <- as.Date(mydata$install_date)
# # mydata$lang <- as.factor(mydata$lang)
# # mydata$ip_country <- as.factor(mydata$ip_country)
# # mydata$hw_type <- as.factor(mydata$hw_type)
# # mydata$hw_ver <- as.factor(mydata$hw_ver)
# # mydata$os_ver <- as.factor(mydata$os_ver)
# # mydata$device_type <- as.factor(mydata$device_type)
# # mydata$device_lang <- as.factor(mydata$device_lang)

# mydata <- mydata[!names(mydata) %in% c('guildapplied', 'guildexited')]

# keepNAcolumns <- c('startups', 'startup_times', 'first_startup')
# keepData <- mydata[,keepNAcolumns]
# mydata[is.na(mydata)] <- 0
# for(c in keepNAcolumns){
#     mydata[,c] <- keepData[,c]
# }
# rm(keepData)

# mydata[which(mydata$gametime < 0), 'gametime'] <- NA

measure_day <- 7
data_day <- 2

parsed_name <- paste0('dragons_churn_d', data_day, '_d', measure_day, '_2017.12.5_parsed')

# saveData(mydata, name=parsed_name, overwrite=TRUE)

mydata <- loadData(name=parsed_name)

# FEATURE ENGINEERING


mydata <- mydata %>%
    mutate(
        avg_startup = startup_times / startups,
        sessions_per_startup = sessions / startups,
        sessions_per_day = sessions / sessiondays,
        avg_session_time = gametime / sessions,
        crashes_per_gametime = numcrashes / gametime,
        atkacceptrate = attacksaccepted / attackinvites,
        dfacceptrate = defendsaccepted / defendinvites,
        atkfollowrate = follows / attacks,
        defassistrate = defenseassists / defenses,
        assist_diff = attacksaccepted - follows,
        def_diff = defendsaccepted - defenseassists
    )


mydata$d7ret.factor <- as.factor(mydata$d7ret)
mydata <- mydata %>%
    select(-c(udid, install_ts, install_date, country, gae_country, run_date, d7sessions, d7rev, d7rev_count))

mydata <- mydata %>%
    select(-c(device_type, device_lang, time_zone, numnullchats, attackinvites, defendinvites,
              sessions, sessions_per_day, breeding_tokens_dragon_autobreed_count, gametime,
              numpiercgiftsent, daily_tokens_collected_count, sumpiercgiftrec, sumpiercgiftsent, sumfoodgiftsent, ))

version_name <- paste0(parsed_name, '_v3')

# saveData(mydata,name=version_name, overwrite=TRUE)

mydata <- loadData(name=version_name)

# imputedata <- mydata[,!(colnames(mydata) %in% c('trophy'))]
# aggr_plot <- aggr(imputedata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydata), 
#   cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

# installdata <- ddply(mydata, .(install_date), summarise,
#                      d3churn = mean(d3churn)
# )

pred_var <- 'd7ret.factor'
pred_var_num <- 'd7ret'

ind_vars <- names(mydata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_var, pred_var_num))

var_names <- paste(ind_vars, collapse=' + ')

model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))
gbm_formula <- as.formula(paste(pred_var_num, ' ~ ', var_names, sep=''))

measure_vars <- c()


corr_vars <- c(pred_var, ind_vars)
corr_vars <- unique(c(pred_var_num, names(mydata)[sapply(mydata, class) == 'numeric' | sapply(mydata, class) == 'integer']))
corrmatrix <- cor(mydata[,corr_vars], use='pairwise.complete.obs')
corrmatrix <- corrmatrix[order(corrmatrix[,1], decreasing=TRUE),]
corrmatrix <- corrmatrix[,rownames(corrmatrix)]

corrmatrix[which(corrmatrix > .8, arr.ind= TRUE)]
which(corrmatrix > .8 & corrmatrix != 1, arr.ind= TRUE)
colnames(corrmatrix)[5]
rownames(corrmatrix)[6]

ggplot(melt(corrmatrix), aes(x=Var1, y=Var2, fill=value, label=format(round(value, 2), nsmall=2))) +
    geom_tile() +
    geom_text(size=2, alpha=.3) + 
    scale_fill_gradientn(colours=c('#ef8a62','#f7f7f7','#67a9cf'), limits=c(-1,1), values=c(0, .5, 1)) +
    theme(axis.text.x = element_text(angle=75, vjust=.9),
          axis.title.x=element_text(margin=ggplot2::margin(-30,0,0,0)),
          axis.title.y=element_text(margin=ggplot2::margin(0,-10,0,0)))




# Partition into training, validation, testing set

#spec = c(treat = .3, train = .5, validate = .2, test = 0)
spec = c(treat = .3, train = .5, validate = .2)
# spec = c(train = .2, test = .3, validate = .3)

g = sample(cut(
  seq(nrow(mydata)),
  nrow(mydata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(mydata, g)

treatdata <- res$treat
traindata <- res$train
validdata <- res$validate
testdata <- res$test

# saveData(res, name=paste(version_name, 'labeldata', sep='_'), overwrite=TRUE)
saveData(treatdata, name=paste(version_name, 'treatdata', sep='_'), overwrite=TRUE)
saveData(traindata, name=paste(version_name, 'traindata', sep='_'), overwrite=TRUE)
saveData(validdata, name=paste(version_name, 'validdata', sep='_'), overwrite=TRUE)
saveData(testdata, name=paste(version_name, 'testdata', sep='_'), overwrite=TRUE)

# res <- loadData(name=paste(version_name, 'labeldata', sep='_'))
treatdata <- loadData(name=paste(version_name, 'treatdata', sep='_'))
traindata <- loadData(name=paste(version_name, 'traindata', sep='_'))
validdata <- loadData(name=paste(version_name, 'validdata', sep='_'))
testdata <- loadData(name=paste(version_name, 'testdata', sep='_'))


# TREAT DATA

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameCExperiment(traindata, varlist = ind_vars, outcomename = pred_var, outcometarget = 1,
                               scale=TRUE,
                               rareCount=.001*nrow(traindata),  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)
treatment_plan_factor <- cfe$treatments
sf <- treatment_plan_factor$scoreFrame
var_names <- sf$varName[sf$sig <= 1/nrow(sf)]
train.treat <- cfe$crossFrame

# use separate data to treat and train

treatment_plan_factor <- designTreatmentsC(treatdata, varlist = ind_vars, outcomename = pred_var, outcometarget = 1,
                                        rareCount=.001*nrow(treatdata),  # Note set this to something larger, like 5
                                        rareSig=.3 # Note set this to something like 0.3
)

treatment_name <- paste(version_name, 'treatment', 'plan', sep='_')
# saveData(treatment_plan_factor, name=treatment_name, overwrite=TRUE)
treatment_plan_factor <- loadData(name=treatment_name)

PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame) * 100)
treatment_plan_factor$scoreFrame %>% arrange(sig)

num_vars = length(which(treatment_plan_factor$scoreFrame$sig < PRUNE_SIG))
num_vars

train.treat <- prepare(treatment_plan_factor, traindata, scale=TRUE, pruneSig=PRUNE_SIG)
var_names <- setdiff(colnames(train.treat), c(pred_var, pred_var_num))

# treat validation and testing set



valid.treat <- prepare(treatment_plan_factor, validdata, scale=TRUE, pruneSig=PRUNE_SIG)
test.treat <- prepare(treatment_plan_factor, testdata, scale=TRUE, pruneSig=PRUNE_SIG)
train.treat[,pred_var_num] <- traindata[,pred_var_num]
valid.treat[,pred_var_num] <- validdata[,pred_var_num]
test.treat[,pred_var_num] <- testdata[,pred_var_num]

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))
num_formula <- as.formula(paste(pred_var_num, ' ~ ', paste(var_names, collapse=" + "), sep=''))

smalltrain <- train.treat[sample(1:nrow(train.treat), 10000),]
smallvalid <- valid.treat[sample(1:nrow(valid.treat), 10000),]

train.treat.csv <- smalltrain %>%
    select(-d7ret.factor)
valid.treat.csv <- smallvalid %>%
    select(-d7ret.factor)

# FEATURE SELECTION

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(smalltrain[,var_names], smalltrain[,pred_var], sizes=c(100:114), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

saveData(results, name=paste0(version_name, '_rfe'))
results <- loadData(paste0(version_name, '_rfe'))

var_names[!var_names %in% predictors(results)]

var_names <- predictors(results)

treatment_plan_factor$scoreFrame %>%
    filter(varName %in% var_names) %>%
    arrange(sig)

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))
num_formula <- as.formula(paste(pred_var_num, ' ~ ', paste(var_names, collapse=" + "), sep=''))

train.treat.csv <- train.treat.csv %>%
    select(var_names, d7ret) %>%
    rename(class = d7ret)
valid.treat.csv <- valid.treat.csv %>%
    select(var_names, d7ret) %>%
    rename(class = d7ret)


write.csv(train.treat.csv, file=paste0(version_name, '_train_small.csv'), row.names=FALSE)
write.csv(valid.treat.csv, file=paste0(version_name, '_valid_small.csv'), row.names=FALSE)

vaset.seed(21)



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

model_name <- paste(version_name, 'rf', 'v1', sep='_')

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

model_name <- paste(version_name, 'gbm', 'v1', sep='_')

n.trees = 2000
gbm1 <- gbm(num_formula, data=train.treat, distribution='bernoulli', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
if(n.trees < gbm.perf(gbm1)*1.2){
  gbm.sessions <- gbm.more(gbm1, n.new.trees=(gbm.perf(gbm1)*1.2 - n.trees))
  n.trees = gbm.perf(gbm1)*1.2
}
# gbm1 <- gbm.more(gbm1, n.new.trees=1000)
saveModel(gbm1, name=model_name, overwrite=FALSE)
gbm1 <- loadModel(name=model_name)

best.iter <- gbm.perf(gbm1, method='OOB')
best.iter
par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance')
var_imp <- subset(var_imp, rel.inf != 0)
var_imp <- subset(var_imp, rel.inf >= 0.05)
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








