# By: James Tan

# Date: 5/3/2018

p_load(dplyr, readr, ggplot2, reshape2, tseries, lmtest, vtreat, caret, randomForest, gbm, ggbiplot, ggalt, gridExtra, lubridate)


source('~/.Rprofile')
setwd(datapath)

mydata <- read.csv('jan_feb_2000_bievents_2018_5_22.csv')
saveRDS(mydata, file='jan_feb_2000_bievents_2018_5_22.RData')

mydata <- readRDS(file='jan_feb_2000_bievents_2018_5_22.RData')

sapply(mydata, class)

mydata <- filter(mydata, game_id == 23)
mydata$game_id <- as.factor(mydata$game_id)
mydata$user_id <- as.factor(mydata$user_id)

mydata$date <- as.Date(mydata$metric_date)
mydata <- select(mydata, -metric_date)
mydata$first_login_ts <- as.POSIXct(mydata$first_login_ts, format='%Y-%m-%d %H:%M:%S', tz='UTC')
mydata$current_gold_spent <- as.numeric(as.character(mydata$current_gold_spent))

mydata$march_split <- factor(ifelse(mydata$rev_201803 < 2000, '<2000', '2000+'))
mydata <- select(mydata, game_id, user_id, rev_201801, rev_201802, rev_201803, rev_201804, date, revenue, everything())

saveRDS(mydata, file='jan_feb_18_bievents_2018_5_22_parsed.RData')

mydata <- readRDS(file='jan_feb_18_bievents_2018_5_22_parsed.RData')
plotname <- '_RoC.png'

mydata <- filter(mydata, date >= as.Date('2018-5-3'), date <= as.Date('2018-5-24'))
mydata <- mydata %>%
  mutate(
    date_ts = as.POSIXct(date, format='%Y-%m-%d', tz='UTC'),
    month_ts = floor_date(date_ts, unit='month'),
    month = format(date_ts, format='%Y-%m')
  )

summary(mydata)

timedata <- mydata %>%
  filter(login_count > 0) %>%
  # filter(login_count > 0, march_split==0) %>%
  group_by(date) %>%
  dplyr::summarise(
    rev = sum(revenue, na.rm=TRUE),
    arpdau = mean(revenue, na.rm=TRUE),
    # dau = length(which(login_count > 0)),
    dau = n(),
    purchases = mean(purchases, na.rm=TRUE),
    total_power = mean(total_power, na.rm=TRUE),
    user_level = mean(user_level, na.rm=TRUE),
    vip_level = mean(vip_level, na.rm=TRUE),
    lifetime_revenue = mean(lifetime_revenue, na.rm=TRUE),
    building_power = mean(building_power, na.rm=TRUE),
    troop_power = mean(troop_power, na.rm=TRUE),
    hero_power = mean(hero_power, na.rm=TRUE),
    quest_power = mean(quest_power, na.rm=TRUE),
    research_power = mean(research_power, na.rm=TRUE),
    trap_power = mean(trap_power, na.rm=TRUE),
    gold_balance = mean(gold_balance, na.rm=TRUE),
    # chips = mean(chips, na.rm=TRUE),
    # stone = mean(stone, na.rm=TRUE),
    # ore = mean(ore, na.rm=TRUE),
    # food = mean(food, na.rm=TRUE),
    # coin = mean(coin, na.rm=TRUE),
    # wood = mean(wood, na.rm=TRUE),
    lifetime_login = mean(lifetime_login, na.rm=TRUE),
    gold_spent = mean(gold_spent, na.rm=TRUE),
    craft_complete = mean(craft_complete, na.rm=TRUE),
    training_instant = mean(training_instant, na.rm=TRUE),
    research_complete = mean(research_complete, na.rm=TRUE),
    building_complete = mean(building_complete, na.rm=TRUE),
    healing_complete = mean(healing_complete, na.rm=TRUE),
    training_complete = mean(training_complete, na.rm=TRUE),
    combat = mean(combat, na.rm=TRUE),
    pve_combat = mean(pve_combat, na.rm=TRUE),
    game_event_payout = mean(game_event_payout, na.rm=TRUE),
    expert_game_event_payout = mean(expert_game_event_payout, na.rm=TRUE),
    chat_login_count = mean(chat_login_count, na.rm=TRUE),
    playminutes = mean(playminutes, na.rm=TRUE),
    login_count = mean(login_count, na.rm=TRUE),
    item_purchase_cnt = mean(item_purchase_cnt, na.rm=TRUE),
    gift_item_purchase_cnt = mean(gift_item_purchase_cnt, na.rm=TRUE),
    building_speedup = mean(building_speedup, na.rm=TRUE),
    crafting_speedup = mean(crafting_speedup, na.rm=TRUE),
    quest_speedup = mean(quest_speedup, na.rm=TRUE),
    research_speedup = mean(research_speedup, na.rm=TRUE),
    training_speedup = mean(training_speedup, na.rm=TRUE),
    attack = mean(attack, na.rm=TRUE),
    scouts = mean(scouts, na.rm=TRUE),
    attack_creature = mean(attack_creature, na.rm=TRUE),
    impression = mean(impression, na.rm=TRUE),
    alliance_power = mean(alliance_power, na.rm=TRUE),
    alliance_size = mean(alliance_size, na.rm=TRUE),
    alliance_gifts = mean(alliance_gifts, na.rm=TRUE),
    complete_empire_quest = mean(complete_empire_quest, na.rm=TRUE),
    donate_resources = mean(donate_resources, na.rm=TRUE),
    alliance_store_purchase = mean(alliance_store_purchase, na.rm=TRUE),
    alliance_store_spent = mean(alliance_store_spent, na.rm=TRUE),
    alliance_secret_gift = mean(alliance_secret_gift, na.rm=TRUE),
    global_gift = mean(global_gift, na.rm=TRUE),
    speed_dungeon = mean(speed_dungeon, na.rm=TRUE),
    wonder_dungeon = mean(wonder_dungeon, na.rm=TRUE),
    current_gold_spent = mean(current_gold_spent, na.rm=TRUE),
    subs_active = mean(subs_active, na.rm=TRUE),
    hero_level = mean(hero_level, na.rm=TRUE),
  ) %>%
  select(
    -c(gift_item_purchase_cnt, donate_resources, alliance_store_purchase, alliance_store_spent, alliance_secret_gift, wonder_dungeon)
  )

meltdata <- melt(timedata, id.vars=c('date', 'march_split'))
# meltdata <- melt(timedata, id.vars=c('date'))


p1 <- ggplot(meltdata, aes(date, value, group=march_split, color=march_split)) +
  geom_vline(xintercept=as.Date('2018-5-10'), color='green') +
  geom_vline(xintercept=as.Date('2018-5-17'), color='red') +
  geom_line() +
  facet_wrap(~variable, scales='free_y', nrow=7) +
  scale_color_brewer(type='qual', palette=3)

ggsave(paste0('all_vars', plotname),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')



changedata <- timedata %>%
  dplyr::group_by(march_split) %>%
  dplyr::mutate(
    total_power_change = total_power - dplyr::lag(total_power, n=1, order_by=date),
    user_level_change = user_level - lag(user_level, n=1, order_by=date),
    vip_level_change = vip_level - lag(vip_level, n=1, order_by=date),
    lifetime_revenue_change = lifetime_revenue - lag(lifetime_revenue, n=1, order_by=date),
    building_power_change = building_power - lag(building_power, n=1, order_by=date),
    troop_power_change = troop_power - lag(troop_power, n=1, order_by=date),
    hero_power_change = hero_power - lag(hero_power, n=1, order_by=date),
    quest_power_change = quest_power - lag(quest_power, n=1, order_by=date),
    research_power_change = research_power - lag(research_power, n=1, order_by=date),
    trap_power_change = trap_power - lag(trap_power, n=1, order_by=date),
    gold_balance_change = gold_balance - lag(gold_balance, n=1, order_by=date),
    lifetime_login_change = lifetime_login - lag(lifetime_login, n=1, order_by=date),
    gold_spent_change = gold_spent - lag(gold_spent, n=1, order_by=date),
    alliance_power_change = alliance_power - lag(alliance_power, n=1, order_by=date),
    hero_level_change = hero_level - lag(hero_level, n=1, order_by=date),
  ) %>%
  select(
    -c(total_power, user_level, vip_level, lifetime_revenue, building_power, troop_power, hero_power, quest_power, research_power, trap_power, gold_balance, lifetime_login, gold_spent, alliance_power, hero_level, impression)
  )

meltdata <- melt(changedata, id.vars=c('date', 'march_split'))

p1 <- ggplot(meltdata, aes(date, value, group=march_split, color=march_split)) +
  geom_vline(xintercept=as.Date('2018-5-10'), color='green') +
  geom_vline(xintercept=as.Date('2018-5-17'), color='red') +
  geom_line() +
  facet_wrap(~variable, scales='free_y', nrow=7) +
  scale_color_brewer(type='qual', palette=3)

ggsave(paste0('change_vars', plotname),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')


# Cointegration Test

changedata <- filter(changedata, march_split == levels(march_split)[1])

results <- data.frame(variable=character(), p_autolag=numeric(), p_lag1=numeric())

for(i in 5:ncol(changedata)){
  name <- colnames(changedata)[i]
  formula <- as.formula(paste0('arpdau ~ ', name))
  lm_temp <- lm(formula, changedata)
  p_autolag <- adf.test(lm_temp$residuals)$p.value
  p_lag1 <- adf.test(lm_temp$residuals, k=1)$p.value
  results_temp <- data.frame(name, p_autolag, p_lag1)
  results <- rbind(results, results_temp)
}

results <- arrange(results, p_autolag)
plotdata <- melt(results, id.vars = 'name')
plotlevels <- plotdata %>% filter(variable == 'p_autolag') %>% arrange(value) %>% pull(name)
plotdata$name <- factor(plotdata$name, levels=rev(plotlevels))


corrmatrix <- corrmatrix[,rownames(corrmatrix)]

ggplot(plotdata, aes(variable, name, label=format(round(value, 3), nsmall=3))) +
  geom_tile(alpha=0) +
  geom_text(size=2) +
  theme(axis.text.x = element_text(angle=75, vjust=.7),
        axis.title.x=element_text(margin=ggplot2::margin(-30,0,0,0)),
        axis.title.y=element_text(margin=ggplot2::margin(0,-10,0,0)))

results_export <- results %>% select(name, p_autolag)
write.csv(results_export, 'cointegration_scores.csv', row.names=FALSE)

ggplot(changedata, aes(date, group=march_split, color=march_split)) +
  geom_line(aes(y=arpdau, color='arpdau')) +
  geom_line(aes(y=user_level_change*300, color='user_level_change')) +
  labs(title = 'ARPDAU and user_level_change/DAU') +
  theme(plot.title = element_text(hjust=.5))

ggplot(changedata, aes(date)) +
  geom_line(aes(y=arpdau, color='arpdau')) +
  geom_line(aes(y=building_speedup*50, color='building_speedup')) +
  labs(title = 'ARPDAU and Building Speedup/DAU*50') +
  theme(plot.title = element_text(hjust=.5))

ggplot(changedata, aes(date)) +
  geom_line(aes(y=arpdau, color='arpdau')) +
  geom_line(aes(y=complete_empire_quest*20, color='complete_empire_quest')) +
  labs(title = 'ARPDAU and complete_empire_quest') +
  theme(plot.title = element_text(hjust=.5))

ggsave(paste0('arpdau_complete_empire_quest', plotname),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

ggplot(changedata, aes(date)) +
  geom_line(aes(y=arpdau, color='arpdau')) +
  geom_line(aes(y=gold_spent_change/1e05, color='gold_spent_change')) +
  labs(title = 'ARPDAU and Gold Spent Change/10,000') +
  theme(plot.title = element_text(hjust=.5))

ggplot(changedata, aes(date)) +
  geom_line(aes(y=arpdau, color='arpdau')) +
  geom_line(aes(y=alliance_gifts*5, color='alliance_gifts')) +
  labs(title = 'ARPDAU and Alliance Gifts/DAU*5') +
  theme(plot.title = element_text(hjust=.5))

ggplot(changedata, aes(date)) +
  geom_line(aes(y=building_complete, color='completes/dau')) +
  geom_line(aes(y=building_speedup, color='speedups/dau')) +
  labs(title = 'Building Completes vs Speedups') +
  theme(plot.title = element_text(hjust=.5))

topdata <- changedata %>%
  select(date, march_split, arpdau, speed_dungeon, building_speedup, chat_login_count, complete_empire_quest, research_speedup, alliance_gifts)

meltdata <- melt(topdata, id.vars=c('date', 'march_split'))

ggplot(meltdata, aes(date, value)) +
  geom_line() +
  facet_wrap(~variable, scales='free_y', ncol=2)

names <- c()
for(i in 1:5){
  names <- c(names, paste0('p_order', i))
}
names <- c('variable', names)
results_granger <- data.frame(matrix(ncol=6, nrow=(ncol(changedata) - 5 + 1)))
names(results_granger) <- names

for(i in 5:ncol(changedata)){
  name <- colnames(changedata)[i]
  formula <- as.formula(paste0('arpdau ~ ', name))
  pvalues <- c()
  for(j in 1:5){
    p <- grangertest(formula, changedata, order=j)$`Pr(>F)`[2]
    pvalues <- c(pvalues, p)
  }
  results_temp <- list(name)
  results_temp <- append(results_temp, pvalues)
  results_granger[i-4,] <- results_temp
}

results_granger <- results_granger %>%
  arrange(p_order1)

write.csv(results_granger, 'granger_causality_scores_RoC.csv', row.names=FALSE)

diffdata <- changedata %>%
  ungroup() %>%
  select(-c(date, march_split, rev))
diffdata <- data.frame(sapply(diffdata, function(x) diff(x, lag=1)))

names <- c()
for(i in 1:5){
  names <- c(names, paste0('diff_p_order', i))
}
names <- c('variable', names)
results_granger_diff <- data.frame(matrix(ncol=6, nrow=(ncol(diffdata) - 1)))
names(results_granger_diff) <- names

for(i in 2:ncol(diffdata)){
  name <- colnames(diffdata)[i]
  formula <- as.formula(paste0('arpdau ~ ', name))
  pvalues <- c()
  for(j in 1:5){
    p <- grangertest(formula, diffdata, order=j)$`Pr(>F)`[2]
    pvalues <- c(pvalues, p)
  }
  results_temp <- list(name)
  results_temp <- append(results_temp, pvalues)
  results_granger_diff[i-1,] <- results_temp
}

results_granger_diff <- results_granger_diff %>%
  arrange(diff_p_order1)

write.csv(results_granger_diff, 'granger_diff_causality_scores_RoC.csv', row.names=FALSE)

pred_var <- 'arpdau'

ind_vars <- names(changedata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_var))

var_names <- paste(ind_vars, collapse=' + ')

corr_method <- 'pearson'
corr_vars <- c(pred_var, ind_vars)
corr_vars <- names(changedata)[sapply(changedata, class) == 'numeric' | sapply(changedata, class) == 'integer']
corr_vars <- corr_vars[corr_vars != 'march_split']
corr_vars <- c(pred_var, corr_vars[which(corr_vars != pred_var)])
corrmatrix <- cor(changedata[,corr_vars], use='pairwise.complete.obs', method=corr_method)
corrmatrix <- corrmatrix[order(corrmatrix[,1], decreasing=TRUE),]
corrmatrix <- corrmatrix[,rownames(corrmatrix)]

ggplot(melt(corrmatrix), aes(x=Var1, y=Var2, fill=value, label=format(round(value, 2), nsmall=2))) +
    geom_tile() +
    geom_text(size=2, alpha=.5) +
    scale_fill_gradientn(colours=c('#ef8a62','#f7f7f7','#67a9cf'), limits=c(-1,1), values=c(0, .5, 1)) +
    theme(axis.text.x = element_text(angle=75, vjust=.7),
          axis.title.x=element_text(margin=ggplot2::margin(-30,0,0,0)),
          axis.title.y=element_text(margin=ggplot2::margin(0,-10,0,0)))

ggsave(paste0(corr_method, '_corr', plotname),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')


# feature importance ML

mldata <- changedata %>%
  ungroup %>%
  select(
    -c(date, march_split, rev, purchases)
  )

ind_vars <- names(mldata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c('arpdau'))

pred_var <- 'arpdau'
var_names <- paste(ind_vars, collapse=' + ')

model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))

measure_vars <- c()


# spec = c(train = .8, validate = .2, test = 0)
# spec = c(treat = .2, train = .7, validate = .1)
spec = c(train = .9, validate = .1)

g = sample(cut(
  seq(nrow(mldata)),
  nrow(mldata)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(mldata, g)

treatdata <- res$treat
traindata <- res$train
validdata <- res$validate
testdata <- res$test

saveRDS(treatdata, file='arpdau_timeseries_treatdata_v1')
saveRDS(traindata, file='arpdau_timeseries_traindata_v1')
saveRDS(validdata, file='arpdau_timeseries_validdata_v1')

treatdata <- readRDS(file='arpdau_timeseries_treatdata_v1')
traindata <- readRDS(file='arpdau_timeseries_traindata_v1')
validdata <- readRDS(file='arpdau_timeseries_validdata_v1')




factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))
num_formula <- as.formula(paste(pred_var_num, ' ~ ', paste(var_names, collapse=" + "), sep=''))

preprocessParams <- preProcess(traindata[,ind_vars], method=c('scale', 'center'), na.remove=TRUE)
print(preprocessParams)
traindata_transformed <- predict(preprocessParams, traindata)

# random forest

control <- trainControl(method="cv", number=10)

fit.rf <- train(model_formula, data=traindata_transformed, method="rf", metric=pred_var, trControl=control, na.action=na.omit)

rf1 <- fit.rf$finalModel

png(paste0('../Graphs/rf_varimp', plotname), width=1600, height=900, res=320)
varImpPlot(rf1)
dev.off()



varImp(rf1)


# GBM

model_name <- paste(parsed_name, 'gbm', 'v1', sep='_')

n.trees = 2000
gbm1 <- gbm(model_formula, data=traindata_transformed, distribution='gaussian', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
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


# PCA


pcadata <- changedata

pcadata <- pcadata[complete.cases(pcadata),]
pca <- prcomp(pcadata[,ind_vars], center=TRUE, scale=TRUE, na.action= na.omit)

print(pca)
plot(pca, type='l')
summary(pca)

write.csv(pca$rotation, 'pca_matrix.csv', row.names=TRUE)

ggbiplot(pca, groups=pcadata$march_split)


ggbiplot(pca, choices=c(1, 2), obs.scale = 1, var.scale = 1, ellipse = TRUE, groups=pcadata$march_split,
         circle = FALSE) +
  labs(title='PCA of BI Event Time Series Grouped By March Split',
       caption='Users that spent 2000+ in Jan and Feb') +
  theme(plot.title = element_text(hjust=.5))

rot <- pca$rotation
rot <- data.frame(names=rownames(rot), rot)
ggplot(rot, aes(x=names, y=PC1, group=1)) +
  geom_line() +
  labs(title='Principle Component 2') +
  theme(plot.title = element_text(hjust=.5),
        axis.text.x = element_text(angle=75, vjust=.7))

df_pc <- data.frame(pca$x, march_split=pcadata$march_split)
df_pc_2000_plus <- filter(df_pc, march_split == levels(march_split)[2])
df_pc_2000_less <- filter(df_pc, march_split == levels(march_split)[1])
ggplot(df_pc, aes(PC1, PC2, col=march_split)) +
  geom_point(aes(shape=march_split), size=2) +   # draw points
  labs(title="User Spend PCA",
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Users that spent 2000+ in Jan & Feb") +
  coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)),
                  ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_2000_plus, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_2000_less, aes(x=PC1, y=PC2))



pc2data <- select(rot, names, PC2) %>% arrange(desc(PC2))
pc2 <- filter(pc2data, PC2 > 0)
pc2less <- filter(pc2data, PC2 < 0) %>% arrange(PC2)

pc2table <- cbindPad(pc2, pc2less)
pc2table <- pc2table[1:15,]
dev.off()
grid.table(pc2table)


df_pc <- data.frame(pca$x, march_split=)

write.csv(changedata, 'time_series_data.csv', row.names=FALSE)




data(iris)
iris


monthdata <- mydata %>%
  filter(login_count > 0) %>%
  # filter(login_count > 0, march_split==0) %>%
  group_by(month, march_split) %>%
  dplyr::summarise(
    rev = sum(revenue, na.rm=TRUE),
    arpdau = mean(revenue, na.rm=TRUE),
    # dau = length(which(login_count > 0)),
    dau = n(),
    purchases = mean(purchases, na.rm=TRUE),
    total_power = mean(total_power, na.rm=TRUE),
    user_level = mean(user_level, na.rm=TRUE),
    vip_level = mean(vip_level, na.rm=TRUE),
    lifetime_revenue = mean(lifetime_revenue, na.rm=TRUE),
    building_power = mean(building_power, na.rm=TRUE),
    troop_power = mean(troop_power, na.rm=TRUE),
    hero_power = mean(hero_power, na.rm=TRUE),
    quest_power = mean(quest_power, na.rm=TRUE),
    research_power = mean(research_power, na.rm=TRUE),
    trap_power = mean(trap_power, na.rm=TRUE),
    gold_balance = mean(gold_balance, na.rm=TRUE),
    # chips = mean(chips, na.rm=TRUE),
    # stone = mean(stone, na.rm=TRUE),
    # ore = mean(ore, na.rm=TRUE),
    # food = mean(food, na.rm=TRUE),
    # coin = mean(coin, na.rm=TRUE),
    # wood = mean(wood, na.rm=TRUE),
    lifetime_login = mean(lifetime_login, na.rm=TRUE),
    gold_spent = mean(gold_spent, na.rm=TRUE),
    craft_complete = mean(craft_complete, na.rm=TRUE),
    training_instant = mean(training_instant, na.rm=TRUE),
    research_complete = mean(research_complete, na.rm=TRUE),
    building_complete = mean(building_complete, na.rm=TRUE),
    healing_complete = mean(healing_complete, na.rm=TRUE),
    training_complete = mean(training_complete, na.rm=TRUE),
    combat = mean(combat, na.rm=TRUE),
    pve_combat = mean(pve_combat, na.rm=TRUE),
    game_event_payout = mean(game_event_payout, na.rm=TRUE),
    expert_game_event_payout = mean(expert_game_event_payout, na.rm=TRUE),
    chat_login_count = mean(chat_login_count, na.rm=TRUE),
    playminutes = mean(playminutes, na.rm=TRUE),
    login_count = mean(login_count, na.rm=TRUE),
    item_purchase_cnt = mean(item_purchase_cnt, na.rm=TRUE),
    gift_item_purchase_cnt = mean(gift_item_purchase_cnt, na.rm=TRUE),
    building_speedup = mean(building_speedup, na.rm=TRUE),
    crafting_speedup = mean(crafting_speedup, na.rm=TRUE),
    quest_speedup = mean(quest_speedup, na.rm=TRUE),
    research_speedup = mean(research_speedup, na.rm=TRUE),
    training_speedup = mean(training_speedup, na.rm=TRUE),
    attack = mean(attack, na.rm=TRUE),
    scouts = mean(scouts, na.rm=TRUE),
    attack_creature = mean(attack_creature, na.rm=TRUE),
    impression = mean(impression, na.rm=TRUE),
    alliance_power = mean(alliance_power, na.rm=TRUE),
    alliance_size = mean(alliance_size, na.rm=TRUE),
    alliance_gifts = mean(alliance_gifts, na.rm=TRUE),
    complete_empire_quest = mean(complete_empire_quest, na.rm=TRUE),
    donate_resources = mean(donate_resources, na.rm=TRUE),
    alliance_store_purchase = mean(alliance_store_purchase, na.rm=TRUE),
    alliance_store_spent = mean(alliance_store_spent, na.rm=TRUE),
    alliance_secret_gift = mean(alliance_secret_gift, na.rm=TRUE),
    global_gift = mean(global_gift, na.rm=TRUE),
    speed_dungeon = mean(speed_dungeon, na.rm=TRUE),
    wonder_dungeon = mean(wonder_dungeon, na.rm=TRUE),
    current_gold_spent = mean(current_gold_spent, na.rm=TRUE),
    subs_active = mean(subs_active, na.rm=TRUE),
    hero_level = mean(hero_level, na.rm=TRUE),
  ) %>%
  select(
    -c(gift_item_purchase_cnt, donate_resources, alliance_store_purchase, alliance_store_spent, alliance_secret_gift, wonder_dungeon)
  )

meltdata <- melt(monthdata, id.vars=c('month', 'march_split'))
# meltdata <- melt(timedata, id.vars=c('date'))


p1 <- ggplot(meltdata, aes(month, value, group=march_split, color=march_split)) +
  # geom_vline(xintercept=as.Date('2018-5-10'), color='green') +
  # geom_vline(xintercept=as.Date('2018-5-17'), color='red') +
  geom_line() +
  facet_wrap(~variable, scales='free_y', nrow=7) +
  scale_color_brewer(type='qual', palette=3)
p1
ggsave(paste0('all_vars', plotname),
       path = '../Graphs',
       plot = last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

topdata <- monthdata %>%
  select(month, march_split, arpdau, craft_complete, training_complete, chat_login_count)

meltdata <- melt(topdata, id.vars=c('month', 'march_split'))

ggplot(meltdata, aes(month, value, group=march_split, color=march_split)) +
  geom_line() +
  labs(title = 'High Spender Behavior', subtitle='Includes Players who spent >2000 in January and February') +
  facet_wrap(~variable, scales='free_y', ncol=2)

