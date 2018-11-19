# By: James Tan

# Date: 10/18/2018

p_load(rutils, dplyr, readr, ggplot2, reshape2, tseries, lmtest, vtreat, caret, randomForest, gbm, ggbiplot, ggalt, gridExtra, lubridate)

source('~/.Rprofile')
setwd(datapath)

# mydata <- read.csv('feature_importance_12h_d1_retention.csv', sep='\t')
# mydata <- mydata %>%
#   filter(
#     game_id == 23,
#     !is.na(retention_d1h)
#   )
# 
# factor_cols <- vapply(mydata, is.factor, logical(1))
# mydata[factor_cols] <- lapply(mydata[factor_cols], factor)

mkt_metrics = c('install_geo_country','install_geo_country_tier','install_device_country','install_geo_continent','install_platform','install_market_place','campaign_type','campaign_type_detail','partner','publisher','device_game_campaign_type','device_game_campaign_type_detail','device_game_traffic_subtype','device_game_partner','device_game_publisher','traffic_type','traffic_subtype','dup_account','install_os_version_detail','install_language','device_brand','device_name','device_age_day','device_release_year','payload_kb','first_login_duration_s','first_login_complete_duration_s','first_login_empire_response_s','first_login_presync_time_s','first_login_manifest_download_time_s','first_login_manifest_decode_time_s','first_login_synced_file_count','first_login_sync_error_count','install_date','install_date_hour','install_hour','install_day_of_week','install_day_of_week_type','install_event_ts_local','install_date_local','install_date_hour_local','install_hour_local','install_day_of_week_local','install_day_of_week_type_local','click_ts','time_click_to_install_s','device_install_order','device_purchase_order','device_has_game','device_active_last_7_day','idfa','attr1_client_carrier','cpu_abi','internal_free_storage','install_num','dup_account_inferred','partner_utm','partner_kochava_claims','memory_total_gb','storage_total_gb','partner_preload','click_ts_preload','hours_install_to_now','ko_matched_by','mz_matched_by','install_app_version','install_conn_type','install_carrier','install_kingdom','tutorial_id','time_install_to_first_login_hour','marketer_team','install_isogroup')
cum_rev_metrics = c('revenue_d0h','revenue_d1h','revenue_d2h','revenue_d3h','revenue_d7h','revenue_d14h','revenue_d21h','revenue_d30h','purchase_1_ts_d0h','purchase_1_ts_d1h','purchase_1_ts_d2h','purchase_1_ts_d3h','purchase_1_ts_d7h','purchase_1_ts_d14h','purchase_1_ts_d21h','purchase_1_ts_d30h','purchase_2_ts_d0h','purchase_2_ts_d1h','purchase_2_ts_d2h','purchase_2_ts_d3h','purchase_2_ts_d7h','purchase_2_ts_d14h','purchase_2_ts_d21h','purchase_2_ts_d30h','purchase_3_ts_d0h','purchase_3_ts_d1h','purchase_3_ts_d2h','purchase_3_ts_d3h','purchase_3_ts_d7h','purchase_3_ts_d14h','purchase_3_ts_d21h','purchase_3_ts_d30h','purchase_4_ts_d0h','purchase_4_ts_d1h','purchase_4_ts_d2h','purchase_4_ts_d3h','purchase_4_ts_d7h','purchase_4_ts_d14h','purchase_4_ts_d21h','purchase_4_ts_d30h','purchase_5_ts_d0h','purchase_5_ts_d1h','purchase_5_ts_d2h','purchase_5_ts_d3h','purchase_5_ts_d7h','purchase_5_ts_d14h','purchase_5_ts_d21h','purchase_5_ts_d30h','time_install_to_purchase_1_hour_d7h','time_install_to_purchase_2_hour_d7h','time_install_to_purchase_3_hour_d7h','time_install_to_purchase_4_hour_d7h','time_install_to_purchase_5_hour_d7h','purchase_1_price','purchase_2_price','purchase_3_price','purchase_4_price','purchase_5_price','retention_d1h','retention_d2h','retention_d3h','retention_d4h','retention_d5h','retention_d6h','retention_d7h')
non_metrics = c('game_id','user_id','install_event_ts','days_since_release_lr','days_since_release_ww','time_install_to_uninstall_hour','metric_ts','user_age_hours','first_purchase_flag','lifetime_revenue','random_num','row_num','device_ltr_bin')

# mydata <- mydata %>%
#   select(
#     -cum_rev_metrics,
#     -non_metrics,
#     retention_d1h
#   )
# 
# summary(mydata)

# saveRDS(mydata, 'retention_d1h_feature_importance.RData')
mydata <- readRDS('retention_d1h_feature_importance.RData')

name <- 'retention_d1h_feature_importance'
parsed_name <- paste0(name, '_parsed')

# corr plot

pred_var <- 'retention_d1h'
pred_vars <- c(pred_var)

ind_vars <- names(mydata)
ind_vars <- subset(ind_vars, ! ind_vars %in% c(pred_var))

corr_method <- 'pearson'
corr_vars <- c(pred_var, ind_vars)
corr_vars <- names(mydata)[sapply(mydata, class) == 'numeric' | sapply(mydata, class) == 'integer']
corr_vars <- c(pred_var, corr_vars[which(corr_vars != pred_var)])
corrmatrix <- cor(mydata[,corr_vars], use='pairwise.complete.obs', method=corr_method)
corrmatrix <- corrmatrix[order(corrmatrix[,1], decreasing=TRUE),]
corrmatrix <- corrmatrix[,rownames(corrmatrix)]

na_vars <- names(which(is.na(corrmatrix[1,])))
corr_vars <- corr_vars[! corr_vars %in% na_vars]
ind_vars <- ind_vars[! ind_vars %in% na_vars]
corrmatrix <- cor(mydata[,corr_vars], use='pairwise.complete.obs', method=corr_method)
corrmatrix <- corrmatrix[order(corrmatrix[,1], decreasing=TRUE),]
corrmatrix <- corrmatrix[,rownames(corrmatrix)]

p1 <- ggplot(melt(corrmatrix), aes(x=Var1, y=Var2, fill=value, label=format(round(value, 2), nsmall=2))) +
    geom_tile() +
    geom_text(size=2, alpha=.5) +
    scale_fill_gradientn(colours=c('#ef8a62','#f7f7f7','#67a9cf'), limits=c(-1,1), values=c(0, .5, 1)) +
    theme(axis.text.x = element_text(angle=75, vjust=.7),
          axis.title.x=element_text(margin=ggplot2::margin(-30,0,0,0)),
          axis.title.y=element_text(margin=ggplot2::margin(0,-10,0,0)))

p1

ggsave(paste0(corr_method, '_corr_', name, '.png'),
       path = '../Graphs',
       plot = p1, # or give ggplot object name as in myPlot,
       width = 32, height = 18,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# feature importance ML

mldata <- mydata %>%
  ungroup

# %>%
  # select(
    # -c(date, march_split, rev, purchases)
  # )

var_names <- paste(ind_vars, collapse=' + ')
model_formula <- as.formula(paste(pred_var, ' ~ ', var_names, sep=''))

traindata <- mydata

# spec = c(train = .8, validate = .2, test = 0)
spec = c(treat = .3, train = .7)
# spec = c(treat = .3, train = .7, validate = .1)
# spec = c(train = .9, validate = .1)

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


# TREAT DATA

# Use simulated out of sample methods (cross methods)

cfe <- mkCrossFrameNExperiment(traindata, varlist = ind_vars, outcomename = pred_var, #outcometarget = 1,
                               scale=TRUE,
                               rareCount=5,  # Note set this to something larger, like 5
                               rareSig=.3 # Note set this to something like 0.3)
)

treatment_plan_factor <- cfe$treatments
sf <- treatment_plan_factor$scoreFrame
var_names <- sf$varName[sf$sig <= 1/nrow(sf)]
train.treat <- cfe$crossFrame

sf %>% arrange(sig)

# use separate data to treat and train

treatment_plan_factor <- designTreatmentsN(treatdata, varlist = ind_vars, outcomename = pred_var,
                                           rareCount=5,  # Note set this to something larger, like 5
                                           rareSig=.3 # Note set this to something like 0.3
)

treatment_plan_factor$scoreFrame %>% arrange(sig)


treatment_name <- paste0(name, '_treatment_plan.RData')
saveRDS(treatment_plan_factor, treatment_name)
treatment_plan_factor <- readRDS(treatment_name)

# treat data

PRUNE_SIG = 1 / (nrow(treatment_plan_factor$scoreFrame))

train.treat <- prepare(treatment_plan_factor, traindata, scale=TRUE, pruneSig=PRUNE_SIG)
var_names <- setdiff(colnames(train.treat), c(pred_vars))

factor_formula <- as.formula(paste(pred_var, ' ~ ', paste(var_names, collapse=" + "), sep=''))
num_formula <- as.formula(paste(pred_var_num, ' ~ ', paste(var_names, collapse=" + "), sep=''))


# Scale Data

# preprocessParams <- preProcess(traindata[,ind_vars], method=c('scale', 'center'), na.remove=TRUE)
# print(preprocessParams)
# traindata_transformed <- predict(preprocessParams, traindata)

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
gbm1 <- gbm(factor_formula, data=train.treat, distribution='bernoulli', n.trees=n.trees, shrinkage=.01, interaction.depth=2, train.fraction = .8, verbose=TRUE)
if(n.trees < gbm.perf(gbm1)*1.2){
    gbm1 <- gbm.more(gbm1, n.new.trees=(gbm.perf(gbm1)*1.2 - n.trees))
    n.trees = gbm.perf(gbm1)*1.2
}
# gbm1 <- gbm.more(gbm1, n.new.trees=1000)
saveModel(gbm1, name=model_name, overwrite=FALSE)

best.iter <- gbm.perf(gbm.sessions, method='OOB')
best.iter
# par(mar=c(3,15,3,3))
var_imp <- summary(gbm1, n.trees=best.iter, las=1, main = 'GBM Var Importance')
var_imp <- filter(var_imp, rel.inf > .5)
var_imp <- var_imp %>% arrange(desc(rel.inf))
var_imp$var <- factor(var_imp$var, levels=rev(var_imp$var))
ggplot(var_imp, aes(var, rel.inf)) +
    geom_bar(stat='identity', fill='blue') +
    coord_flip() +
    ggtitle('GBM Variable Importance') +
    labs(x ='Variable', y = 'Relative Influence') +
    theme(plot.title = element_text(hjust=.5))

ggsave(paste0('gbm_varimp_', name, '.png'),
       path = '../Graphs',
       plot = ggplot2::last_plot(), # or give ggplot object name as in myPlot,
       width = 16, height = 9,
       units = "in", # other options c("in", "cm", "mm"),
       dpi = 'retina')

# PCA


pcadata <- train.treat

pcadata <- pcadata[complete.cases(pcadata),]
num_vars <- unlist(lapply(pcadata, is.numeric))
pca <- prcomp(pcadata[,num_vars], center=TRUE, scale=TRUE, na.action= na.omit)

print(pca)
plot(pca, type='l')
summary(pca)

write.csv(pca$rotation, 'pca_matrix.csv', row.names=TRUE)

ggbiplot(pca, groups=pcadata$retention_d1h)


ggbiplot(pca, choices=c(1, 2), obs.scale = 1, var.scale = 1, ellipse = TRUE, groups=pcadata$retention_d1h,
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

write.csv(mydata, 'time_series_data.csv', row.names=FALSE)




data(iris)
iris