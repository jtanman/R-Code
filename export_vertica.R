# Export data from Vertica to R

install.packages("rJava",type='source')

library(feather)

source('~/.Rprofile')
setwd(datapath)

options(java.parameters = "-Xmx8048m")

library(rJava);library(RJDBC);library(DBI)

#this is where i store my credentials (userName and password) that are referenced below
# source("/Users/jting/Documents/creds.R")

connect <- c("jdbc:vertica://bi-db-vert-vip.addsrv.com:5433/analytics",
             "jdbc:vertica://Agencyvertdb-1-vip.agency.live.las1.mz-inc.com:5433/")
drv <- JDBC("com.vertica.jdbc.Driver", "vertica-jdbc-8.1.1-10.jar", identifier.quote="'")
conn <- dbConnect(drv, connect[1], userName, password[1])

start_time <- Sys.time()
mydata <- dbGetQuery(conn,"SELECT 
uds.game_id,
                     uds.session_daily_ts,
                     uds.dup_account,
                     uds.campaign_type,
                     
                     COUNT(uds.user_id) 		AS DAU,
                     COUNT(t.user_id) 		AS paid_players,
                     SUM(t.revenue) 			AS total_revenue,
                     SUM(t.transaction_num) 	AS transaction_num,
                     SUM(case when IFNULL(t.lifetime_revenue, uds.lifetime_revenue) >0 and age >1 then 1 else 0 end) 			AS RPDAU,
                     SUM(case when IFNULL(t.lifetime_revenue, uds.lifetime_revenue) >0 and age >1 then t.revenue else 0 end) 	AS RPDAU_revenue,
                     SUM(case when date(uds.session_daily_ts)=date(uds.first_purchase_ts) then 1 else 0 end) 					AS NPU,
                     SUM(case when date(uds.session_daily_ts)=date(uds.first_login_ts) then 1 else 0 end) 						AS FL_Count,
                     SUM(case when date(uds.session_daily_ts)=date(uds.install_ts) then 1 else 0 end)      						AS installs
                     FROM 
                     (
                     SELECT 
                     aa.game_id,
                     metric_date_ts session_daily_ts,
                     aa.user_id,
                     CASE 
                     when bb.campaign_type IN ('incent') THEN 'incent'
                     WHEN bb.campaign_type IN ('nonincent') THEN 'not-incent'
                     WHEN bb.campaign_type IN ('unclassified','organic') THEN 'organic' 
                     ELSE isnull(bb.campaign_type, 'organic') END campaign_type,
                     CASE WHEN bb.install_platform = 'iOS' THEN 1
                     WHEN bb.install_platform = 'android' THEN 2
                     ELSE 0 END AS install_platform,
                     ifnull(dup_account,0) AS dup_account,
                     DATEDIFF(day, first_login_ts, metric_date_ts) as age,
                     CASE WHEN DATEDIFF(day, first_login_ts, metric_date_ts) <= 30 THEN 1 ELSE 2 END AS agegroup,
                     user_level,
                     first_purchase_ts,
                     first_login_ts,
                     install_ts,
                     total_power as cumulative_power,
                     aa.lifetime_revenue
                     FROM 
                     bi_pipeline.nrt_user_daily aa
                     
                     LEFT JOIN 
                     bi_pipeline.user_level_data_base_utc bb
                     ON aa.game_id = bb.game_id 
                     AND aa.user_id = bb.user_id
                     
                     WHERE 
                     DATE(aa.metric_date_ts) BETWEEN '{START_DATE}' AND TIMESTAMPADD('day',1000,'{START_DATE}')
                     AND aa.game_id = {GAME_ID}
                     
                     ) uds
                     
                     LEFT JOIN 
                     (
                     SELECT 
                     game_id,
                     user_id, 
                     date(purchase_ts) purchase_date,
                     sum(purchase_revenue) revenue,
                     count(1) transaction_num,
                     max(lifetime_revenue) - sum(purchase_revenue) lifetime_revenue
                     
                     FROM bi_pipeline.nrt_purchase 
                     WHERE 
                     DATE(event_ts) BETWEEN '{START_DATE}' AND TIMESTAMPADD('day',1000,'{START_DATE}')
                     AND game_id = {GAME_ID}
                     GROUP BY game_id, user_id, purchase_ts
                     ) t 
                     ON uds.game_id = t.game_id 
                     and uds.user_id = t.user_id 
                     and date(uds.session_daily_ts) = date(t.purchase_date)
                     GROUP BY 
                     uds.game_id,
                     uds.session_daily_ts,
                     uds.dup_account,
                     uds.campaign_type
                     ORDER BY 
                     uds.game_id,
                     uds.session_daily_ts,
                     uds.dup_account,
                     uds.campaign_type")
end_time <- Sys.time()

print(end_time - start_time)

saveRDS(mydata, file='revenue_modeling_2018.6.27.RData')
# write_feather(mydata, 'metric_dau_isogroup_timeseries.feather')
