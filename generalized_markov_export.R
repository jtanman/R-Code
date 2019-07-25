 # Export data from Vertica to R

install.packages("rJava",type='source')

source('~/.Rprofile')
setwd(datapath)

options(java.parameters = "-Xmx8048m")

p_load(RJDBC, DBI, feather)

connect <- c("jdbc:vertica://bi-db-vert-vip.addsrv.com:5433/analytics",
             "jdbc:vertica://Agencyvertdb-1-vip.agency.live.las1.mz-inc.com:5433/")
drv <- JDBC("com.vertica.jdbc.Driver", "vertica-jdbc-8.1.1-10.jar", identifier.quote="'")
conn <- dbConnect(drv, connect[1], userName, password[1])


# p_load(RODBC)
# 
# vertica <- odbcDriverConnect(sprintf("driver={/Library/Vertica/ODBC/lib/libverticaodbc.so};server=jdbc:vertica://bi-db-vert-vip.addsrv.com;database=analytics;Uid=%s;Pwd=%s", userName, password))


game_id <- 23
install_start <- '2018-11-01'
install_end <- '2018-11-28'

name <- paste('generalized_markov', game_id, install_start, install_end, sep='_')
filename <- paste0(name, '.RData')

query <- sprintf("
SELECT
    install_date,
    DATE,
    cohortday,
    campaign_type,
    install_platform,
    dup_account,
    state,
    next_state,
    state_count,
    COUNT(*) AS n,
    item_purchase_overall,
    revenue_overall,
    SUM(item_purchase_cnt) AS item_purchase_count,
    SUM(revenue)           AS revenue
FROM
    (
        SELECT
            *,
            LEAD(state, 1) OVER (PARTITION BY user_id ORDER BY cohortday) AS next_state,
            COUNT(*) OVER (PARTITION BY install_date, DATE, cohortday, campaign_type,
            install_platform, dup_account, state) AS state_count,
            SUM(item_purchase_cnt) OVER (PARTITION BY install_date, DATE, cohortday, campaign_type,
            install_platform, dup_account, state) AS item_purchase_overall,
            SUM(CEILING(revenue)) OVER (PARTITION BY install_date, DATE, cohortday, campaign_type,
            install_platform, dup_account, state) AS revenue_overall
        FROM
            (
                SELECT
                    a.user_id,
                    a.install_ts::DATE                     AS install_date,
                    a.metric_ts::DATE                      AS DATE,
                    a.metric_ts::DATE - a.install_ts::DATE AS cohortday,
                    CASE
                        WHEN a.campaign_type = 'unclassified'
                        THEN 'organic'
                        ELSE ISNULL(a.campaign_type, 'organic')
                    END AS campaign_type,
                    a.install_platform,
                    CASE
                        WHEN b.dup_account = 1
                        THEN 'nondup'
                        WHEN b.dup_account = 0
                        THEN 'dup'
                        ELSE NULL
                    END         AS dup_account,
                    a.is_active AS session,
                    a.item_purchase_cnt,
                    CEILING(a.revenue)          AS revenue,
                    CEILING(a.lifetime_revenue) AS lifetime_revenue,
                    CASE
                        WHEN a.is_active = 0
                        AND CEILING(a.lifetime_revenue) = 0
                        THEN 0
                        WHEN a.is_active = 0
                        AND CEILING(a.lifetime_revenue) > 0
                        AND CEILING(a.lifetime_revenue) < 20
                        THEN 1
                        WHEN a.is_active = 0
                        AND CEILING(a.lifetime_revenue) >= 20
                        AND CEILING(a.lifetime_revenue) < 100
                        THEN 2
                        WHEN a.is_active = 0
                        AND CEILING(a.lifetime_revenue) >= 100
                        THEN 3
                        WHEN a.is_active = 1
                        AND CEILING(a.lifetime_revenue) = 0
                        THEN 4
                        WHEN a.is_active = 1
                        AND CEILING(a.lifetime_revenue) > 0
                        AND CEILING(a.lifetime_revenue) < 20
                        THEN 5
                        WHEN a.is_active = 1
                        AND CEILING(a.lifetime_revenue) >= 20
                        AND CEILING(a.lifetime_revenue) < 100
                        THEN 6
                        WHEN a.is_active = 1
                        AND CEILING(a.lifetime_revenue) >= 100
                        THEN 7
                    END AS state
                FROM
                    zeus_pipeline.user_daily a
                LEFT JOIN
                    zeus_pipeline.user_level b
                USING
                    (user_id, game_id)
                WHERE
                    a.game_id = %d
                AND a.install_ts >= '%s'
                AND a.install_ts < '%s'
                AND a.metric_ts >= '%s') a) a
GROUP BY
    install_date,
    DATE,
    cohortday,
    campaign_type,
    install_platform,
    dup_account,
    state,
    next_state,
    state_count,
    item_purchase_overall,
    revenue_overall
ORDER BY
    campaign_type,
    install_platform,
    dup_account,
    install_date,
    cohortday,
    state,
    next_state
", game_id, install_start, install_end, install_start)

start_time <- Sys.time()
# mydata <- dbGetQuery(conn,query)
sample_query <- sqlQuery(vertica, query)
end_time <- Sys.time()

print(end_time - start_time)

saveRDS(mydata, file=filename)
# write_feather(mydata, 'metric_dau_isogroup_timeseries.feather')