# By: James Tan

# Date: 1/18/2019
p_load(tidyverse)

numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

monnb <- function(d){
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
}

format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

getDailyInstalls <- function(dates, install_assumptions, customcpi, group_variables, spend_included=TRUE){
  # get daily installs from install assumptions, dates, customcpi, and whether spend is included or not

  if(spend_included){
    newdata <- tibble(install_date = dates) %>%
      mutate(
        month = as.factor(format(install_date, format='%Y-%m'))
      ) %>%
      left_join(install_assumptions, by='month') %>%
      mutate(
        month = as.factor(month),
        spend = spend / numdays,
        installs = if(customcpi){
          ifelse(spend == 0, installs / numdays, spend / cpi)
        }else{
          installs / numdays
        }
      ) %>%
      rename(
        total_spend_new = spend,
        installs_new = installs
      ) %>%
      ungroup() %>%
      mutate(
        days_since_launch = as.numeric(install_date - min(install_date)),
        weeks_since_launch = as.numeric(floor(difftime(install_date, min(install_date), units='weeks'))),
        months_since_launch = monnb(install_date) - monnb(min(install_date))
      ) %>%
      select(install_date, month, !!! group_variables, installs_new, total_spend_new, cpi, days_since_launch, weeks_since_launch, months_since_launch)
  }else{
    newdata <- tibble(install_date = dates) %>%
      mutate(
        month = as.factor(format(install_date, format='%Y-%m'))
      ) %>%
      left_join(install_assumptions, by='month') %>%
      mutate(
        month = as.factor(month),
        installs = installs / numdays
      ) %>%
      rename(
        installs_new = installs
      ) %>%
      ungroup() %>%
      mutate(
        days_since_launch = as.numeric(install_date - min(install_date)),
        weeks_since_launch = as.numeric(floor(difftime(install_date, min(install_date), units='weeks'))),
        months_since_launch = monnb(install_date) - monnb(min(install_date))
      ) %>%
      select(install_date, month, !!! group_variables, installs_new, days_since_launch, weeks_since_launch, months_since_launch)
  }

  return(newdata)
}


getRevDaily <- function(newdata, cohort_simulation, dates, group_variables, max_cohortday, post_group_variables=NULL, detailed=FALSE, spend_included=TRUE){
  # get daily revenue predictions

  # browser()
  join_vars <- quos(cohortday, !!! group_variables)
  dates <- data.frame(date = dates)

  alldata <- merge(newdata, dates) %>%
    filter(date >= install_date,
           date - install_date <= max_cohortday,
           date <= min(install_date) + max_cohortday
    ) %>%
    mutate(
      cohortday = as.numeric(date - install_date)
    ) %>%
    select(install_date, date, cohortday, month, !!! group_variables, installs_new) %>%
    left_join(cohort_simulation, by=sapply(join_vars, quo_name)) %>%
    arrange(install_date, cohortday)

  if(spend_included){
    installdata <- newdata %>%
      group_by(install_date, !!! post_group_variables) %>%
      summarise(
        installs = sum(installs_new),
        total_spend = sum(total_spend_new),
        cpi = total_spend / installs
      ) %>%
      rename(date = install_date)
  }else{
    installdata <- newdata %>%
      group_by(install_date, !!! post_group_variables) %>%
      summarise(
        installs = sum(installs_new),
      ) %>%
      rename(date = install_date)
  }

  # daily predictions

  join_vars <- quos(date, !!! post_group_variables)

  if(detailed){
    predictdata <- alldata %>%
      mutate(
        revenue = arpi * installs_new,
        revenue_rpdau = revenue * rpdau_revshare,
        rpdau = rpdau_ret * installs_new,
        rpdau_20 = rpdau_20_ret_installs * installs_new,
        rpdau_100 = rpdau_100_ret_installs * installs_new,
        dau = retention * installs_new,
        payers = conversion * dau
      ) %>%
      group_by(date, !!! post_group_variables) %>%
      summarise(
        revenue = sum(revenue),
        revenue_rpdau = sum(revenue_rpdau),
        rpdau = sum(rpdau),
        rpdau_20 = sum(rpdau_20),
        rpdau_100 = sum(rpdau_100),
        dau = sum(dau),
        payers = sum(payers),
        arpdau = revenue / dau,
        arprpdau = revenue_rpdau / rpdau
      ) %>%
      left_join(installdata, by=sapply(join_vars, quo_name)) %>%
      group_by(!!! post_group_variables) %>%
      arrange(date) %>%
      mutate(
        cum_rev = cumsum(revenue),
        cum_installs = cumsum(installs),
        month = as.factor(format(date, '%Y-%m'))
      )
  }else{
    predictdata <- alldata %>%
      mutate(
        revenue = arpi * installs_new,
        dau = retention * installs_new,
      ) %>%
      group_by(date, !!! post_group_variables) %>%
      summarise(
        revenue = sum(revenue),
        dau = sum(dau),
        arpdau = revenue / dau
      )  %>%
      left_join(installdata, by=sapply(join_vars, quo_name)) %>%
      group_by(!!! post_group_variables) %>%
      arrange(date) %>%
      mutate(
        cum_rev = cumsum(revenue),
        cum_installs = cumsum(installs),
        month = as.factor(format(date, '%Y-%m'))
      )
  }

  return(list(installdata=installdata, alldata=alldata, predictdata=predictdata))
}

getRevMonthly <- function(predictdata, group_variables=NULL, detailed=FALSE, alldata=NA, installdata=NA, spend_included=TRUE, cut_incomplete=FALSE){
  # get monthly rev predictions from daily predictions

  # browser()

  join_vars <- quos(date, !!! group_variables)

  if(detailed){
    # return detailed rev predictions

    predictdata_month <- alldata %>%
      mutate(
        revenue = arpi * installs_new,
        revenue_rpdau = revenue * rpdau_revshare,
        rpdau = rpdau_ret * installs_new,
        rpdau_20 = rpdau_20_ret_installs * installs_new,
        rpdau_100 = rpdau_100_ret_installs * installs_new,
        dau = retention * installs_new,
        payers = conversion * dau
      ) %>%
      group_by(date, month, !!! group_variables) %>%
      summarise(
        revenue = sum(revenue),
        revenue_rpdau = sum(revenue_rpdau),
        rpdau = sum(rpdau),
        rpdau_20 = sum(rpdau_20),
        rpdau_100 = sum(rpdau_100),
        dau = sum(dau),
        payers = sum(payers),
        arprpdau = revenue_rpdau / rpdau
      ) %>%
      left_join(installdata, by=sapply(join_vars, quo_name))

    rpdau_delta <- predictdata_month %>%
      ungroup() %>%
      dplyr::mutate(
        date_month = as.factor(format(date, '%Y-%m'))
      ) %>%
      group_by(date_month, !!! group_variables) %>%
      filter(date == max(date)) %>%
      summarise(
        rpdau_total = sum(rpdau),
        rpdau_gain = rpdau[which(date_month == month)],
        rpdau_existing = rpdau_total - rpdau_gain,
        rpdau_20_total = sum(rpdau_20),
        rpdau_20_gain = rpdau_20[which(date_month == month)],
        rpdau_20_existing = rpdau_20_total - rpdau_20_gain,
        rpdau_100_total = sum(rpdau_100),
        rpdau_100_gain = rpdau_100[which(date_month == month)],
        rpdau_100_existing = rpdau_100_total - rpdau_100_gain
      ) %>%
      ungroup() %>%
      group_by(!!! group_variables)
      mutate(
        rpdau_net_churn = lag(rpdau_total) - rpdau_existing,
        rpdau_20_net_churn = lag(rpdau_20_total) - rpdau_20_existing,
        rpdau_100_net_churn = lag(rpdau_100_total) - rpdau_100_existing
      ) %>%
      rename(month = date_month)

    # monthly predictions

    join_vars <- quos(month, !!! group_variables)

    if(spend_included){
      month_output <- predictdata %>%
        group_by(month, !!! group_variables) %>%
        summarise(
          revenue = sum(revenue),
          cpi = weighted.mean(cpi, installs),
          installs = sum(installs),
          arpdau = revenue / sum(dau),
          arprpdau = sum(revenue_rpdau) / sum(rpdau),
          rpdau_end = rpdau[which(date == max(date))],
          rpdau_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          rpdau_20_end = rpdau_20[which(date == max(date))],
          rpdau_20_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau_20[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          rpdau_100_end = rpdau_100[which(date == max(date))],
          rpdau_100_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau_100[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          dau = dau[which(date == max(date))],
          spend = installs * cpi
        ) %>%
        mutate(
          rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01')))
        ) %>%
        left_join(rpdau_delta, by=sapply(join_vars, quo_name)) %>%
        group_by(!!! group_variables) %>%
        arrange(month) %>%
        mutate(
          cum_rev = cumsum(revenue),
          cum_installs = cumsum(installs)
        )  %>%
        select(
          month, !!! group_variables, spend, revenue, cpi, installs, dau, rpdau_mid, rpdau_end, rpdau_gain,
          rpdau_net_churn, rpdau_20_mid, rpdau_20_end, rpdau_20_gain, rpdau_20_net_churn, rpdau_100_mid, rpdau_100_end,
          rpdau_100_gain, rpdau_100_net_churn, arpdau, arprpdau, cum_rev, cum_installs
        )
    }else{
      month_output <- predictdata %>%
        group_by(month, !!! group_variables) %>%
        summarise(
          revenue = sum(revenue),
          installs = sum(installs),
          arpdau = revenue / sum(dau),
          arprpdau = sum(revenue_rpdau) / sum(rpdau),
          rpdau_end = rpdau[which(date == max(date))],
          rpdau_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          rpdau_20_end = rpdau_20[which(date == max(date))],
          rpdau_20_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau_20[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          rpdau_100_end = rpdau_100[which(date == max(date))],
          rpdau_100_mid = if(length(which(format(date, '%d') == '15')) > 0){
            rpdau_100[which(format(date, '%d') == '15')]
          }else{
            NA
          },
          dau = dau[which(date == max(date))],
        ) %>%
        mutate(
          rev_rpdau_mid_arprpdau = rpdau_mid * arprpdau * days_in_month(as.Date(paste0(month, '-01')))
        ) %>%
        left_join(rpdau_delta, by=sapply(join_vars, quo_name)) %>%
        group_by(!!! group_variables) %>%
        arrange(month) %>%
        mutate(
          cum_rev = cumsum(revenue),
          cum_installs = cumsum(installs)
        ) %>%
        select(
          month, !!! group_variables, revenue, installs, dau, rpdau_mid, rpdau_end, rpdau_gain, rpdau_net_churn,
          rpdau_20_mid, rpdau_20_end, rpdau_20_gain, rpdau_20_net_churn, rpdau_100_mid, rpdau_100_end, rpdau_100_gain,
          rpdau_100_net_churn, arpdau, arprpdau, cum_rev, cum_installs
        )
    }

  }else if(spend_included){
    month_output <- predictdata %>%
      group_by(month, !!! group_variables) %>%
      summarise(
        revenue = sum(revenue),
        cpi = weighted.mean(cpi, installs),
        installs = sum(installs),
        arpdau = revenue / sum(dau),
        dau = dau[which(date == max(date))],
        spend = installs * cpi
      ) %>%
      group_by(!!! group_variables) %>%
      arrange(month) %>%
      mutate(
        cum_rev = cumsum(revenue),
        cum_installs = cumsum(installs)
      ) %>%
      select(
        month, !!! group_variables, spend, revenue, cpi, installs, dau, arpdau, cum_rev, cum_installs
      )
  }else{
    month_output <- predictdata %>%
      group_by(month, !!! group_variables) %>%
      summarise(
        revenue = sum(revenue),
        installs = sum(installs),
        arpdau = revenue / sum(dau),
        dau = dau[which(date == max(date))],
      ) %>%
      group_by(!!! group_variables) %>%
      arrange(month) %>%
      mutate(
        cum_rev = cumsum(revenue),
        cum_installs = cumsum(installs)
      ) %>%
      select(
        month, !!! group_variables, revenue, installs, dau, arpdau, cum_rev, cum_installs
      )
  }

  # browser()

  if(cut_incomplete){
    testLast <- function(predictdata, month_output){
      last_day <- tail(predictdata, 1)
      last_day <- last_day %>%
        mutate(
          month_start = as.Date(paste0(as.character(month), '-01'), format='%Y-%m-%d'),
          numdays = numberOfDays(month_start)
        )
      
      if(day(last_day$date) != last_day$numdays){
        month_output <- head(month_output, -1)
      }
      
      return(month_output)
    }
    
    month_output <- month_output %>%
      group_by(!!! group_variables) %>%
      do(
        testLast(predictdata, .)
      )
  }

  return(month_output)

}

parseCohortData <- function(cohortdata, group_variables=NULL, detailed=FALSE){
  # parse cohortdata from hive



  cohortdata <- cohortdata %>%
  filter(install_platform == 'iOS' | install_platform == 'android') %>%
  # rename(install_date = install_event_date) %>%
  rename(date = curr_date) %>%
  mutate(
    game_id = as.factor(game_id),
    install_date = as.Date(install_date),
    install_month = as.factor(format(install_date, '%Y-%m')),
    date = as.Date(date),
    date_month = as.factor(format(date, '%Y-%m')),
    # install_platform = as.factor(ifelse(install_platform == 1, 'iOS', ifelse(install_platform == 2, 'android', 'Other'))),
    # dup_account = as.factor(ifelse(dup_account == 0, 'dup', 'non-dup')),
    install_platform = droplevels(install_platform),
    # dup_account = as.factor(dup_account),
    # country_tier = as.factor(country_tier),
    type =   if(is.null(group_variables)){
      NA
    }else{
      droplevels(interaction(!!! group_variables))
    }
  ) %>%
  group_by(!!! group_variables) %>%
  mutate(
    days_since_launch = as.numeric(install_date - min(install_date)),
    weeks_since_launch = as.numeric(floor(difftime(install_date, min(install_date), units='weeks'))),
    months_since_launch = monnb(install_date) - monnb(min(install_date))
  ) %>%
  arrange(game_id, install_date, cohortday) %>%
  select(game_id, install_date, date, cohortday, install_month, date_month, everything())

  if(detailed){
    cohortdata <- cohortdata %>%
      mutate(
        arpi = revenue / installs,
        retention = dau / installs,
        rpdau_ret = rpdau / installs,
        arpdau = revenue / dau,
        arppu = revenue / payers,
        conversion = payers / dau,
        streak_conversion = payers_streak / dau,
        arprpdau = revenue_rpdau / rpdau,
        rpdau_revshare = revenue_rpdau / revenue,
        rpdau_20_ret_installs = rpdau_20 / installs,
        rpdau_100_ret_installs = rpdau_100 / installs,
        step_5_conversion = payers_5 / installs,
        step_20_conversion = payers_20 / installs,
        step_100_conversion = payers_100 / installs,
        streak_revshare = revenue_streak / revenue,
        streak_arpdau = revenue_streak / payers_streak,
        unique_arpdau = revenue_unique / payers_unique,
      )
  }

  return(cohortdata)
}

regroupCohortData <- function(cohortdata, group_variables, cohortday_start=0){
  # regroup cohortdata according to group_variables
  
  cohortdata <- cohortdata %>%
    group_by(cohortday, !!! group_variables) %>%
    summarise(
      n = n(),
      installs = sum(installs),
      dau = sum(dau),
      rpdau = sum(rpdau),
      rpdau_5 = sum(rpdau_5),
      rpdau_20 = sum(rpdau_20),
      rpdau_100 = sum(rpdau_100),
      payers = sum(payers),
      payers_5 = sum(payers_5),
      payers_20 = sum(payers_20),
      payers_100 = sum(payers_100),
      payers_unique = sum(payers_unique),
      payers_streak = sum(payers_streak),
      revenue = sum(revenue),
      revenue_npu = sum(revenue_npu),
      revenue_rpdau = sum(revenue_rpdau),
      revenue_unique = sum(revenue_unique),
      revenue_streak = sum(revenue_streak),
      revenue_npu = sum(revenue_npu),
      revenue_rpdau = sum(revenue_rpdau),
      arpi = revenue / installs,
      retention = dau / installs,
      rpdau_ret = rpdau / installs,
      arpdau = revenue / dau,
      arppu = revenue / payers,
      conversion = payers / dau,
      streak_conversion = payers_streak / dau,
      arprpdau = revenue_rpdau / rpdau,
      rpdau_revshare = revenue_rpdau / revenue,
      rpdau_20_ret_installs = rpdau_20 / installs,
      rpdau_100_ret_installs = rpdau_100 / installs,
      step_5_conversion = payers_5 / installs,
      step_20_conversion = payers_20 / installs,
      step_100_conversion = payers_100 / installs,
      streak_revshare = revenue_streak / revenue,
      streak_arpdau = revenue_streak / payers_streak,
      unique_arpdau = revenue_unique / payers_unique,
    )  %>%
    group_by(!!! group_variables) %>%
    arrange(cohortday) %>%
    mutate(
      ltv_5_users = cumsum(payers_5),
      ltv_20_users = cumsum(payers_20),
      ltv_100_users = cumsum(payers_100),
      cumrev = cumsum(revenue),
      cumrev_npu = cumsum(revenue_npu),
      cumrev_rpdau = cumsum(revenue_rpdau),
      cumarpi = cumsum(arpi),
      ltv_5_conversion = cumsum(step_5_conversion),
      ltv_20_conversion = cumsum(step_20_conversion),
      ltv_100_conversion = cumsum(step_100_conversion),
      rpdau_5_ret = rpdau_5 / ltv_5_users,
      rpdau_20_ret = rpdau_20 / ltv_20_users,
      rpdau_100_ret = rpdau_100 / ltv_100_users,
      cumarpi_growth = cumarpi / lag(cumarpi, n=1),
      cumarpi_growth = ifelse(cohortday <= cohortday_start, 1, cumarpi_growth),
      cumarpi_mult = cumprod(cumarpi_growth),
      cumarpi_mult = ifelse(cohortday < cohortday_start, NA, cumarpi_mult)
    ) %>%
    ungroup() %>%
    arrange(!!! group_variables, cohortday)
  
  return(cohortdata)
}


getG4Predictions <- function(newdata, gamedata, r, g, dates, group_variables, max_cohortday, use_CI){

  # browser()

  gamedata <- filter(gamedata, game==g, region==r)
  newdata_region <- filter(newdata, region==r)

  if(use_CI){

    game_normal <- gamedata %>%
      mutate(arpi = retention * arpdau)
    results_normal <- getG4PredictionsHelper(newdata_region, game_normal, dates, group_variables, max_cohortday)
    predict_normal <- results_normal$predictdata %>%
      mutate(
        type='predicted'
      )
    month_normal <- results_normal$monthdata %>%
      mutate(
        type='predicted'
      )

    game_high <- gamedata %>%
      mutate(retention = retention_upper, arpdau = arpdau_upper, arpi = retention * arpdau)
    results_high <- getG4PredictionsHelper(newdata_region, game_high, dates, group_variables, max_cohortday)
    predict_high <- results_high$predictdata %>%
      mutate(
        type='upper'
      )
    month_high <- results_high$monthdata %>%
      mutate(
        type='upper'
      )

    game_low <- gamedata %>%
      mutate(retention = retention_lower, arpdau = arpdau_lower, arpi = retention * arpdau)
    results_low <- getG4PredictionsHelper(newdata_region, game_low, dates, group_variables, max_cohortday)
    predict_low <- results_low$predictdata %>%
      mutate(
        type='lower'
      )
    month_low <- results_low$monthdata %>%
      mutate(
        type='lower'
      )

    predictdata <- reduce(list(predict_normal, predict_high, predict_low), rbind) %>%
      select(game, region, date, month, type, everything())
    monthdata <- reduce(list(month_normal, month_high, month_low), rbind) %>%
      select(game, region, month, type, everything())

  }else{
    gamedata <- gamedata %>%
      mutate(arpi = retention * arpdau)

    results <- getG4PredictionsHelper(newdata_region, gamedata, dates, group_variables, max_cohortday)
    predictdata <- results$predictdata %>%
      mutate(
        type = 'predicted'
      )
    monthdata <- results$monthdata %>%
      mutate(
        type = 'predicted'
      )
  }
  
  predictdata$type <- as.factor(predictdata$type)
  monthdata$type <- as.factor(monthdata$type)

  return(list(predictdata=predictdata, monthdata=monthdata))

}

getG4PredictionsHelper <- function(newdata, gamedata, dates, group_variables, max_cohortday){

  revdata <- getRevDaily(newdata, gamedata, dates, group_variables, max_cohortday, post_group_variables=NULL, detailed=FALSE, spend_included = FALSE)
  alldata <- filter(revdata[['alldata']], cohortday <= max_cohortday)
  predictdata <- revdata[['predictdata']]
  installdata <- revdata[['installdata']]
  monthdata <- getRevMonthly(predictdata, group_variables=NULL, detailed=FALSE, alldata, installdata, spend_included = FALSE, cut_incomplete = TRUE)

  predictdata <- predictdata %>%
    mutate(
      game = g,
      region = r
    ) %>%
    select(game, region, date, month, everything())

  monthdata <- monthdata %>%
    mutate(
      game = g,
      region = r
    ) %>%
    select(game, region, month, everything())

  return(list(predictdata=predictdata, monthdata=monthdata))
}

