# Code to simulate forward split data and train on post data

  #
  #   mydata <- parseData(dldata, game, region,  end_cohortday)
  #
  #   splitdata <- tibble(days_since_launch = seq(0, end_cohortday), installs = mydata$installs, dau_combined = mydata$dau,
  #                       dau_pre = dau_pre, dau_post = dau_combined - dau_pre)
  #
  #   plotdata <- melt(splitdata, id.vars='days_since_launch')
  #   ggplot(plotdata, aes(days_since_launch, value, color=variable)) +
  #     geom_line()
  #
  #   splitdata <- splitdata %>%
  #     filter(days_since_launch > max_cohortday) %>%
  #     mutate(days_since_launch = row_number()- 1) %>%
  #     select(days_since_launch, installs, dau)
  #   retention_fit_split <- fitRetention(splitdata, max(splitdata$days_since_launch), par=c(retention_fit$values[1], retention_fit$values[2]), na.rm=TRUE)
  #   retention_fit_split$retention
  #   retention_fit$retention


# change age of magic

        # if(g == 'Age.of.Magic'){
        #   browser()
        #   c <- 117
        #   temp_game <- gamedata
        #   temp_my <- mydata
        #   load(paste(g, r, startday, bounded, c, 'fit.RData', sep='_'))
        #   # gamedata <- tibble(game=g, region=r, cohortday=seq(0, max_cohortday), retention=getRetention(max_cohortday, retention_fit$values), arpdau=arpdau_fit$fn(seq(0, max_cohortday), arpdau_fit$values))
        #   #
        #   # mynew <- temp_my %>% mutate(
        #   #   game = g,
        #   #   region = r,
        #   #   installs = c(mydata$installs, rep(NA, nrow(temp_my) - nrow(mydata))),
        #   #   dau = c(mydata$dau, rep(NA, nrow(temp_my) - nrow(mydata))),
        #   #   rev = c(mydata$rev, rep(NA, nrow(temp_my) - nrow(mydata))),
        #   # )
        #   #
        #   # mynew <- mynew %>%
        #   # mutate(
        #   #   dau_pred = getDAU(mynew, max_cohortday, gamedata$retention),
        #   #   rev_pred = getRev(gamedata$arpdau, gamedata$retention, max_cohortday, mynew)
        #   # )
        #   #
        #   # save(mydata, gamedata, retention_fit, arpdau_fit, file=paste(g, r, startday, bounded, max_cohortday, 'fit.RData', sep='_'))
        #
        #
        # }else{
        #   load(paste(g, r, startday, bounded, c, 'fit.RData', sep='_'))
        # }


# average high retention middle arpdau and middle retention high arpdau

    # game_high_ret <- gamedata %>%
    #   mutate(retention = retention_upper) %>%
    #   mutate(arpi = retention_upper * arpdau)
    # results_high_ret <- getG4PredictionsHelper(newdata_region, game_high_ret, dates, group_variables, max_cohortday)
    # game_high_arpdau <- gamedata %>%
    #   mutate(arpi = retention * arpdau_upper)
    # results_high_arpdau <- getG4PredictionsHelper(newdata_region, game_high_arpdau, dates, group_variables, max_cohortday)
    #
    # predictdata_high <- results_high_ret$predictdata %>%
    #   mutate(
    #     revenue = (revenue + pull(results_high_arpdau$predictdata, revenue)) / 2
    #   )