# By: James Tan

# Date: 2/12/2019

source('~/.Rprofile')
setwd(datapath)

p_load(ggplot2, reshape2, lubridate, dplyr, beepr, scales, zoo)

RETENTION_PRIOR <- c(0.907267613763426, 0.149440616538399, 0.090465960329371, 0.0733020455677506,
                     0.0647969890872745, 0.0570828618411206, 0.0513304143689085, 0.0469656935761463,
                     0.0423748392919596, 0.0387402390058474, 0.0359329077415501, 0.0337256559955903,
                     0.0319111364036798, 0.0305880218448581, 0.0295284176611529, 0.0279475097919056)

RETENTION_RANGE <- readRDS('apac_west_retention_range.RData')

FF_GAMES <- c('NISO.Actual', 'FF.actual', 'Final.Fantasy')

trimNA <- function(a){
  return(a[min(which(!is.na(a))):max(which(!is.na(a)))])
}

parseData <- function(dldata, game, region=c('apac', 'west'), max_cohortday=NULL){

  # browser()

  region <- match.arg(region)

  if(is.null(max_cohortday)){
    max_cohortday <- max(dldata$Cohort.Day)
  }

  if(region == 'west'){

    mydata <- dldata %>% rename(
      installs = paste0('Downloads.', game)
    )

  }else if(region == 'apac'){

    mydata <- dldata %>% rename(
      installs = paste0('Daily.Downloads.', game)
    )

  }else{
    stop('Incorrect Region')
  }

  mydata <- mydata %>% rename(
    month = Cohort.Month,
    days_since_launch = Cohort.Day,
    rev = paste0('Revenue.', game),
    dau = paste0('DAU.', game),
    # arpdau = paste0('ARPDAU.', game),
    # install_base = paste0('Install.Base.', game),
  )

 mydata <- mydata %>%
  filter(
    dplyr::row_number() >= min(which(!is.na(installs))),
    dplyr::row_number() <= max(which(!is.na(installs)))
  ) %>%
  mutate(
    days_since_launch = row_number() - 1,
    game = game,
    region = region,
    installs = na.spline(installs),
    # installs = installs*10,
    # cum_installs = cumsum(installs),
    # dau_retention = dau / cum_installs,
  ) %>%
  filter(
    days_since_launch <= max_cohortday
  ) %>%
  select(
    # game, month, days_since_launch, installs, dau, rev, cum_installs, dau_retention
    game, region, month, days_since_launch, installs, dau, rev
  )

  return(mydata)
}

pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  # browser()
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b), pbeta(x, a,b), qbeta(x, a,b))
  f <- fx; f[fx == Inf] <- NA
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}

survivalRate <- function(a, b, t){
  return(beta(a, b+t) / beta(a, b))
}

getRetention <- function(n, values){

  if(length(values) == 2){
    days <- seq(0, n)
    retention <- sapply(days, function(n) beta(values[1], values[2] + n) / beta(values[1], values[2]))
  }else{
    days <- seq(0, n - (length(values) - 3))
    retention <- values[3:length(values)]

    retention_beta <- sapply(days, function(n) beta(values[1], values[2] + n) / beta(values[1], values[2]))
    retention_beta <- retention_beta * retention[length(retention)]
    retention <- c(retention, retention_beta[-1])
  }

  return(retention)
}

getDAU <- function(mydata, max_cohortday, retention){

  retentiondata <- data.frame(cohortday = seq(0, max_cohortday), retention = retention)
  alldata <- merge(mydata, retentiondata) %>%
    filter(days_since_launch + cohortday <= max_cohortday) %>%
    mutate(
      day = days_since_launch + cohortday,
      dau_pred = retention * installs
    ) %>%
    arrange(days_since_launch, cohortday)

  dau_pred <- alldata %>%
    group_by(day) %>%
    summarise(
      dau_pred = sum(dau_pred)
    ) %>%
    pull(dau_pred)

  return(dau_pred)
}

getDauLoss <- function(values, max_cohortday, mydata, squared=FALSE, na.rm=FALSE){

  # browser()
  # alpha <- values[1]
  # beta <- values[2]

  dau <- filter(mydata, days_since_launch <= max_cohortday) %>% pull(dau)
  dau_pred <- getDAU(mydata, max_cohortday, getRetention(max_cohortday, values))

  if(squared){
    return(mean((dau - dau_pred)^2, na.rm=na.rm))
  }else{
    return(mean(abs(dau - dau_pred), na.rm=na.rm))
  }
}


fitRetention <- function(mydata, max_cohortday, startday=2, retention_prior=RETENTION_PRIOR, par=NULL, bounded=FALSE, squared=FALSE, na.rm=FALSE){

  values <- par

  if(is.null(values)){
    a <- 1
    b <- 1
    if(startday > 0){
      values <- c(a, b, retention_prior[1:startday])
    }else{
      values <- c(a, b)
    }
  }

  if(bounded){

    if(mydata$game[1] %in% c('NISO.Actual', 'FF.actual', 'Final.Fantasy')){
      lower_bounds <- c(1e-10, 1e-10, pull(RETENTION_RANGE, min_ret))[1:length(values)]
      upper_bounds <- c(Inf, Inf,  pull(RETENTION_RANGE, max_ret))[1:length(values)]
    }else{
      lower_bounds <- c(1e-10, 1e-10, rep(0, length(values) - 2))
      upper_bounds <- c(Inf, Inf,  rep(1, length(values) - 2))
    }

    result <- optim(values, getDauLoss, lower=lower_bounds, upper=upper_bounds, max_cohortday=max_cohortday, mydata=mydata, squared=squared, na.rm=na.rm)
  }else{
    result <- optim(values, getDauLoss, max_cohortday=max_cohortday, mydata=mydata, squared=squared, na.rm=na.rm)
  }

  values <- result$par
  alpha_opt <- values[1]
  beta_opt <- values[2]

  retention_opt <- getRetention(max_cohortday, values)
  retentiondata <- data.frame(cohortday = seq(0, max_cohortday), retention = retention_opt)

  return(list(result=result, retention=retention_opt, values=values))
}

getRev <- function(arpdau, retention, max_cohortday, mydata){

  arpidata <- data.frame(cohortday = seq(0, max_cohortday), arpi=retention*arpdau)

  alldata <- merge(mydata, arpidata) %>%
    mutate(
      day = days_since_launch + cohortday
    ) %>%
    filter(day <= max_cohortday) %>%
    mutate(
      rev = arpi * installs
    )

  rev_pred <- alldata %>%
    group_by(day) %>%
    summarise(
      rev_pred = sum(rev),
    ) %>%
    pull(rev_pred)

  return(rev_pred)
}

getRevLoss <- function(arpdau, retention, max_cohortday, mydata, squared=FALSE, na.rm=FALSE){

  rev <- filter(mydata, days_since_launch <= max_cohortday) %>% pull(rev)
  rev_pred <- getRev(arpdau, retention, max_cohortday, mydata)

  if(squared){
    return(mean((rev - rev_pred)^2, na.rm=na.rm))
  }else{
    return(mean(abs(rev - rev_pred), na.rm=na.rm))
  }
}

fitARPDAU <- function(mydata, retention, max_cohortday, method=c('log', 'power', 'exp', 'linear', 'full', 'all'), par=NULL, squared=FALSE, na.rm=FALSE){

  # browser()

  method <- match.arg(method)
  values <- par

  if(method == 'log'){
    if(is.null(values)){
      a = 1
      b = 0
      c = 1
      values = c(a, b, c)
    }

    getArpdauFunction <- function(x, values) values[1] * log(x + values[3]) + values[2]

  }else if(method == 'power'){
    if(is.null(values)){
      a = 1
      b = .5
      c = 0
      d = 0
      values = c(a, b, c, d)
    }

    getArpdauFunction <- function(x, values) values[1] * (x + values[3]) ^ values[2]+ values[4]

  }else if(method == 'exp'){
    if(is.null(values)){
      a = 1
      b = 1
      c = 0
      d = 0
      values = c(a, b, c, d)
    }

    getArpdauFunction <- function(x, values) values[1] * values[2] ^ (x + values[3]) + values[4]

  }else if(method == 'linear'){
    if(is.null(values)){
      a = 1
      b = 0
      c = 0
      values = c(a, b, c)
    }

    getArpdauFunction <- function(x, values) values[1] * (x + values[2]) + values[3]

  }else if(method == 'full'){
    if(is.null(values)){
      values <- seq(0, 5, length.out = max_cohortday + 1)
    }

    getArpdauFunction <- function(x, values) values[x+1]

  }else if(method == 'all'){

    log_loss <- fitARPDAU(mydata, retention, max_cohortday, method='log', par=par, squared=squared, na.rm=na.rm)
    power_loss <- fitARPDAU(mydata, retention, max_cohortday, method='power', par=par, squared=squared, na.rm=na.rm)
    exp_loss <- fitARPDAU(mydata, retention, max_cohortday, method='exp', par=par, squared=squared, na.rm=na.rm)
    linear_loss <- fitARPDAU(mydata, retention, max_cohortday, method='linear', par=par, squared=squared, na.rm=na.rm)

    # browser()

    combined_models <- list(log_loss, power_loss, exp_loss, linear_loss)

    losses <- unlist(lapply(combined_models, function(x) x$result$value))

    return(combined_models[[which.min(losses)]])
  }

  getLossOptim <- function(values){
    arpdau <- sapply(seq(0, max_cohortday), getArpdauFunction, values=values)
    return(getRevLoss(arpdau, retention, max_cohortday, mydata, squared, na.rm))
  }

  result <- optim(values, getLossOptim)
  values <- result$par
  arpdau <- sapply(seq(0, max_cohortday), getArpdauFunction, values=values)

  return(list(result=result, arpdau=arpdau, values=values, method=method, fn=getArpdauFunction))

}

runGame <- function(dldata, game, region, startday, max_cohortday, end_cohortday=270, split=FALSE, bounded=FALSE, plot_flag=TRUE){

  # browser()

  if(region == 'apac'){
    region_print <- toupper(region)
  }else{
    region_print <- toTitleCase(region)
  }

  tryCatch({

    mydata <- parseData(dldata, game, region, max_cohortday)

    max_cohortday <- max(mydata$days_since_launch)

    # plotdata <- mydata %>% select(days_since_launch, installs, dau)
    # plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='num_users')
    # ggplot(plotdata, aes(days_since_launch, num_users, color=metric)) +
    #   geom_line() +
    #   labs(title = paste(game, region_print, max_cohortday, 'Installs and DAU')) +
    #   theme(plot.title = element_text(hjust=.5))

    # alpha <- 1
    # beta <- 1

    # pl.beta(alpha, beta)

    retention_fit <- fitRetention(mydata, max_cohortday, startday=startday, bounded=bounded, na.rm=TRUE)
    retention_opt <- retention_fit$retention
    # pl.beta(retention_fit$values[1], retention_fit$values[2])

    retentiondata <- tibble(cohortday = seq(0, max_cohortday), retention = retention_opt)
    ggplot(retentiondata, aes(cohortday, retention)) +
      geom_line() +
      geom_point() +
      labs(title = paste(game, region_print, max_cohortday, 'Retention')) +
      theme(plot.title = element_text(hjust=.5))

    ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'retention.png', sep='_'))

    mydata$dau_pred <- getDAU(mydata, max_cohortday, retention_opt)

    plotdata <- mydata %>% select(days_since_launch, installs, dau, dau_pred) %>% rename(dau_actual=dau, dau_pred=dau_pred)
    plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='num_users')
    ggplot(plotdata, aes(days_since_launch, num_users, color=metric)) +
      geom_line() +
      labs(title = paste(game, region_print, max_cohortday, 'Installs and DAU')) +
      theme(plot.title = element_text(hjust=.5))

    ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'installs_dau.png', sep='_'))

    # Fit ARPDAU

    arpdau_fit <- fitARPDAU(mydata, retention_opt, max_cohortday, method='all')

    mydata$rev_pred <- getRev(arpdau_fit$arpdau, retention_opt, max_cohortday, mydata)
    gamedata <- tibble(game=game, region=region, cohortday=seq(0, max_cohortday), retention=retention_opt, arpdau=arpdau_fit$arpdau)

    ggplot(gamedata, aes(cohortday, arpdau)) +
      geom_point() +
      geom_line() +
      labs(title = paste(game, region_print, max_cohortday, 'ARPDAU')) +
      theme(plot.title = element_text(hjust=.5))

    ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'arpdau.png', sep='_'))

    plotdata <- mydata %>% select(days_since_launch, rev, rev_pred) %>% rename(rev_actual=rev)
    plotdata <- melt(plotdata, id.vars='days_since_launch', variable.name='metric', value.name='dollars')
    ggplot(plotdata, aes(days_since_launch, dollars, color=metric)) +
      geom_line() +
      labs(title = paste(game, region_print, max_cohortday, 'Revenue')) +
      theme(plot.title = element_text(hjust=.5))

    ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'revenue.png', sep='_'))

    save(mydata, gamedata, retention_fit, arpdau_fit, file=paste(game, region, startday, bounded, max_cohortday, 'fit.RData', sep='_'))

    if(split){

      # browser()
      retention_pre <- getRetention(end_cohortday, retention_fit$values[1], retention_fit$values[2])
      dau_pre <- getDAU(mydata, end_cohortday, retention_pre)

      arpdau_pre <- arpdau_fit$fn(seq(0, end_cohortday), arpdau_fit$values)
      rev_pre <- getRev(arpdau_pre, retention_pre, end_cohortday, mydata)

      installs_pre <- sum(mydata$installs)

      load(paste(game, region, end_cohortday, 'fit.RData', sep='_'))

      installs_combined <- sum(mydata$installs)
      installs_post <- installs_combined - installs_pre

      retention_combined <- retention_fit$retention
      retention_post <-  (installs_combined * retention_combined - installs_pre * retention_pre) / installs_post
      retention_post <- ifelse(retention_post < 0, 0, retention_post)

      arpdau_combined <- arpdau_fit$arpdau
      arpdau_post <-  (installs_combined * arpdau_combined - installs_pre * arpdau_pre) / installs_post
      arpdau_post <- ifelse(arpdau_post < 0, 0, arpdau_post)

      splitdata <- tibble(game=game, region=region, max_cohortday=max_cohortday, end_cohortday = end_cohortday,
                           cohortday = seq(0, end_cohortday), retention_pre = retention_pre, retention_post = retention_post,
                           retention_combined = retention_combined, arpdau_pre = arpdau_pre, arpdau_post = arpdau_post,
                           arpdau_combined = arpdau_combined)

      if(plot_flag){
        plotdata <- melt(splitdata, id.vars=c('game', 'region', 'max_cohortday', 'end_cohortday', 'cohortday'))

        ggplot(filter(plotdata, grepl('retention', variable)), aes(cohortday, value, color=variable)) +
          geom_line() +
          labs(title = paste(game, region_print, max_cohortday, end_cohortday, 'Retention Split')) +
          theme(plot.title = element_text(hjust=.5))

        ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'retention_split.png', sep='_'))

        ggplot(filter(plotdata, grepl('arpdau', variable)), aes(cohortday, value, color=variable)) +
          geom_line() +
          labs(title = paste(game, region_print, max_cohortday, end_cohortday, 'ARPDAU Split')) +
          theme(plot.title = element_text(hjust=.5))

        ggsave_default(paste(game, region, startday, bounded, max_cohortday, 'arpdau_split.png', sep='_'))
      }

      save(splitdata, file=paste(game, region, startday, bounded, max_cohortday, end_cohortday, 'split.RData', sep='_'))

      load(paste(game, region, max_cohortday, 'fit.RData', sep='_'))
      return(list(mydata=mydata, gamedata=gamedata, splitdata = splitdata, retention_fit=retention_fit, arpdau_fit=arpdau_fit))
    }

    return(list(mydata=mydata, gamedata=gamedata, retention_fit=retention_fit, arpdau_fit=arpdau_fit))

  }, error = function(e){
    print(paste('Error on', game, e, sep=' '))
  }, finally = {

  })

}


