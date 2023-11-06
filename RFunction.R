library('move2')
library('lubridate')
library('magrittr')
library('plyr')
library('dplyr')
library('ggplot2')
library('sf')


## The parameter "data" is reserved for the data object passed on from the previous app

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

# Showcase injecting app setting (parameter `year`)
rFunction = function(data, start = "05-19", end = "07-07", nfixes = 1, dayloss = 10, restrictive = FALSE,
                     int = 3, kcons_min = 5, kcons_max = 21, models = c("full","calfonly")) {
  
  
  # First step: preparing data by 
  # (a) trimming to analysis period of interest; for example, in the parturition analysis
  # dates were limited to May 19 to July 7 ; 
  # (b) guaranteeing a minimum number of location fixes a day (e.g. 1 for the parturiton analysis), 
  # and removing 
  # individuals that have too few; and 
  # (c) removing individuals with a data gap greater than some minimum threshold 
  # (e.g. 3 days for the parturiton analysis). 
  prepData <- function(df, start, end, nfixes = 1, dayloss = 10, restrictive = FALSE) {
    
    # Preparation of data: Need to create coordinates in metric system to be able to 
    # do the next steps and rename some columns
    df <- df %>% st_as_sf(df, coords=c("location.long", "location.lat"), crs = 4326) %>%
      st_transform(crs = 3857) %>% mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2]) %>%
      as.data.frame %>% rename(Time = timestamp) %>%
      mutate(geometry = NULL, Year = year(Time))
    
    if (any(names(df)=="tag.local.identifier")){
      df$ID <- data$tag.local.identifier
    } else if (any(names(df)=="tag.id")){
      df$ID <- df$tag.id
    }else if (any(names(df)=="individual_local_identifier")){
      df$ID <- df$individual_local_identifier
    } else {
      logger.info("There is no standard variable for animal ID in your data set, therefore track is used.")
      df$ID <- df$track
    }
    
    # just keep time series between defined start and end
    tempo <- df %>% as.data.frame %>% 
      mutate(Year = year(Time), 
             start = ymd(paste(Year, start), tz = tz(Time)),
             end = ymd(paste(Year, end), tz = tz(Time))) %>%
      subset(Time >= start & Time < (end + 3600 * 24)) %>% 
      mutate(start = as.Date(start), end = as.Date(end)) %>% droplevels
    
    # Remove individuals for which monitoring stopped before the end or 
    # began after the start
    if(restrictive){
      tempo <- tempo %>% ddply(c("ID", "Year"), function(x) x %>% 
                                 mutate(start.monitoring = as.Date(min(.$Time)), 
                                        end.monitoring = as.Date(max(.$Time)))) %>% 
        subset(start.monitoring == start & end.monitoring == end) %>% 
        mutate(ID_Year = as.factor(paste(ID, Year, sep ="_")))
    }
    
    if(dim(tempo)[1] > 0) {
      # calculate the dt between successive relocations for each individual and exclude 
      # those with less fixes per day than nfixes (e.g., 1 for parturition) and missing data for more than
      # dayloss (e.g., 3 for parturition) consecutive days
      tempo2 <- tempo %>% droplevels %>%
        ddply(c("ID", "Year"), 
              function(x) x %>% 
                arrange(Time) %>% 
                mutate(n = length(Time),
                       dt = ifelse(n < 3, NA,
                                   c(NA, as.integer(difftime(Time[2:length(Time)],Time[1:(length(Time)-1)],"hours")))),
                       n = NULL))

        tempo2 <- tempo2 %>% ddply(c("ID", "Year"), 
              function(x) x %>% 
                mutate(meandt = mean(.$dt, na.rm = TRUE), maxdt = max(.$dt, na.rm = TRUE))) %>% 
        subset(meandt < 24/nfixes & maxdt < dayloss*24) %>% droplevels %>% 
          mutate(start = NULL, end = NULL, start.monitoring = NULL, end.monitoring = NULL, 
                 ID_Year = NULL, dt = NULL, meandt = NULL, maxdt = NULL) %>% suppressWarnings
        
        # how many individuals have been removed?
        cat(paste0("Period clipped to ", start," - ", end, "\n"))
        cat(paste0("Number of excluded individuals-years: ",
                   length(unique(paste0(df$ID,df$Year)))-
                     length(unique(paste0(tempo2$ID,tempo2$Year))),
                   "\n"))
        
        removed_inds <- setdiff(unique(paste(df$ID,df$Year, sep = " in ")),
                                unique(paste(tempo2$ID,tempo2$Year, sep = " in ")))
        cat("Excluded individuals: \n")
        cat(paste0(removed_inds, "\n"))
        if(class(df)[1] == "sf"){
          tempo2 <- st_as_sf(tempo2, crs = st_crs(df))
        }
        return(tempo2)
    } 
    if(dim(tempo2)[1] == 0){
      stop("There is no individual in the dataset, try changing the fixrate, the allowed number of days with missing data or uncheck the 'restriction' argument")
    }
  }
  
  # Second step: get movement rate for all individuals by calculating speed 
  # from raw movement data containing x and y 
  # coordinates (in meters) with corresponding date and time
  # in POSIXct format. This will calculate the speed between subsequent 
  # relocations (i.e. of each step), as well as the middle time
  # of the step.
  getSpeed <- function(x, id.col = "ID", x.col = "x", y.col = "y", ...){
    
    if(class(x)[1] == "data.frame"){
      df_annotated <- x %>% mutate(X = get(x.col), Y = get(y.col)) %>%
        ddply(c(id.col), annotateWithSpeeds, ...)
    }
    if(class(x)[1] == "sf"){
      CRS = st_crs(x)
      x$x <- st_coordinates(x)[,1]
      x$y <- st_coordinates(x)[,2]
      df_annotated <- x %>% as.data.frame %>% ddply(c(id.col), annotateWithSpeeds, ...) %>% 
        st_as_sf(crs = CRS)
    }
    return(df_annotated)
  }
  
  annotateWithSpeeds <- function(df, time.col = "Time"){
    droplevels(df) %>% mutate(Time = get(time.col)) %>% dplyr::arrange(Time) %>% 
      mutate(z = x+1i*y, 
             dt = c(NA, difftime(Time[-1], Time[-length(Time)], units = "hours") %>% as.numeric), 
             sl = c(NA, Mod(diff(z))), 
             dhours = difftime(Time, Time[1], units = "hours") %>% as.numeric,
             speed = sl/dt)
  }  
  
  # Third step: Estimate calving
  # For this, we need functions as mnllMs and nlls (see below)
  
  # nll.post calculates the negative log-likelihood of the section of the time series
  # when the female is with a calf in both cases where the calf survived and the one where the calf died.
  # It is used in part to estimate k.
  # k represents the time it takes, in days, for the female to recover her normal movement.
  
  # function that calculate the negative likelihood after parturition
  nllPost <- function(par = c(log.beta.calf = NA, recovery = NA), alpha.mean, beta.mean, alpha.calf, dhours.b, speed.b){
    
    beta.calf <-  exp(par["log.beta.calf"])
    recovery <- par["recovery"]
    
    alpha.slope <- (alpha.mean - alpha.calf) / recovery
    
    beta.slope <- (beta.mean - beta.calf) / recovery
    
    alpha.hat <- alpha.calf + alpha.slope * dhours.b
    alpha.hat[dhours.b >  recovery] <- alpha.mean
    
    beta.hat <- beta.calf + beta.slope * dhours.b
    beta.hat[dhours.b > recovery] <- beta.mean
    
    -sum(dgamma(speed.b, rate = beta.hat, shape = alpha.hat, log = TRUE))
  }
  
  #  nllCalf calculates the negative log-likelihood of a female with a calf 
  #  that survived the early rearing period 
  #
  # The models assume that the mean speed is constant prior to the birth of a 
  # calf and after recovery or calf's death
  #
  # Once the calf is born the speed decrease close to 0.
  # Then the speed increases slowly until recovery or calf's death,
  # then increases immediately to reach the initial speed before parturition if 
  # the calf died.
  #
  # Once the mean speed reaches the value from before the birth of the calf,
  # it remains at this value.
  nllCalf <- function(df, BP1, kcons_min, kcons_max, PlotMe = FALSE) {
    
    # Divides the time series into two sections:
    # a: before the birth of the calf
    # b: after the birth of the calf
    df <- droplevels(subset(df %>% as.data.frame, is.na(speed)==FALSE))
    # speed vector
    speed <- df$speed
    speed.a <- df$speed[df$dhours <= BP1*24]
    speed.b <- df$speed[df$dhours > BP1*24]
    
    # hour vector (in hours difference between each value and the first one)
    dhours <- df$dhours
    dhours.a <- df$dhours[df$dhours <= BP1*24]
    dhours.b <- df$dhours[df$dhours > BP1*24]
    dhours.b <- dhours.b - dhours.b[1]
    
    # date time vector
    Time.b <- df$Time[df$dhours > BP1*24]
    
    # average speed before parturition
    speedmean.a <- mean(speed.a)
    
    # speed variance before parturition
    speedvar.a <- var(speed.a)
    
    
    ### negative log-likelihood before parturition (assuming speed = average speed before parturition)
    beta.hat <- speedmean.a / speedvar.a
    alpha.hat <- speedmean.a * beta.hat
    nlla <- -sum(dgamma(speed.a, rate = beta.hat, shape = alpha.hat, log = TRUE))
    
    
    ### optimize the function nllPost (after parturition)
    par0 <- c(log.beta.calf=0,
              recovery = mean(c(kcons_min, kcons_max))*24)
    alpha.calf <- ifelse(alpha.hat>1, 1, alpha.hat)
    mod <- optim(par0, nllPost, alpha.calf = alpha.calf, alpha.mean = alpha.hat, beta.mean = beta.hat, dhours.b = dhours.b, speed.b = speed.b,
                 hessian = TRUE, method = "L-BFGS-B",upper=c(log.beta.calf = Inf, recovery=kcons_max*24),
                 lower=c(log.beta.calf = log(beta.hat*2), recovery=kcons_min*24))
    # control=list(trace=3))
    
    # get the negative log-likelihood
    nllb <- mod$value
    # get the recovery (in days)
    recovery.hat <- mod$par["recovery"]/24
    # get the estimated parameters
    par <- mod$par
    par <- c(par, alpha.mean = alpha.hat, beta.mean=beta.hat, alpha.calf = alpha.calf)
    
    ### get the total negative log-likelihood of the calf model (nll before parturition and nll after parturition)
    total.nll = nlla + nllb
    
    
    ### function to calculate the fitted values
    
    fitted.calf <- function(par, dhours.a, dhours.b){
      alpha.mean <- par["alpha.mean"]
      beta.mean <-  par["beta.mean"]
      alpha.calf <- par["alpha.calf"]
      beta.calf <-  exp(par["log.beta.calf"])
      recovery <- par["recovery"]
      
      # speed before birth
      speed.hat.a <- rep(alpha.mean/beta.mean,length(dhours.a))
      
      # calculation of speed after birth
      alpha.slope.b <- (alpha.mean - alpha.calf) / recovery
      beta.slope.b <- (beta.mean - beta.calf) / recovery
      alpha.hat.b <- alpha.calf + alpha.slope.b * dhours.b
      alpha.hat.b[dhours.b >  recovery] <- alpha.mean
      beta.hat.b <- beta.calf + beta.slope.b * dhours.b
      beta.hat.b[dhours.b > recovery] <- beta.mean
      
      speed.hat.b <- alpha.hat.b / beta.hat.b
      
      speed.hat <- c(speed.hat.a, speed.hat.b)
      
      return(fit = speed.hat)
    }
    
    fit <- fitted.calf(par = par, dhours.a = dhours.a, dhours.b = dhours.b)
    
    ### function to plot the speed in function to hours with fitted values
    plotFit <- function(ID, dhours, speed, Time, Time.b, alpha.mean, beta.mean, speed.hat, recovery){
      plot(Time, speed, type = "l", main = paste("Individual", unique(ID), sep = " ", bty = "l"))
      lines(Time, speed.hat, col = 2, lwd = 2)
      text(Time[which(Time==Time.b[1])-1]+2*3600*24,alpha.mean/beta.mean+600,
           labels = paste("Calving = ", substr(Time[which(Time==Time.b[1])-1],1,10),sep=""),srt=90,font=3, col="red")
      text(mean(Time),max(speed),
           labels = paste(paste("recovery = ", round(recovery/24,0),sep=""), "days", sep = " "),font=3)
    }
    if (PlotMe)
      plotFit(ID = df$ID, speed = speed, dhours = dhours, alpha.mean = par["alpha.mean"], recovery = par["recovery"],
              Time = df$Time, Time.b = Time.b, beta.mean = par["beta.mean"], speed.hat = fit)
    
    
    return(list(Log.Likelihood = -total.nll, par=par, fit = fit))
  }
  
  #' @describeIn nll.calf Likelihood of Female having a calf that died
  
  nllCalfDeath <- function(df, BP1, BP2, kcons_min, kcons_max, PlotMe = FALSE){
    
    # Divides the time series into three sections:
    # a: before the birth of the calf
    # b: after the birth of the calf but before it dies
    # c: after the death of the calf
    
    df <- droplevels(subset(df %>% as.data.frame, is.na(speed) == FALSE))
    # speed vector
    speed <- df$speed
    speed.a <- df$speed[df$dhours <= BP1*24]
    speed.b <- df$speed[df$dhours > BP1*24 & df$dhours <= BP2*24]
    speed.c <- df$speed[df$dhours > BP2*24]
    
    # hour vector (in hours difference between each value and the first one)
    dhours <- df$dhours
    dhours.a <- df$dhours[df$dhours <= BP1*24]
    dhours.b <- df$dhours[df$dhours > BP1*24 & df$dhours <= BP2*24]
    dhours.b <- dhours.b - dhours.b[1]
    dhours.c <- df$dhours[df$dhours > BP2*24]
    dhours.c <- dhours.c - dhours.c[1]
    
    # date time vector
    Time.b <- df$Time[df$dhours > BP1*24 & df$dhours <= BP2*24]
    Time.c <- df$Time[df$dhours > BP2*24]
    
    # average speed before parturition
    speedmean.a <- mean(speed.a)
    
    # speed variance before parturition
    speedvar.a <- var(speed.a)
    # negative log-likelihood before parturition (assuming speed = average speed before parturition)
    beta.hat <- speedmean.a / speedvar.a
    alpha.hat <- speedmean.a * beta.hat
    nlla <- -sum(dgamma(speed.a, rate = beta.hat, shape = alpha.hat, log = TRUE))
    nllc <- -sum(dgamma(speed.c, rate = beta.hat, shape = alpha.hat, log = TRUE))
    
    # optimize the function nllPost (after parturition but before calf death)
    par0 <- c(log.beta.calf=0,
              recovery = mean(c(kcons_min, kcons_max))*24)
    alpha.calf <- ifelse(alpha.hat>1, 1, alpha.hat)
    mod <- optim(par0, nllPost, alpha.calf = alpha.calf, alpha.mean = alpha.hat, beta.mean = beta.hat, dhours.b = dhours.b, speed.b = speed.b,
                 hessian = TRUE, method = "L-BFGS-B",upper=c(log.beta.calf = Inf, recovery=kcons_max*24),
                 lower=c(log.beta.calf = log(beta.hat*2), recovery=kcons_min*24))
    
    # get the negative log-likelihood
    nllb <- mod$value
    # get the recovery (in days)
    recovery.hat <- mod$par["recovery"]/24
    # get the estimated parameters
    par <- mod$par
    par <- c(par, alpha.mean = alpha.hat, beta.mean=beta.hat, alpha.calf = alpha.calf)
    
    # Getting the total nll for the whole time series
    total.nll <- sum(nlla, nllb, nllc)
    
    # function to calculate fitted values
    fitted.calfdeath <- function(par, dhours.a, dhours.b, dhours.c){
      alpha.mean <- par["alpha.mean"]
      beta.mean <-  par["beta.mean"]
      alpha.calf <- par["alpha.calf"]
      beta.calf <-  exp(par["log.beta.calf"])
      recovery <- par["recovery"]
      
      # speed before birth and after calf death
      speed.hat.a <- rep(alpha.mean/beta.mean,length(dhours.a)) %>% as.numeric
      speed.hat.c <- rep(alpha.mean/beta.mean,length(dhours.c)) %>% as.numeric
      
      # calculation of speed after birth but before calf death
      alpha.slope.b <- (alpha.mean - alpha.calf) / recovery
      beta.slope.b <- (beta.mean - beta.calf) / recovery
      alpha.hat.b <- alpha.calf + alpha.slope.b * dhours.b
      alpha.hat.b[dhours.b >  recovery] <- alpha.mean
      beta.hat.b <- beta.calf + beta.slope.b * dhours.b
      beta.hat.b[dhours.b > recovery] <- beta.mean
      
      speed.hat.b <- alpha.hat.b / beta.hat.b
      
      speed.hat <- c(speed.hat.a, speed.hat.b, speed.hat.c)
      
      return(fit = speed.hat)
    }
    
    fit <- fitted.calfdeath(par = par, dhours.a = dhours.a, dhours.b = dhours.b, dhours.c = dhours.c)
    
    ## function to plot the speed in function to hours with fitted values
    plotFit <- function(ID, dhours, speed, Time, Time.b, Time.c, alpha.mean, beta.mean, speed.hat, recovery){
      plot(Time, speed, type = "o", main = paste("Individual", unique(ID), sep = " "))
      lines(Time, speed.hat, col = 2, lwd = 2)
      text(Time[which(Time==Time.b[1])-1]+2*3600*24,alpha.mean/beta.mean+600,
           labels = paste("Calving = ", substr(Time[which(Time==Time.b[1])-1],1,10),sep=""),srt=90,font=3, col="red")
      text(Time[which(Time==Time.b[length(Time.b)])]-2*3600*24, alpha.mean/beta.mean+600,
           labels = paste("Calf death = ", substr(Time[which(Time==Time.c[length(Time.c)])],1,10),sep=""),
           srt=90,font=3, col="red")
      # text(mean(Time),max(speed),labels = paste(paste("recovery = ", round(recovery/24,0),sep=""), "days", sep = " "),font=3)
    }
    if (PlotMe)
      plotFit(ID = df$ID, speed = speed, Time = df$Time, Time.b = Time.b, Time.c = Time.c,
              alpha.mean = par["alpha.mean"], beta.mean = par["beta.mean"], speed.hat = fit)
    
    return(list(Log.Likelihood = -total.nll, par=par, fit = fit))
  }

  
  # mnll2M minimize the negative log-likelihood of the no calf and calf models only
  mnll2M <- function(df, int, kcons_min, kcons_max){
    
    speed <- na.omit(df$speed)
    speed[speed == 0] <- 1
    speedmean <- mean(speed)
    speedvar <- var(speed)
    
    Time <- na.omit(df$Time)
    dhours <- na.omit(df$dhours)
    
    # Results for model comparison
    results <- matrix(NA, 1, ncol=6)
    colnames(results) <- c("n", "mnllNoCalf","mnllCalf",
                           "AIC.nocalf", "AIC.calf","Best.Model")
    
    # BPs and actual date and time and recovery (in days)
    BPs <- data.frame(matrix(NA, 1, ncol=3))
    colnames(BPs) <- c("date.BP1.calf", "BP1.calf", "recovery.calf")
    
    # Sample size
    results[1,"n"] <- length(speed)
    
    ##
    # M0: No calf
    # This model assumes that the movement pattern is constant for the whole time series
    # and thus the mean speed is constant.
    # The only parameters estimated are beta and alpha, which is the inverse of the rate.
    # It has a analytical solution and
    # thus we can easily get the minimum negative log likelihood (mnll)
    # negative log-likelihood before parturition (assuming speed = average speed before parturition)
    beta.mean <- speedmean / speedvar
    alpha.mean <- speedmean * beta.mean
    MLL0  <- -sum(dgamma(speed, rate = beta.mean, shape = alpha.mean, log = TRUE))
    results[1,"mnllNoCalf"] <- -MLL0
    
    ##
    # M1: Calf survived
    BPmax <- max(dhours)/24
    
    if(BPmax-int<=0){
      results[1,"mnllCalf"] <- NA # maximum log-likelihood of the model
      BPs[["BP1.calf"]] <- NA # mle of BP1 in terms of days
      
      BPs[["date.BP1.calf"]] <- NA #mle of BP1 in real date and time
      
      BPs[["recovery"]] <- NA
    } else {
      BP1s  <- (int:(BPmax-int)) # All possible BP
      ll1s <- rep(NA,length(BP1s))
      trytogetbp <- function (bp) {
        fit <- try(nllCalf(df = df, BP = bp, kcons_min = kcons_min, kcons_max = kcons_max),silent=TRUE)
        if (!inherits(fit, "try-error"))
          return(fit$Log.Likelihood) else return(NA)}
      
      ll1s <- sapply(BP1s, trytogetbp)
      ll1s[ll1s %in% c(-Inf, Inf)] <- NA
      MLL1 <- which.max(ll1s)
      
      if(length(MLL1)==0){
        results[1,"mnllCalf"] <- NA
        BPs[["BP1.calf"]] <- NA
        BPs[["date.BP1.calf"]] <- NA
        BPs[["recovery.calf"]] <- NA
      } else {
        results[1,"mnllCalf"] <- ll1s[[MLL1]] # maximum log-likelihood of the model
        BPs[["BP1.calf"]] <- BP1s[MLL1] # BP1 in terms of days
        
        BPs[["date.BP1.calf"]] <- Time[which.min(abs(dhours-(BPs[["BP1.calf"]]*24)))] # BP1 in real date and time
        
        BPs[["recovery.calf"]] <- round(nllCalf(df = df, BP = BPs[["BP1.calf"]], kcons_min = kcons_min, kcons_max = kcons_max)$par["recovery"]/24)
      }
    }
    
    # Calculate AIC and compare models
    results[[1,"AIC.nocalf"]] <- 2*(-as.numeric(results[[1,"mnllNoCalf"]]) + 2)
    results[[1,"AIC.calf"]] <- ifelse(is.na(results[[1,"mnllCalf"]]) == FALSE & results[[1,"mnllCalf"]] != 0, 2*(-as.numeric(results[[1,"mnllCalf"]]) + 5), NA)
    results[[1,"Best.Model"]] <- substr(names(which.min(results[,4:5])),5,nchar(names(which.min(results[,4:5]))))
    
    
    return(list(results=results, BPs=BPs))
    
  }
  
  # mnll3M minimize the negative log-likelihood of the no calf, the calf and calf death models
  mnll3M <- function(df, int, kcons_min, kcons_max){
    
    speed <- na.omit(df$speed)
    speed[speed == 0] <- 1
    speedmean <- mean(speed)
    speedvar <- var(speed)
    
    Time <- na.omit(df$Time)
    dhours <- na.omit(df$dhours)
    
    # Results for model comparison
    results <- matrix(NA, 1, ncol=8)
    colnames(results) <- c("n", "mnllNoCalf","mnllCalf","mnllCalfDeath",
                           "AIC.nocalf", "AIC.calf", "AIC.calfdeath","Best.Model")
    
    # BPs and actual date and time and recovery (in days)
    BPs <- data.frame(matrix(NA, 1, ncol=7))
    colnames(BPs) <- c("date.BP1.calf", "date.BP1.calfdeath", "date.BP2.calfdeath",
                       "BP1.calf", "BP1.calfdeath", "BP2.calfdeath","recovery.calf")
    
    # Sample size
    results[1,"n"] <- length(speed)
    
    ##
    # M0: No calf
    # This model assumes that the movement pattern is constant for the whole time series
    # and thus the mean speed is constant.
    # The only parameters estimated are beta and alpha, which is the inverse of the rate.
    # It has a analytical solution and
    # thus we can easily get the minimum negative log likelihood (mnll)
    # negative log-likelihood before parturition (assuming speed = average speed before parturition)
    beta.mean <- speedmean / speedvar
    alpha.mean <- speedmean * beta.mean
    MLL0  <- -sum(dgamma(speed, rate = beta.mean, shape = alpha.mean, log = TRUE))
    results[1,"mnllNoCalf"] <- -MLL0
    
    # M1: Calf survived
    BPmax <- max(dhours)/24
    
    if(BPmax-int<=0){
      results[1,"mnllCalf"] <- NA # maximum log-likelihood of the model
      BPs[["BP1.calf"]] <- NA # mle of BP1 in terms of days
      
      BPs[["date.BP1.calf"]] <- NA #mle of BP1 in real date and time
      
      BPs[["recovery"]] <- NA
    } else {
      BP1s  <- (int:(BPmax-int)) # All possible BP
      ll1s <- rep(NA,length(BP1s))
      trytogetbp <- function (bp) {
        fit <- try(nllCalf(df = df, BP = bp, kcons_min = kcons_min, kcons_max = kcons_max),silent=TRUE)
        if (!inherits(fit, "try-error"))
          return(fit$Log.Likelihood) else return(NA)}
      
      ll1s <- sapply(BP1s, trytogetbp)
      ll1s[ll1s %in% c(-Inf, Inf)] <- NA
      MLL1 <- which.max(ll1s)
      
      if(length(MLL1)==0){
        results[1,"mnllCalf"] <- NA
        BPs[["BP1.calf"]] <- NA
        BPs[["date.BP1.calf"]] <- NA
        BPs[["recovery.calf"]] <- NA
      } else {
        results[1,"mnllCalf"] <- ll1s[[MLL1]] # maximum log-likelihood of the model
        BPs[["BP1.calf"]] <- BP1s[MLL1] # BP1 in terms of days
        
        BPs[["date.BP1.calf"]] <- Time[which.min(abs(dhours-(BPs[["BP1.calf"]]*24)))] # BP1 in real date and time
        
        BPs[["recovery.calf"]] <- round(nllCalf(df = df, BP = BPs[["BP1.calf"]], kcons_min = kcons_min, kcons_max = kcons_max)$par["recovery"]/24)
      }
    }
    
    # M2: Calf lost
    # Getting all possible combination of BPs
    # Note that BP are constrained to be int number of non-missing steps apart.
    # To make the code run faster, the BPs are also limited to be less than
    # maximum number of steps it takes for the female to recover her movement apart
    if(BPmax-int <=0){
      results[1,"mnllCalfDeath"] <- NA # mnll2
      
      BPs[["BP1.calfdeath"]] <- NA #mle of BP1.calfdeath in days
      BPs[["BP2.calfdeath"]] <- NA #mle of BP2.calfdeath in days
      BPs[["date.BP1.calfdeath"]] <- NA  #mle of BP1 in real date and time
      BPs[["date.BP2.calfdeath"]] <- NA  #mle of BP2 in real date and time
    } else {
      BP2s <- expand.grid(list(BP1=int:(BPmax-int),BP2=int:(BPmax-int)))
      BP2s <- BP2s[(BP2s$BP2-BP2s$BP1)>=int,]
      BP2s <- BP2s[(BP2s$BP2-BP2s$BP1) < kcons_max,]
      
      if(dim(BP2s)[1] == 0)
      {
        results[1,"mnllCalfDeath"] <- NA # mnll2
        
        BPs[["BP1.calfdeath"]] <- NA # BP1.calfdeath in days
        BPs[["BP2.calfdeath"]] <- NA # BP2.calfdeath in days
        BPs[["date.BP1.calfdeath"]] <- NA  # BP1 in real date and time
        BPs[["date.BP2.calfdeath"]] <- NA  # BP2 in real date and time
      } else{
        trytogetbp2 <- function (bp1,bp2) {
          fit <- try(nllCalfDeath(df = df, BP1 = bp1, BP2 = bp2, kcons_min = kcons_min, kcons_max = kcons_max),silent=T)
          if (!inherits(fit, "try-error"))
            return(list(lls = fit$Log.Likelihood, recovery = fit$par[["recovery"]])) else return(list(lls = NA, recovery = NA))}
        
        ll2s <- rep(NA,length(BP2s$BP1))
        recoveries <- rep(NA, length(BP2s$BP1))
        for(i in 1:length(BP2s$BP1)){
          ll2s[i] <- trytogetbp2(bp1 = BP2s$BP1[i], bp2 = BP2s$BP2[i])$lls
          recoveries[i] <- trytogetbp2(bp1 = BP2s$BP1[i], bp2 = BP2s$BP2[i])$recovery
        }
        BP2s <- cbind(BP2s,ll2s,recoveries)
        BP2s$recoveries <- BP2s$recoveries/24
        BP2s <- BP2s[(BP2s$BP2-BP2s$BP1)<BP2s$recoveries,]
        BP2s$ll2s[BP2s$ll2s %in% c(-Inf, Inf)] <- NA
        MLL2 <- which.max(BP2s$ll2s)
        
        if(length(MLL2)==0){
          results[1,"mnllCalfDeath"] <- NA
          BPs[["BP1.calfdeath"]] <- NA
          BPs[["BP2.calfdeath"]] <- NA
          BPs[["date.BP1.calfdeath"]] <- NA
          BPs[["date.BP2.calfdeath"]] <- NA
        } else {
          results[1,"mnllCalfDeath"] <- BP2s$ll2s[[MLL2]] # mnll2
          
          BPs[["BP1.calfdeath"]] <- BP2s[MLL2,"BP1"] # BP1.calfdeath in days
          BPs[["BP2.calfdeath"]] <- BP2s[MLL2,"BP2"] # BP2.calfdeath in days
          BPs[["date.BP1.calfdeath"]] <- Time[which.min(abs(dhours-(BPs[["BP1.calfdeath"]]*24)))]  # BP1 in real date and time
          BPs[["date.BP2.calfdeath"]] <- Time[which.min(abs(dhours-(BPs[["BP2.calfdeath"]]*24)))]  # BP2 in real date and time
        }
      }
    }
    
    # Calculate AIC and compare models
    results[[1,"AIC.nocalf"]] <- 2*(-as.numeric(results[[1,"mnllNoCalf"]]) + 2)
    results[[1,"AIC.calf"]] <- ifelse(is.na(results[[1,"mnllCalf"]]) == FALSE & results[[1,"mnllCalf"]] != 0,2*(-as.numeric(results[[1,"mnllCalf"]]) + 5),NA)
    results[[1,"AIC.calfdeath"]] <- ifelse(is.na(results[[1,"mnllCalfDeath"]])== FALSE & results[[1,"mnllCalfDeath"]] != 0, 2*(-as.numeric(results[[1,"mnllCalfDeath"]]) + 6),NA)
    
    results[[1,"Best.Model"]] <- substr(names(which.min(results[,5:7])),5,nchar(names(which.min(results[,5:7]))))
    
    
    return(list(results=results, BPs=BPs))
    
  }
  
  # Function that determine calving status of a female (i.e. no calf, with a calf, calf lost)
  # the calving date and the calf death date (if any).
  # This is an adaptation of the individual based method developed by Demars et al. (2013),
  # which has proven good reliability to estimate calving status and calving date for females
  # from the Western Arctic Herd in a previous study (Cameron et al. 2018).
  # However, we adapted this method to be able to infer parturition based on the 
  # female movement rate through time.
  estimateCalving <- function (df, int, kcons_min, kcons_max, models = c("full","calfonly")){
    
    df2 <- df %>% as.data.frame %>% mutate(ID_Year = as.factor(paste(ID, year(Time), sep = "_")))
    
    # create the returned dataframes
    coeffs <- data.frame()
    par <- data.frame()
    results.summary <- data.frame()
    
    # run the mnll3 for each individual to obtain MLE, AIC for the 3 models
    for(i in unique(df2$ID_Year))  {
      print(i)
      
      temp=droplevels(subset(df2,ID_Year==i))
      temp <- temp[order(temp$Time),]
      ID_Year <- unique(temp$ID_Year)
      ID <- unique(temp$ID)
      Year <- unique(year(temp$Time))
      
      speed <- na.omit(temp$speed)
      speedmean <- mean(speed)
      speedvar <- var(speed)
      
      # run the mnll3M or mnll2M function for the individual
      if(models == "full"){
        results <- mnll3M(temp, int, kcons_min, kcons_max)
        # exctract the parameters, AICs...
        results.data.temp=data.frame(ID_Year = as.factor(as.character(ID_Year)), ID=ID, Year = Year,
                                     Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                     M0.AIC=as.numeric(results$results[1,"AIC.nocalf"]),
                                     Mcalf.AIC=as.numeric(results$results[1,"AIC.calf"]),
                                     Mcalfdeath.AIC=as.numeric(results$results[1,"AIC.calfdeath"]),
                                     M0.mnll=as.numeric(results$results[1,"mnllNoCalf"]),
                                     Mcalf.mnll=as.numeric(results$results[1,"mnllCalf"]),
                                     Mcaldeath.mnll=as.numeric(results$results[1,"mnllCalfDeath"]))
        
        
        ## results table
        results.summary.temp <- data.frame(ID=ID, Year = Year, Best.Model=results.data.temp$Best.Model,
                                           calving.date=ifelse(as.character(results$results[1,"Best.Model"]) == "calf",
                                                               as.POSIXct(results$BPs[["date.BP1.calf"]]),
                                                               ifelse(as.character(results$results[1,"Best.Model"]) == "calfdeath",
                                                                      as.POSIXct(results$BPs[["date.BP1.calfdeath"]]),NA)),
                                           mort.date=ifelse(as.character(results$results[1,"Best.Model"])=="calfdeath",
                                                            as.POSIXct(results$BPs[["date.BP2.calfdeath"]]),NA),
                                           Recovery=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                           results$BPs$recovery.calf,NA))
        results.summary.temp$calving.date <- as.POSIXct(results.summary.temp$calving.date,
                                                        origin="1970-01-01", tz="GMT")
        results.summary.temp$mort.date <- as.POSIXct(results.summary.temp$mort.date,
                                                     origin="1970-01-01", tz="GMT")
        calving.date.julian <- yday(results.summary.temp$calving.date)
        
        # Estimated parameters (alpha.mean, beta.mean, alpha.calf, beta.calf and recovery time)
        
        {
          beta.0 <- speedmean / speedvar
          alpha.0 <- speedmean * beta.0
          parnocalf <- data.frame(ID = ID, Year = Year, alpha.0 = alpha.0, beta.0 = beta.0)
          fit.values.nocalf <- rep(alpha.0/beta.0, length(speed))
          results.summary.temp$calf.loc.x <- NA
          results.summary.temp$calf.loc.y <- NA
        }
        
        
        {
          fit.calf <- try(nllCalf(temp, BP = results$BPs[["BP1.calf"]], kcons_min = kcons_min, kcons_max = kcons_max), silent = TRUE)
          if(!inherits(fit.calf, 'try-error')){
            parcalf <- data.frame(alpha.mean1=fit.calf$par[["alpha.mean"]], beta.mean1=fit.calf$par[["beta.mean"]],
                                  alpha.calf1=fit.calf$par[["alpha.calf"]], beta.calf1=exp(fit.calf$par[["log.beta.calf"]]),
                                  BP.calf1 = results$BPs[["BP1.calf"]], calving.date1 = results$BPs[["date.BP1.calf"]], 
                                  recovery1=fit.calf$par[["recovery"]])
            fit.values.calf <- fit.calf$fit } 
          if (inherits(fit.calf, 'try-error')) {
            parcalf <- data.frame(alpha.mean1=NA, beta.mean1=NA,
                                  alpha.calf1=NA, beta.calf1=NA,
                                  BP.calf1 = NA, calving.date1 = NA, 
                                  recovery1=NA)
          }
        }
        if(results.summary.temp$Best.Model == 'calf'){
          results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("x")][1]
          results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("y")][1]
        }
        
        
        
        {
          fit.calfdeath <- try(nllCalfDeath(temp, BP1 = results$BPs[["BP1.calfdeath"]], BP2 = results$BPs[["BP2.calfdeath"]], kcons_min = kcons_min, kcons_max = kcons_max), silent = TRUE)
          if(!inherits(fit.calfdeath, 'try-error')){
            parcalfdeath <- data.frame(alpha.mean2=fit.calfdeath$par[["alpha.mean"]], beta.mean2=fit.calfdeath$par[["beta.mean"]],
                                       alpha.calf2=fit.calfdeath$par[["alpha.calf"]], beta.calf2=exp(fit.calfdeath$par[["log.beta.calf"]]),
                                       BP.calf2 = results$BPs[["BP1.calfdeath"]], calving.date2 = results$BPs[["date.BP1.calfdeath"]], 
                                       BP.calfdeath2 = results$BPs[["BP2.calfdeath"]], calfdeath.date2 = results$BPs[["date.BP2.calfdeath"]], 
                                       recovery2=fit.calfdeath$par[["recovery"]])
            fit.values.calfdeath <- fit.calfdeath$fit } 
          if (inherits(fit.calfdeath, "try-error")){
            parcalfdeath <- data.frame(alpha.mean2=NA, beta.mean2=NA,
                                       alpha.calf2=NA, beta.calf2=NA,
                                       BP.calf2 = NA, calving.date2 = NA, 
                                       BP.calfdeath2 = NA, calfdeath.date2 = NA, 
                                       recovery2=NA)
          }}
        
        if(results.summary.temp$Best.Model == 'calfdeath'){
          results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("x")][1]
          results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("y")][1]
        }
        
        par <- rbind(par, cbind(parnocalf, parcalf, parcalfdeath))
        coeffs <- rbind(coeffs,results.data.temp[1,])
        results.summary=rbind(results.summary,results.summary.temp)
        
      } else if(models == "calfonly"){
        # run the mnll2M function for the individual
        results <- mnll2M(temp, int, kcons_min, kcons_max)
        # exctract the parameters, AICs...
        results.data.temp=data.frame(ID_Year = as.factor(as.character(ID_Year)), ID=ID, Year = Year,
                                     Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                     M0.AIC=as.numeric(results$results[1,"AIC.nocalf"]),
                                     Mcalf.AIC=as.numeric(results$results[1,"AIC.calf"]),
                                     M0.mnll=as.numeric(results$results[1,"mnllNoCalf"]),
                                     Mcalf.mnll=as.numeric(results$results[1,"mnllCalf"]))
        
        ## results table
        results.summary.temp <- data.frame(ID=ID, Year = Year, Best.Model=as.factor(as.character(results$results[1,"Best.Model"])),
                                           calving.date=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                               as.POSIXct(results$BPs[["date.BP1.calf"]]), NA),
                                           Recovery=ifelse(as.character(results$results[1,"Best.Model"])=="calf",
                                                           results$BPs[["recovery.calf"]], NA))
        results.summary.temp$calving.date <- as.POSIXct(results.summary.temp$calving.date,
                                                        origin="1970-01-01", tz="GMT")
        calving.date.julian <- yday(results.summary.temp$calving.date)
        
        
        
        # Estimated parameters (alpha.mean, beta.mean, alpha.calf, beta.calf and recovery time)
        {
          beta.0 <- speedmean / speedvar
          alpha.0 <- speedmean * beta.0
          parnocalf <- data.frame(ID = ID, Year = Year, alpha.0 = alpha.0, beta.0 = beta.0)
          fit.values.nocalf <- rep(alpha.0/beta.0, length(speed))
          results.summary.temp$calf.loc.x <- NA
          results.summary.temp$calf.loc.y <- NA
        }
        
        
        {
          fit.calf <- try(nllCalf(temp, BP = results$BPs[["BP1.calf"]], kcons_min = kcons_min, kcons_max = kcons_max), silent = TRUE)
          if(!inherits(fit.calf, "try-error")){
            parcalf <- data.frame(alpha.mean1=fit.calf$par[["alpha.mean"]], beta.mean1=fit.calf$par[["beta.mean"]],
                                  alpha.calf1=fit.calf$par[["alpha.calf"]], beta.calf1=exp(fit.calf$par[["log.beta.calf"]]),
                                  BP.calf1 = results$BPs[["BP1.calf"]], calving.date1 = results$BPs[["date.BP1.calf"]], 
                                  recovery1=fit.calf$par[["recovery"]])
            fit.values.calf <- fit.calf$fit
          }
          if(inherits(fit.calf, "try-error")){
            parcalf <- data.frame(alpha.mean1=NA, beta.mean1=NA,
                                  alpha.calf1=NA, beta.calf1=NA,
                                  BP.calf1 = NA, calving.date1 = NA, 
                                  recovery1=NA)
          }
          
          results.summary.temp$calf.loc.x <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("x")][1]
          results.summary.temp$calf.loc.y <- temp[which.min(abs(as.numeric(difftime(temp$Time,results.summary.temp$calving.date,units = "secs"))))+1,
                                                  c("y")][1]
        }
        
        par <- rbind(par, cbind(parnocalf, parcalf))
        coeffs <- rbind(coeffs,results.data.temp[1,])
        results.summary=rbind(results.summary,results.summary.temp)
      }
      
      ####### Making plots of results ##########
      temp <- droplevels(subset(temp, is.na(speed)==FALSE))
      temp <- temp[order(temp$Time),]
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "nocalf") {   #if the best model is the "didn't calve model", plot a flat line
        
        p1 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background = element_blank()) +  #Sets background to white
          geom_hline(aes(yintercept=fit.values.nocalf, group=1, colour=2), show.legend=FALSE, size = 1) +
          labs(x = "Date", y = "Speed (m.h-1)", title = paste("No calf model for",ID_Year, sep = " ")) +
          theme(axis.line.x = element_line(linewidth = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y = element_line(linewidth = .5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title = element_text(size = 12,face = "bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500))
        print(p1)
      } # END plot of the no calf model
      
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "calf"){   #if the best model is the "calf model", plot a single break point
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calf"]])
        
        ## Plotting ##
        p2 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+2*24*3600),label=calve,y=1500),angle=90,size=4,fontface="italic") + #Labels the calving line with calving date
          labs(x="Date",y="Speed (m.h-1)",title=paste("Calf model for",ID_Year,sep = " " )) +
          theme(axis.line.x=element_line(linewidth=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(linewidth=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=12,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(as.POSIXct(Time), fit.values.calf, colour=1, group=1), show.legend=FALSE, size=1) #plots predicted values
        print(p2)
        } # END plot of the calf model
      
      
      if (coeffs$Best.Model[coeffs$ID_Year == i] == "calfdeath"){  #if the best model is the "calved then calf lost" model, plot 2 breakpoints
        ## Settings for line commands ##
        calve=as.POSIXct(results$BPs[["date.BP1.calfdeath"]])
        calf.loss=as.POSIXct(results$BPs[["date.BP2.calfdeath"]])
        
        ## Plotting ##
        p3 <- ggplot(temp,aes(Time,speed,group=1)) +
          geom_line() +
          theme(panel.background=element_blank()) +  #Sets background to white
          geom_vline(xintercept=as.numeric(calve),linetype=4,colour="black") + #break point at calving event
          geom_text(aes(x=(calve+2*24*3600),label=calve,y=1500),angle=90,size=4,fontface="italic") + #Labels the calving line with calving date
          geom_vline(xintercept=as.numeric(calf.loss),linetype=4,colour="black") + #break point at calf loss event
          geom_text(aes(x=(calf.loss-2*24*3600),label=calf.loss,y=1500),angle=90,size=4,fontface="italic") + #Labels calf loss
          labs(x="Date",y="Speed (m.h-1)",title=paste("Calf death model for",ID_Year,sep = " " )) +
          theme(axis.line.x=element_line(linewidth=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(axis.line.y=element_line(linewidth=.5,colour = "black",linetype = "solid")) + #add axis lines
          theme(plot.title=element_text(size=12,face="bold",margin = margin(10,0,10,0))) +
          ylim(c(0,2500)) +
          geom_line(aes(as.POSIXct(Time), fit.values.calfdeath, colour=1, group=1), show.legend=FALSE, size=1) #plots predicted values
       print(p3)
        } # End Plot of the calf death model
      
    } # END for loop for each individual
    
    return(list(statistics = coeffs, par = par, results = results.summary))
    
  }
  
  # Run the functions to estimate calving
  prepped_data <- data %>% prepData(start = start, end = end, nfixes = nfixes, 
                                    dayloss = 3, restrictive = restrictive) %>% 
    getSpeed(id.col = "ID", x.col = "x", y.col = "y", time.col = "Time")
  
  pdf(file=appArtifactPath("Calving_plots.pdf"))
  calving_results <- estimateCalving(prepped_data, int = int, kcons_min = kcons_min, 
                                     kcons_max = kcons_max, models = models)
  dev.off()
  
  statistics <- calving_results$statistics
  write.csv(statistics, file = appArtifactPath("calving_statistics.csv"))
  
  parameters <- calving_results$par
  write.csv(parameters, file = appArtifactPath("calving_models_parameters.csv"))
  
  calving_results <- calving_results$results
  write.csv(calving_results, file = appArtifactPath("calving_results.csv"))
  
  return(data)
}
