merge_mult <- function(list_of_dfs, by){
  #check if all dfs contain the column to merge over
  for(df in list_of_dfs){
    stopifnot(by %in% colnames(df))
  }
  
  # initiate merged df by 1st entry of list_of_dfs
  merged_df <- list_of_dfs[1]
  # delete first df since initiated
  list_of_dfs <- list_of_dfs[-1]
  
  # loop over remaining dfs
  while(length(list_of_dfs)>0){
    merged_df <- merge(merged_df, list_of_dfs[1], by=by)
    list_of_dfs <- list_of_dfs[-1]
  }
  
  return(merged_df)
}

#merge_mult(list_of_dfs=list(WikiDaily_Id, FactorReturns, EuroStoxx), by=c("date"))


#abs_ret <- function(ret){
#  return <- (tail(ret, 1) - ret[1])/ret[1]
#  return(return)
#}

#abs_ret(WikiDaily_merge$EuroStoxx)

#######

# implementation of the annualized return

#######

ann_ret <- function(prices, days_ret, returns=FALSE){
  # flag for returns/prices is propagated through the geom_ret function
  geom_ret <- geom_ret(prices, returns = returns)
  # use dates to calculate fractions of years. 
  # dependent on the global variable "days_ret" (i.e. the specified number of days per year)
  # if data other than daily were to be used -> days_ret has to be changed
  ann_ret <- ((1+geom_ret)^(days_ret))-1
  return(ann_ret)
}

#ann_ret(prices= WikiDaily_merge$ret, days_ret=days_ret, start_date = start_date, end_date=end_date, returns = TRUE)

#####

# implementation of the geometric return

#####

geom_ret <- function(prices, returns=FALSE){
  #flag if user provided prices or returns
  if(returns==FALSE){
    # take ratio of last and first prices
    ratio <- (tail(prices, 1) / prices[1])
    # number of periods
    len <- length(prices)-1
    # geometric return
    geom_ret <- ratio^(1/len)-1
    # returns a DAILY return
    return(geom_ret)
  }else{
    # if returns are provided the geometric return is calculated as the product of the individual returns
    len <- length(prices)-1
    geom_ret <- (prod(1+prices[-1]))^(1/len)-1
    # returns a DAILY return
    return(geom_ret)
  }
}
#geom_ret(prices = WikiDaily_merge$close)

geom_ret_shrink <- function(prices, 
                            rf=NULL,
                            returns=FALSE, 
                            start_date, 
                            end_date,
                            time_unit=list("years", "months", "days"),
                            shrink = list("None", "exponential"), 
                            expo_factor = 0.5
                            ){
  #flag if user provided prices or returns
  if(returns==FALSE){
    return <- geom_ret(prices, returns=FALSE)
  }else{
    return <- geom_ret(prices, returns=TRUE)
  }
  # flag if return should be shrinked
  if(shrink=="None"){
    return(return)
  }else if(shrink!="None"){
    stopifnot(length(prices)==length(rf))
    risk_free <- geom_ret(rf, returns=TRUE)
    inv_time <- time_length(difftime(end_date,start_date), unit = time_unit)
    if(shrink=="exponential"){
      shrink_factor =expo_shrink(inv_time, factor=expo_factor)
      return(shrink_factor * return + (1-shrink_factor)*risk_free)
    }
  }
}

###############

# implementation of downward volatility

#############

downward_vola <- function(list_of_returns, 
                          minimum_acceptable_return = list(0, "average", "user_defined"),
                          user_treshold, 
                          raw_ret = TRUE){
  # case differentiation for different thresholds
  # yields returns below a user-defined threshold
  if(minimum_acceptable_return==0){
    cut_off_returns <- elem_min(list_of_returns, threshold=0)
  }else if(minimum_acceptable_return=="average"){
    cut_off_returns <- elem_min(list_of_returns, threshold=mean(list_of_returns))
  } else if(minimum_acceptable_return=="user-defined"){
    cut_off_returns <- elem_min(list_of_returns, threshold=user_treshold)
  }
  # calculate downward standard deviation
  return(sqrt(mean(cut_off_returns**2)))
}

############

# helper function. sets all values in list above user-defined threshold to zero

###########

elem_min <- function(list, threshold=0){
  new_list <- c(0)*length(list)
  for(i in 1:length(list)){
    min <- min(list[i]-threshold, 0)
    new_list[i] <- min
  }
  return(new_list)
}


# define a few functions for transition plot
sortino_ratio <- function(returns, 
                          rf, 
                          closing_prices, 
                          time_adj=FALSE, 
                          start_date, 
                          end_date, 
                          time_unit=list("years", "months", "days"),
                          minimum_acceptable_return = list(0, "average", "user_defined"),
                          user_treshold,
                          shrink = list("None", "exponential"),
                          expo_factor=0.5){
  
  # check if inputs are of the same length
  stopifnot(length(returns)==length(rf))
  
  # calculate downward vola multiplied by sqrt days_vola to adjust for the annualized version
  downward_volatility <- downward_vola(returns-rf, minimum_acceptable_return=minimum_acceptable_return)*sqrt(days_vola)
  # initialize variable for investment time
  inv_time <- time_length(difftime(end_date,start_date), unit = time_unit)
  
  # calculate annualized risk free return
  ann_return_rf <- ann_ret(rf, days_ret = days_ret, start_date = start_date, end_date, returns = TRUE)
  
  # use annualized return
  if(is.null(closing_prices)){
    ann_ret <- ann_ret(returns, days_ret = days_ret, start_date = start_date, end_date, returns=TRUE)
  }else{
    ann_ret <- ann_ret(closing_prices, days_ret = days_ret, start_date = start_date, end_date)
  }
  
  # shrinkage option for expected annualized return. case distinction
  if(shrink=="None"){
    # expected return is equal to the annualized return
    expected_return <- ann_ret
  }else if(shrink=="exponential"){
    # annualized return is shrinked with the exponential function. shrinkage factor is a function of investment time
    shrink_factor =expo_shrink(inv_time, factor=expo_factor)
    # linear combination of risk-free return and annualized return.
    expected_return <- shrink_factor * ann_ret + (1-shrink_factor)*ann_return_rf
  }
  # !!!! implement new shrinkage functions if necessary
  
  # calculate annualized risk free return
  ann_return_rf <- ann_ret(rf, days_ret = days_ret, start_date = start_date, end_date, returns = TRUE)
  
  # cut sortino ratio at o for lack of interpretability for negative ratios
  sort_rat <- max(0,(expected_return-ann_return_rf)/downward_volatility)
  
  # time adjustment option. If true sortino ratio gets multiplied by the sqrt of time units
  if(time_adj==FALSE){
    return(sort_rat)
  }else{
    return(sqrt(inv_time)*sort_rat)
  }
  
}


sharpe_ratio <- function(ret,
                         rf,
                         returns=TRUE,
                         time_adj =FALSE,
                         start_date, 
                         end_date, 
                         time_unit=list("years", "months", "days"),
                         shrink = list("None", "exponential"),
                         expo_factor=0.5,
                         annualized=FALSE){
  
  # check if inputs are of the same length
  stopifnot(length(ret)==length(rf))
  
  # initialize variable for investment time
  inv_time <- time_length(difftime(end_date, start_date), unit = time_unit)
  
  # shrinkage option for expected annualized return. case distinction
  if(shrink=="None"){
    # expected return is equal to the annualized return
    expected_return <- geom_ret(prices = ret, returns=returns)
  }else if(shrink=="exponential"){
    expected_return <- geom_ret_shrink(prices = ret, 
                                       rf = rf, 
                                       returns = returns,
                                       start_date = start_date, # HERE: put dates of whole inv. period of investor
                                       end_date=end_date,
                                       time_unit = time_unit, 
                                       shrink = shrink,
                                       expo_factor = expo_factor)
  }
  # !!!! implement new shrinkage functions if necessary
  
  # now calculate return and variance according to the length of the input
  if(annualized==TRUE){
    sqrt_ret <- sqrt(var(ret)*sqrt(365))
    return <- (1+expected_return)^(365)-1
  }else{
    sqrt_ret <- sqrt(var(ret)*sqrt(length(ret)))
    return <- (1+expected_return)^(length(ret)-1)-1
  }
  
  # calculate risk free return
  risk_free <- geom_ret(prices = rf, returns=TRUE)
  risk_free <- (1+risk_free)^(length(rf)-1)-1
  
  # cut sharpe ratio at 0 for lack of interpretability for negative ratios
  sh_rat = max(0,(return-risk_free)/sqrt_ret)
  
  # time adjustment
  if(time_adj == TRUE){
    return(sqrt(inv_time)*sh_rat)
  }else{
    return(sh_rat)
  }
}


gain_loss_ratio <- function(returns,
                            rf,
                            closing_prices=NULL,
                            threshold=0,
                            start_date,
                            end_date,
                            time_adj= FALSE,
                            time_unit=list("years", "months", "days"),
                            shrink = list("None", "exponential"),
                            expo_factor=0.5){
  
  # check if inputs are of the same length
  stopifnot(length(returns)==length(rf))
  
  # use annualized returns
  if(is.null(closing_prices)){
    ann_ret <- ann_ret(returns, days_ret = days_ret, start_date = start_date, end_date, returns=TRUE)
  }else{
    ann_ret <- ann_ret(closing_prices, days_ret = days_ret, start_date = start_date, end_date)
  }
  
  # calculate annualized risk free return
  ann_return_rf <- ann_ret(rf, days_ret = days_ret, start_date = start_date, end_date, returns = TRUE)
  # initialize variable for investment time
  inv_time <- time_length(difftime(end_date,start_date), unit = time_unit)
  
  # shrinkage option for expected annualized return. case distinction
  if(shrink=="None"){
    # expected return is equal to the annualized return
    expected_return <- ann_ret
  }else if(shrink=="exponential"){
    # annualized return is shrinked with the exponential function. shrinkage factor is a function of investment time
    shrink_factor =expo_shrink(inv_time, factor=expo_factor)
    # linear combination of risk-free return and annualized return.
    expected_return <- shrink_factor * ann_ret + (1-shrink_factor)*ann_return_rf
  }
  # !!!! implement new shrinkage functions if necessary
  
  # get loss mask to get indices of returns below threshold
  loss_mask <- returns < threshold
  # extract the indexed returns. calcualted the mean. flip it with multiplying it with -1.
  loss <- -mean(returns[loss_mask])
  # glr is ratio of expected return and loss (flipped)
  # cut gain/loss ratio at 0 for lack of interpretability for negative ratios
  glr <- max(0,expected_return/loss)
  
  # time adjustment
  if(time_adj == TRUE){
    return(sqrt(inv_time)*glr)
  }else{
    return(glr)
  }
}

expo_shrink <- function(x, factor=0.5){
  -exp(-factor*x)+1
}

na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

drop.na <- function(x){
  return(x[!is.na(x)])
}

rank_custom <- function(x, na.last, quantile){
  # exclude NAN values from ranking
  raw_rank <- rank(-x, na.last=na.last) # multiply x by -1 to award rank 1 to highest value
  # divide the raw rank by the length of non-NAN values to get the relativ ranking.
  # in a second step multiply by the number of quantiles to get a number in the right range
  # ceiling to round all the floats to the next higher integer
  quantile_rank <- ceiling(raw_rank/length(raw_rank[!is.na(raw_rank)])*quantiles)
  return(quantile_rank)
}

sh_rat_rank <- function(sh_rat, raw_ret, na.last="keep", quantiles){
  number_obs <- sum(!is.na(sh_rat))
  # get all the positive sharpe ratios and set the rest to NA
  sh_rat_above_zero <- sh_rat
  sh_rat_above_zero[sh_rat==0] <- NA
  number_non_zero <- sum(!is.na(sh_rat_above_zero))
  # only get the sharpe ratios which are zero
  sh_rat_zero <- sh_rat
  sh_rat_zero[sh_rat!=0] <- NA
  # now only rank the sharpe ratios strictly greater than zero
  sh_rat_above_zero_ranked <- rank(-sh_rat_above_zero, na.last = na.last)
  # now rank the sharpe ratios which are zero according to the raw return
  raw_ret_zero <- raw_ret
  raw_ret_zero[is.na(sh_rat_zero)] <- NA
  sh_rat_zero_ranked <- rank(-raw_ret_zero, na.last = na.last)
  # add max rank of non-zero sharpe ratio
  sh_rat_zero_ranked_adj <- sh_rat_zero_ranked +number_non_zero
  # merge the two rankings again
  rank_combined <- sh_rat_above_zero_ranked
  rank_combined[!is.na(sh_rat_zero_ranked_adj)] <- sh_rat_zero_ranked_adj[!is.na(sh_rat_zero_ranked_adj)]
  # convert raw ranks to quentiles
  quantile_rank <- ceiling(rank_combined/number_obs*quantiles)
  
  return(quantile_rank)
}

scale_3d_array <- function(array, scale_factors=c(100,100,100,100,100,1,1,1)){
  # check dimensions (here specific for the problem at hand)
  dims <- dim(array)
  stopifnot(dims[3]==length(scale_factors))
  for(i in seq(1, dims[1])){
    array[i,,] <- t(scale_factors*t(array[i,,]))
  }
  return(array)
}

corr_custom <- function(data, metrics=1:8, threshold=5){
  dims <- dim(data)
  correlations <- array(NA, dim=c(dims[1], length(metrics)))
  mostattributes(correlations) <- attributes(data)
  dim(correlations) <- c(dims[1], length(metrics))
  attr(correlations, "metrics") <- metrics
  attr(correlations, "threshold") <- threshold
  for(i in 1:dims[1]){
    if(sum(!is.na(data[i,,]))<=threshold*length(metrics)){
      correlations[i,] <- NA
      next()
    }
    for(met in metrics){
      ranks <- drop.na(data[i,,met])
      if(var(ranks[1:length(ranks)-1])==0||var( ranks[-1])==0){
        correlations[i,met] <- NA
      }else{
        correlations[i,met] <- cor(ranks[1:length(ranks)-1], ranks[-1])
      }
    }
  }
  return(correlations)
}

drop <- function(df, colnames){
  # drop a column by name from df
  return(df[, -which(names(df) %in% colnames)])
}

clean_and_save <- function(file){
  # specific data processing for factor data downloaded here: 
  #https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
  file_load <- read.csv(paste0(file,".csv"), sep = ",", header=TRUE)
  print("file loaded")
  date_string <- sapply(file_load$X, toString)
  dates <- as.Date(date_string, "%Y%m%d")
  file_drop <- drop(file_load, c("X"))
  file_norm <- file_drop/100
  file_norm$date <- dates
  saveRDS(file_norm, paste0(file,".RDS"))
}

get_factor_data <- function(factor_data=c("europe", "developed", "north_america")){
  # helper function to get the right factor file
  if(factor_data=="europe"){
    return(Factor_Europe)
  }else if(factor_data=="developed"){
    return(Factor_Developed)
  }else if(factor_data=="north_america"){
    return(Factor_North_America)
  } 
}


# plotting functions
plot_raw <- function(data, trading_ids, metrics, scale_factors=c(100,100,100,100,100,1,1,1), market_ret=NULL){
  dimensions <- dim(data)
  quarter_data <- attributes(data)$quarter_start
  data <- scale_3d_array(data, scale_factors = scale_factors)
  par(mfrow=c(1, length(metrics)))
  for(met in metrics){
    min <- min(sort(data[trading_ids,,met])[1], min(market_ret, na.rm = TRUE))
    max <- max(sort(data[trading_ids,,met], decreasing = TRUE)[1], max(market_ret, na.rm=TRUE))
    plot(NA, xlim=c(1,dimensions[2]), 
         ylim=c(min, max), 
         xlab = "quarters", 
         ylab="raw", 
         main=paste("raw metric: ", attributes(data)$orig_metrics[met]),
         xaxt="n")
    abline(h=0, col="grey")
    for(id in trading_ids){
      lines(data[id,,met], type = "o", col=id, lwd=1.5, lty=id, pch=16)
      if(met==1){
        lines(market_ret, type = "o", col="blue", lwd=1.5, lty=1, pch=25)
      }
    }
    xticks = seq(1, dimensions[2], 5)
    axis(1, at=xticks,labels=FALSE)
    text(x=xticks, par("usr")[3], labels=quarter(quarter_data[xticks], with_year=TRUE), srt=45, pos=2, xpd=TRUE)
    # legend(col=trading_ids)
  }
  
}

plot_ranked <- function(data, trading_ids, metrics, scale_factors=c(100,100,100,100,100,1)){
  dimensions <- dim(data)
  quarter_data <- attributes(data)$quarter_start
  par(mfrow=c(1, length(metrics)))
  for(met in metrics){
    plot(NA, xlim=c(1,dimensions[2]), 
         ylim=c(attributes(data)$quantiles, 1),
         xlab = "quarters", 
         ylab="rank", 
         main=paste("rank. metric: ", attributes(data)$orig_metrics[met]),
         xaxt="n")
    abline(h=attributes(data)$quantiles/2, col="grey")
    for(id in trading_ids){
      lines(data[id,,met], type = "o", col=id, lwd=1.5, lty=id, pch=16)
    }
    xticks = seq(1, dimensions[2], 5)
    axis(1, at=xticks,labels=FALSE)
    text(x=xticks, par("usr")[3]-0.1, labels=quarter(quarter_data[xticks], with_year=TRUE), srt=45, pos=2, xpd=TRUE)
    # legend(col=trading_ids)
  }
}

t_test <- function(data, 
                   metrics=1:8, 
                   alternative="greater",
                   conf_lev=0.95, 
                   mus=c(25,50,75), 
                   threshold=5,
                   all=FALSE){
  # get dimensions of input data
  dims <- dim(data)
  # initialize array with the desired dimensions and save attributes for later
  p_values <- array(NA, dim = c(dims[1], length(metrics)+all, length(mus)))
  mostattributes(p_values) <- attributes(data)
  dim(p_values) <- c(dims[1], length(metrics)+all, length(mus))
  attr(p_values, "metrics") <- metrics
  attr(p_values, "mus") <- mus
  attr(p_values, "conf_lev") <- conf_lev
  attr(p_values, "alternative") <- alternative
  # print(str(p_values))
  # first loop over IDs
  for(ID in 1:dims[1]){#
    # second loop over metrics
    # met_counter for index
    met_counter <- 1
    for(met in metrics){
      # thrid loop over different mus
      # mu_counter for index
      mu_counter <- 1
      for(mu in mus){
        # if values beneath threshold or variance equal to zero -> then fill with NA
        if(sum(!is.na(data[ID,,met]))<=threshold || var(data[ID,,met], na.rm = TRUE)==0){
          p_values[ID,met_counter,mu_counter] <- NA
          # else perform t-test and save p-value
        }else{
          test <- t.test(data[ID,,met], 
                         mu=mu, 
                         conf.level = conf_lev, 
                         alternative=alternative, 
                         na.action=na.omit)
          p_values[ID,met_counter,mu_counter] <- test$p.value
        }
        # separate flag if you want to test over all metrics
        if(all==TRUE){
          if(sum(!is.na(data[ID,,1]))<=threshold){
            p_values[ID,dim(p_values)[3],mu_counter] <- NA
            # else perform t-test and save p-value
          }else{
            test_all <- t.test(data[ID,,], 
                               mu=mu, 
                               conf.level = conf_lev, 
                               alternative=alternative, 
                               na.action=na.omit)
            p_values[ID, dim(p_values)[2], mu_counter] <- test_all$p.value
          }
        }
        mu_counter <- mu_counter+1
      }
      met_counter <- met_counter+1
    }
  }
  return(p_values)
}

var_custom <- function(data, metrics=1:8, threshold=5, all=FALSE){
  dims <- dim(data)
  variances <- array(NA, dim=c(dims[1], length(metrics)+all))
  mostattributes(variances) <- attributes(data)
  dim(variances) <- c(dims[1], length(metrics)+all)
  attr(variances, "metrics") <- metrics
  attr(variances, "threshold") <- threshold
  for(i in 1:dims[1]){
    if(sum(!is.na(data[i,,]))<=threshold*length(metrics)){
      variances[i,] <- NA
      next()
    }
    for(met in metrics){
      variances[i,met] <- var(data[i,,met], na.rm = TRUE)
    }
    if(all==TRUE){
      x <- array(data[i,,], dim=dims[2]*dims[3])
      variances[i, dim(variances)[2]] <- var(x, na.rm=TRUE)
    }
  }
  return(variances)
}

reg_lags <- function(data, metrics=1:8, lags=c(1,2,3,4,5), threshold=5){
  dims <- dim(data)
  reg_lags <- array(dim = c(dims[1], length(metrics), length(lags)))
  mostattributes(reg_lags) <- attributes(data)
  dim(reg_lags) <- c(dims[1], length(metrics), length(lags))
  attr(reg_lags, "metrics") <- metrics
  attr(reg_lags, "lags") <- lags
  attr(reg_lags, "threshold") <- threshold
  for(i in 1:dims[1]){
    # met_counter for index
    met_counter <- 1
    for(met in metrics){
      clean <- drop.na(data[i,,met])
      # lag_counter for index
      lag_counter <- 1
      for(lag in lags){
        if(length(clean)-lag >= threshold){
          x <- clean[1:(length(clean)-lag)]
          y <- clean[-(1:lag)]
          reg <- lm(x~y)
          reg_lags[i,met_counter,lag_counter] <- reg$coefficients[2]
        }else{
          reg_lags[i,met_counter,lag_counter] <- NA
        }
        lag_counter <-  lag_counter+1
      }
      met_counter <- met_counter+1
    }
  }
  return(reg_lags)
}


sig_ratio <- function(data, conf_level=0.05){
  bool <- data<=conf_level
  sum_p_val <- apply(bool, MARGIN = c(2,3), FUN = sum, na.rm=TRUE)
  sum_na <- apply(!is.na(data), MARGIN = c(2,3), FUN = sum, na.rm=TRUE)
  ratios <- sum_p_val/sum_na
  dims_ratios <- dim(ratios)
  mostattributes(ratios) <- attributes(data)
  dim(ratios) <- dims_ratios
  return(ratios)
}

