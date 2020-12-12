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

ann_ret <- function(prices, days_ret, start_date, end_date, returns=FALSE){
  # flag for returns/prices is propagated through the geom_ret function
  geom_ret <- geom_ret(prices, returns = returns)
  # use dates to calculate fractions of years. dependent on the global variable "days_ret" (i.e. the specified number of days per year)
  ann_ret <- ((1+geom_ret)^(days_ret/as.numeric((end_date-start_date))))-1
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
    return(geom_ret)
  }else{
    # if returns are provided the geometric return is calculated as the product of the individual returns
    len <- length(prices)-1
    geom_ret <- (prod(1+prices))^(1/len)-1
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
    return <- geom_ret(prices, returns=FALSE)
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

downward_vola <- function(list_of_returns, minimum_acceptable_return = list(0, "average", "user_defined"), user_treshold, raw_ret = TRUE){
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


sharpe_ratio <- function(returns,
                         rf,
                         closing_prices,
                         time_adj =FALSE,
                         start_date, 
                         end_date, 
                         time_unit=list("years", "months", "days"),
                         shrink = list("None", "exponential"),
                         expo_factor=0.5){
  
  # check if inputs are of the same length
  stopifnot(length(returns)==length(rf))
  
  # calculate simple annualized variance
  ann_variance_ret <- var(returns)*sqrt(days_vola)
  
  # use annualized returns
  if(is.null(closing_prices)){
    ann_ret <- ann_ret(returns, days_ret = days_ret, start_date = start_date, end_date, returns=TRUE)
  }else{
    ann_ret <- ann_ret(closing_prices, days_ret = days_ret, start_date = start_date, end_date)
  }
  
  # initialize variable for investment time
  inv_time <- time_length(difftime(end_date,start_date), unit = time_unit)
  # calculate annualized risk free return
  ann_return_rf <- ann_ret(rf, days_ret = days_ret, start_date = start_date, end_date, returns = TRUE)
  
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
  
  # cut sharpe ratio at 0 for lack of interpretability for negative ratios
  sh_rat = max(0,(expected_return-ann_return_rf)/sqrt(ann_variance_ret))
  
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

rank_custom <- function(x, na.last, quantile){
  # exclude NAN values from ranking
  raw_rank <- rank(-x, na.last=na.last) # multiply x by -1 to award rank 1 to highest value
  # divide the raw rank by the length of non-NAN values to get the relativ ranking.
  # in a second step multiply by the number of quantiles to get a number in the right range
  # ceiling to round all the floats to the next higher integer
  quantile_rank <- ceiling(raw_rank/length(raw_rank[!is.na(raw_rank)])*quantiles)
  return(quantile_rank)
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



