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


abs_ret <- function(ret){
  return <- (tail(ret, 1) - ret[1])/ret[1]
  return(return)
}

#abs_ret(WikiDaily_merge$EuroStoxx)

ann_ret <- function(ret, days_ret, start_date, end_date){
  abs_ret <- abs_ret(ret)
  ann_ret <- ((1+abs_ret)^(days_ret/as.numeric((end_date-start_date))))-1
  return(ann_ret)
}

#ann_ret(ret= WikiDaily_merge$EuroStoxx, days_ret=days_ret, start_date = start_date, end_date=end_date)




# define a few functions for transition plot
sortino_ratio <- function(list_of_returns, 
                          list_of_rf, 
                          closing_prices, 
                          time_adj=FALSE, 
                          start_date, 
                          end_date, 
                          time_unit=list("years", "months", "days"),
                          minimum_acceptable_return = list(0, "average", "user_defined"),
                          user_treshold){
  
  stopifnot(length(list_of_returns)==length(list_of_rf))
  
  downward_volatility <- downward_vola(list_of_returns-list_of_rf, minimum_acceptable_return=minimum_acceptable_return)*sqrt(days_vola)
  ann_return_portfolio <- ann_ret(closing_prices, days_ret = days_ret, start_date = start_date, end_date)
  
  return_rf <- prod(1+list_of_rf[2:length(list_of_rf)])-1
  ann_return_rf <- ((1+return_rf)^(days_ret/as.numeric((end_date-start_date))))-1
  
  sort_rat <- (ann_return_portfolio-ann_return_rf)/downward_volatility
  
  if(time_adj==FALSE){
    return(sort_rat)
  }else{
    inv_time <- time_length(difftime(end_date,start_date), unit = time_unit)
    return(sqrt(inv_time)*sort_rat)
  }
  
}

downward_vola <- function(list_of_returns, minimum_acceptable_return = list(0, "average", "user_defined"), user_treshold, raw_ret = TRUE){
  if(minimum_acceptable_return==0){
    cut_off_returns <- elem_min(list_of_returns, threshold=0)
  }else if(minimum_acceptable_return=="average"){
    cut_off_returns <- elem_min(list_of_returns, threshold=mean(list_of_returns))
  } else if(minimum_acceptable_return=="user-defined"){
    cut_off_returns <- elem_min(list_of_returns, threshold=user_treshold)
  }
  return(sqrt(mean(cut_off_returns**2)))
}

elem_min <- function(list, threshold=0){
  new_list <- c(0)*length(list)
  for(i in 1:length(list)){
    min <- min(list[i]-threshold, 0)
    new_list[i] <- min
  }
  return(new_list)
}





