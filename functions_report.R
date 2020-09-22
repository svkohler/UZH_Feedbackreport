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
