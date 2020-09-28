# Transition plot for time adjusted and standard sortino ratio

sortino_ratio <- function(){
  
}

downward_vola <- function(list_of_returns, treshold = list("zero", "average", "user_defined"), user_treshold, raw_ret = TRUE){
  cut_off_returns <- elem_min(list_of_returns)
  return(sqrt(mean(cut_off_returns**2)))
}

elem_min <- function(list, threshold=0){
  new_list <- c(0)*length(list)
  for(i in 1:length(list)){
    min <- min(list[i], threshold)
    new_list[i] <- min
  }
  return(new_list)
}


elem_min(x, threshold = 0)

downward_vola(x)
