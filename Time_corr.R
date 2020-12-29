# source libraries
source("requirements.R")

# load functions file
source("functions_report.R")

# load data, extract IDs, select relevant columns, fill NA (takes +/- 2 min)
source("load_process_data.R")

##############

# loop through IDs and extract different performance measures. Calculate temporal correlation
# (raw return, shrinked raw return, volatility adj. raw/shrinked raw return, CAPM-alpha, CAPM beta)

##############

#define global variables which are used for return and ratio calculations
days_ret <-  365 # calender days
days_vola <- 252 # average number of trading days in a year
min_days <- 130 # minimum investment period in days
quarter_start <- seq(from=as.Date("2012-01-01"), to=as.Date("2021-01-01"), by="quarter") # all quarter start dates
quarter_end <- quarter_start[-1]-1 # all quarter end dates
orig_metrics <- c("daily_raw_ret",
             "daily_raw_ret_shrink",
             "daily_var_adj_ret",
             "daily_var_adj_ret_shrink",
             "CAPM_a",
             "CAPM_b",
             "sh_rat",
             "sh_rat_shrink")
expo_factor <- 0.5 # shrinkage factor
factor_data_name <- "developed" # which factor returns are used
verbose <- FALSE
quantiles <- 100 # define ranking granularity
rem_selection_bias <- FALSE
special_sh_rat_rank <- TRUE


# initiate tensor to collect measurements
measurements <- array(NA, dim=c(IDs, length(quarter_start), length(orig_metrics)))

# get the rigth factor data
factor_data <- get_factor_data(factor_data_name)

# ***************************************************************************** #

##########

# calculate quarterly market return

##########
calc_market_ret <- TRUE

if(calc_market_ret==TRUE){
  # initiate empty arrays to collect data for market returns
  quarterly_factor_return <- array(NA, dim=c(length(quarter_start)))
  quarterly_factor_return_shrinked <- array(NA, dim=c(length(quarter_start)))
  
  # extract dates
  dates_market <- factor_data$date
  # get first and last date of time series
  start_date <- min(dates_market)
  end_date <- max(dates_market)
  
  # get start date of first full first quarter
  diff_start <- as.vector(quarter_start-start_date)
  first_quarter <- quarter_start[match(min(diff_start[diff_start>0]), diff_start)]
  # get end date of last full quarter
  diff_end <- as.vector(quarter_end-end_date)
  last_quarter <- quarter_end[match(max(diff_end[diff_end<0]), diff_end)]
  # get sequence of active quarters
  active_quarters <- seq(match(first_quarter, quarter_start),match(last_quarter, quarter_end))
  
  for(quarter in active_quarters){
    # get start/end date of current quarter
    start <- quarter_start[quarter]
    end <- quarter_end[quarter]
    # get all the dates which lie between start and end
    diff1 <- as.vector(dates_market-start)
    start_quarter <- dates_market[match(min(diff1[diff1>=0]), diff1)]
    diff2 <- as.vector(dates_market-end)
    end_quarter <- dates_market[match(max(diff2[diff2<=0]), diff2)]
    start_idx <- match(start_quarter, dates_market)
    end_idx <- match(end_quarter, dates_market)
    # relevant returns
    rel_ret <- factor_data[start_idx:end_idx,]
    rel_ret$market_return <- rel_ret$Mkt.RF+rel_ret$RF
    # calculate return
    raw_market_return <- geom_ret(rel_ret$market_return, returns=TRUE)
    raw_market_return_shrinked <- geom_ret_shrink(prices=rel_ret$market_return,
                                                  returns = TRUE,
                                                  rf=rel_ret$RF,
                                                  start_date=start_date, 
                                                  end_date=end_date,
                                                  time_unit="years",
                                                  shrink = "exponential", 
                                                  expo_factor = 0.5)
    # save 
    quarterly_factor_return[quarter] <- raw_market_return
    quarterly_factor_return_shrinked[quarter] <- raw_market_return_shrinked
  }
  saveRDS(quarterly_factor_return, paste0("quarterly_factor_return_",factor_data_name,".RDS"))
  saveRDS(quarterly_factor_return_shrinked, paste0("quarterly_factor_return_shrinked",factor_data_name,".RDS"))
}

# **************************************************************************** #

########

# calculate sharpe ratio of market during analysed period.
# Later use as LAMBDA (utility return function)

########

calc_market_sharpe <- TRUE

if(calc_market_sharpe==TRUE){
  idx <- factor_data$date > first_quarter & factor_data$date < last_quarter
  sharpe_data <- factor_data[idx,]
  
  sharpe_market <- sharpe_ratio(ret = (sharpe_data$Mkt.RF+sharpe_data$RF),
                                rf = sharpe_data$RF,
                                start_date=min(sharpe_data$date),
                                end_date=max(sharpe_data$date),
                                time_unit="years",
                                shrink="None",
                                annualized=TRUE)
}


lambda <- sharpe_market # variance adjustment factor

# ******************************************************************************** #

###########

# main loop through trading IDs

###########
calc_main <- TRUE

if(calc_main==TRUE){
  # initiate buckets to count excluded IDs
  short_inv <- c() # 928
  zero_inv <- c() # 1939
  
  pb <- txtProgressBar(min = 0, max = IDs, style = 3, width=100) # progress bar
  for(i in 1:IDs){
    # get dataframe of individual WikiId
    WikiDaily_Id <- WikiDaily_selected[WikiDaily_selected$wiki_nr==i,]
    
    # merge the different dataframes on date
    WikiDaily_merge <- merge_mult(list_of_dfs=list(WikiDaily_Id, factor_data), by=c("date"))
    
    ##########
    
    # check several conditions
    
    #########
    
    # check if trader has any asset under management (AUM)
    if(mean(WikiDaily_merge$aum)==0){
      if(verbose==TRUE){
        print(sprintf("Trader with ID %i has average AUM of zero.", i))
      }
      zero_inv <- append(zero_inv, i)
      next()
    }
    
    # remove selection bias
    if(rem_selection_bias==TRUE){
      WikiDaily_merge <- WikiDaily_merge[WikiDaily_merge$aum!=0,]
    }
    
    # check if trader fulfills min inv. period
    if(nrow(WikiDaily_merge)<=min_days){
      if(verbose==TRUE){
        print(sprintf("Trader with ID %i does not satisfy minimum investment period.", i)) 
      }
      short_inv <- append(short_inv, i)
      next()
    }
    
    ########
    
    # if conditions are satisfied, continue with calculations
    
    #######
    
    # extract date
    dates <- WikiDaily_merge$date
    # determine first and last date in time series
    start_date <- min(dates)
    end_date <- max(dates)
    
    # get start date of first full first quarter
    diff_start <- as.vector(quarter_start-start_date)
    first_quarter <- quarter_start[match(min(diff_start[diff_start>0]), diff_start)]
    # get end date of last full quarter
    diff_end <- as.vector(quarter_end-end_date)
    last_quarter <- quarter_end[match(max(diff_end[diff_end<0]), diff_end)]
    # get sequence of active quarters
    active_quarters <- seq(match(first_quarter, quarter_start),match(last_quarter, quarter_end))
    
    # for given ID loop through quarters and calculate 
    for(quarter in active_quarters){
      # get start and end date of current quarter
      start_q <- quarter_start[quarter]
      end_q <- quarter_end[quarter]
      # get all the dates which lie between start and end
      diff1 <- as.vector(dates-start_q)
      start_quarter <- dates[match(min(diff1[diff1>=0]), diff1)]
      diff2 <- as.vector(dates-end_q)
      end_quarter <- dates[match(max(diff2[diff2<=0]), diff2)]
      start_idx <- match(start_quarter, dates)
      end_idx <- match(end_quarter, dates)
      # relevant returns
      rel_ret <- WikiDaily_merge[start_idx:end_idx,]
      
      # start calculating measures
      
      # raw return and raw return shrinked. IMPORTANT: avg. daily returns of specific quarter are calculated here.
      daily_raw_ret <- geom_ret(rel_ret$close, returns=FALSE)
      daily_raw_ret_shrink <- geom_ret_shrink(prices=rel_ret$close,
                                              returns=FALSE,
                                              rf = rel_ret$RF,
                                              start_date=start_date,
                                              end_date=end_date,
                                              time_unit = "years",
                                              shrink="exponential",
                                              expo_factor = expo_factor)
      # variance adjusted
      daily_var_adj_raw_ret <- daily_raw_ret - lambda*var(rel_ret$ret)
      daily_var_adj_raw_ret_shrink <- daily_raw_ret_shrink - lambda*var(rel_ret$ret)
      
      # CAPM measures
      capm <- lm(I(ret-RF)~Mkt.RF, data=rel_ret)
      capm_a <- capm$coefficients[1]
      capm_b <- capm$coefficients[2]
      
      # sharpe ratios
      sh_rat <- sharpe_ratio(ret=rel_ret$ret,
                             rf=rel_ret$RF,
                             time_adj =FALSE,
                             start_date=start_date, 
                             end_date=end_date, 
                             time_unit="years",
                             shrink = "None")
      
      sh_rat_shrink <- sharpe_ratio(ret=rel_ret$ret,
                                    rf=rel_ret$RF,
                                    time_adj =FALSE,
                                    start_date=start_date, 
                                    end_date=end_date, 
                                    time_unit="years",
                                    shrink = "exponential",
                                    expo_factor = expo_factor)
      
      # fill tensor
      measurements[i, quarter, 1] <- daily_raw_ret
      measurements[i, quarter, 2] <- daily_raw_ret_shrink
      measurements[i, quarter, 3] <- daily_var_adj_raw_ret
      measurements[i, quarter, 4] <- daily_var_adj_raw_ret_shrink
      measurements[i, quarter, 5] <- capm_a
      measurements[i, quarter, 6] <- capm_b
      measurements[i, quarter, 7] <- sh_rat
      measurements[i, quarter, 8] <- sh_rat_shrink
      
      # update progress bar
      setTxtProgressBar(pb, i)
    }
  }
  
  # set couple of attributes for possible later usage
  attr(measurements, "quarter_start") <- quarter_start
  attr(measurements, "quarter_end") <- quarter_end
  attr(measurements, "orig_metrics") <- orig_metrics
  attr(measurements, "lambda") <- lambda
  attr(measurements, "expo_factor") <- expo_factor
  attr(measurements, "quantiles") <- quantiles
  attr(measurements, "factor") <- factor_data_name
  attr(measurements, "remove_selection_bias") <- rem_selection_bias
  attr(measurements, "special_sh_rat_rank") <- special_sh_rat_rank
  
  # raw data extracted. Next: ranked data (custom quantiles)
  measurements_ranked <- apply(measurements, c(2,3), rank_custom, na.last="keep", quantile=quantiles)
  # rank sharpe ratio specially
  if(special_sh_rat_rank == TRUE){
    for(quarter in 1:dim(measurements_ranked)[2]){
      measurements_ranked[,quarter,7] <- sh_rat_rank(sh_rat = measurements[,quarter,7], 
                                                    raw_ret= measurements[,quarter,1], 
                                                    quantiles=quantiles)
      measurements_ranked[,quarter,8] <- sh_rat_rank(sh_rat = measurements[,quarter,8], 
                                                    raw_ret= measurements[,quarter,2], 
                                                    quantiles=quantiles)
    }
  }
  # assign same attributes to ranked data
  attributes(measurements_ranked) <- attributes(measurements)
  
  saveRDS(measurements, file=paste0("quarterly_measurements_factor_", factor_data_name,"_lambda_",round(lambda,1), "_expo_factor_", expo_factor,".RDS"))
  saveRDS(measurements_ranked, file=paste0("quarterly_measurements_ranked_factor_", factor_data_name, "_lambda_",round(lambda,1), "_expo_factor_", expo_factor,".RDS"))
  
  print("data extracted and saved.")
}

# ********************************************************************************************* #
# Further Analysis

######

# plot evolution of measure/rank and calculate correlation

#####

plotting <- TRUE
if(plotting==TRUE){
  # decide which file to load in order to plot
  qm_ranked_plot <- readRDS("quarterly_measurements_ranked_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  qm_raw_plot <- readRDS("quarterly_measurements_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  quarterly_factor_return <- readRDS("quarterly_factor_return_developed.RDS")
  
  trading_ids <- sample(1:7500, 5)
  metrics <- c(1,2,7)
  
  plot_raw(qm_raw_plot, trading_ids =trading_ids, metrics=metrics, market_ret =quarterly_factor_return*100)
  
  plot_ranked(qm_ranked_plot, trading_ids =trading_ids, metrics =metrics)
}

# ********************************************************************************************* #

##########

# max-beta calculations

#########
  
# decide which file to load in order to calculate correlations
# use the one with the higher beta on average over all trader IDS
calc_beta <- FALSE

if(calc_beta==TRUE){
  factors <- c("europe", "developed", "north_america")
  
  for(fac in factors){
    # print(fac)
    file <- readRDS(paste0("quarterly_measurements_factor_",fac,"_lambda_1_expo_factor_0.5.RDS"))
    avg <- mean(file[,,6], na.rm=TRUE)
    print(paste0(fac, ": ", avg))
  }
  
  # output:
  # [1] "europe: 0.43914899434882"
  # [1] "developed: 0.726025530945095"
  # [1] "north_america: 0.623552168507026"
  
  # developed factor returns show highest beta
}

# ********************************************************************************************* #

#########

# correlations: timedependence of raw and ranked data

########

# load the file using the developed factor returns and calculate necessary correlations
calc_corr <- TRUE

if(calc_corr==TRUE){
  qm_ranked_corr <- readRDS("quarterly_measurements_ranked_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  # qm_raw_corr <- readRDS("quarterly_measurements_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  
  # correlation of ranks returns with and without shrinkage
  correlations <- corr_custom(qm_ranked_corr, threshold = 5)
  
  mean_corr <- apply(correlations, c(2), mean, na.rm=TRUE)

}

# ********************************************************************************************* #

#########

# statistical test: check whether mean return/rank is statisically different from 25/50/75

########
calc_stat_test <- TRUE

if(calc_stat_test==TRUE){
  qm_ranked_stat_test <- readRDS("quarterly_measurements_ranked_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  #qm_raw_stat_test <- readRDS("quarterly_measurements_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  
  p_val <- t_test(data=qm_ranked_stat_test, all = FALSE, metrics = 1:8, mus = c(25, 50, 75))
  
  sig_ratios <- sig_ratio(p_val, conf_level = 0.05)
}

# ********************************************************************************************* #

#########

# variance: calculate the variance of the ranks

########
calc_var_rank <- TRUE

if(calc_var_rank==TRUE){
  qm_ranked_var <- readRDS("quarterly_measurements_ranked_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  #qm_raw_var <- readRDS("quarterly_measurements_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  
  variances <- var_custom(qm_ranked_var, all=FALSE)
  
  mean_var <- apply(variances, c(2), mean, na.rm=TRUE)
}


# ********************************************************************************************* #

#########

# regress ranks on lagged ranks

########

calc_reg_lags <- TRUE

if(calc_reg_lags==TRUE){
  qm_ranked_reg_lags <- readRDS("quarterly_measurements_ranked_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  #qm_raw_reg_lags <- readRDS("quarterly_measurements_factor_developed_lambda_2.2_expo_factor_0.5.RDS")
  
  reg_lags <- reg_lags(qm_ranked_reg_lags, metrics=1:8, lags=1:5)
}

# ********************************************************************************************* #



###################### RESULTS ##################################

# ******************************************************************** #

# represent in dataframe
results <- data.frame(sig_ratios, row.names = orig_metrics[attr(sig_ratios, "metrics")])
colnames(results) <- attr(sig_ratios, "mus")
results$variances <- mean_var
results$correlations <- mean_corr
results


# > results
# 25         50          75 variances correlations
# daily_raw_ret            0.8885993 0.08751357 0.001737242  809.9551  -0.09257744
# daily_raw_ret_shrink     0.9011944 0.08664495 0.001520087  796.2321  -0.09232168
# daily_var_adj_ret        0.8979149 0.11728931 0.011946134  759.2324  -0.08506795
# daily_var_adj_ret_shrink 0.9120330 0.12641182 0.016290182  730.7455  -0.08016973
# CAPM_a                   0.8775244 0.10206298 0.001302932  805.8759  -0.04301497
# CAPM_b                   0.7364762 0.31848794 0.084075603  417.2847   0.23361460
# sh_rat                   0.8920738 0.11834962 0.002823018  778.4436  -0.07512514
# sh_rat_shrink            0.9122693 0.11878393 0.002605863  761.1058  -0.07519179
























# old code

# mean_corr_ranked <- mean(corr_ranked, na.rm = TRUE) # 4605 non-NA values
# mean_corr_ranked_shrinked <- mean(corr_ranked_shrinked, na.rm = TRUE) # 4605 non-NA values

# > mean_corr_ranked
# [1] -0.09257744
# > mean_corr_ranked_shrinked
# [1] -0.09232168

# correlations of raw alphas and ranks of alphas
# corr_alpha_raw <- corr_custom(qm_raw_corr, metrics_idx = 5, threshold = 5)
# corr_alpha_rank <- corr_custom(qm_ranked_corr, metrics_idx = 5, threshold = 5)
# 
# mean_corr_alpha_raw <- mean(corr_alpha_raw, na.rm = TRUE) # 4605 non-NA values
# mean_corr_alpha_rank <- mean(corr_alpha_rank, na.rm = TRUE) # 4605 non-NA values

# > mean_corr_alpha_raw
# [1] -0.06173374
# > mean_corr_alpha_rank
# [1] -0.04298504

