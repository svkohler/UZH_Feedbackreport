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
metrics <- c("raw", "raw_shrink", "var_adj", "var_adj_shrink", "CAPM_a", "CAPM_b")
lambda <- 1 # variance adjustment factor
expo_factor <- 0.5 # shrinkage factor
factor_data_name <- "europe" # which factor returns are used
verbose <- FALSE
quantiles <- 100 # define ranking granularity


# initiate tensor to collect measurements
measurements <- array(NA, dim=c(IDs, length(quarter_start), length(metrics)))

# get the rigth factor data
factor_data <- get_factor_data(factor_data_name)

# initiate buckets to count excluded IDs
short_inv <- c() # 905
zero_inv <- c() # 1715

pb <- txtProgressBar(min = 0, max = IDs, style = 3, width=100)
for(i in 1:IDs){
  # get dataframe of individual WikiId
  WikiDaily_Id <- WikiDaily_selected[WikiDaily_selected$wiki_nr==i,]

  # merge the different dataframes on date
  WikiDaily_merge <- merge_mult(list_of_dfs=list(WikiDaily_Id, factor_data), by=c("date"))
  
  # check if trader fulfills min inv. period
  if(nrow(WikiDaily_merge)<=min_days){
    if(verbose==TRUE){
      print(sprintf("Trader with ID %i does not satisfy minimum investment period.", i)) 
    }
    short_inv <- append(short_inv, i)
    next()
  }
  
  # check if trader has any asset under management (AUM)
  if(mean(WikiDaily_merge$aum)==0){
    if(verbose==TRUE){
      print(sprintf("Trader with ID %i has average AUM of zero.", i))
    }
    zero_inv <- append(zero_inv, i)
    next()
  }
  
  # if investment duration is long enough, continue with calculations
  
  # extract date
  dates <- WikiDaily_merge$date
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
    start <- quarter_start[quarter]
    end <- quarter_end[quarter]
    # get all the dates which lie between start and end
    diff1 <- as.vector(dates-start)
    start_quarter <- dates[match(min(diff1[diff1>=0]), diff1)]
    diff2 <- as.vector(dates-end)
    end_quarter <- dates[match(max(diff2[diff2<=0]), diff2)]
    start_idx <- match(start_quarter, dates)
    end_idx <- match(end_quarter, dates)
    # relevant returns
    rel_ret <- WikiDaily_merge[start_idx:end_idx,]
    
    # start calculating measures
    
    # raw return and raw return shrinked
    raw_return <- geom_ret(rel_ret$close, returns=FALSE)
    raw_return_shrinked <- geom_ret_shrink(prices=rel_ret$close,
                                           rf = rel_ret$RF,
                                           start_date=start_date,
                                           end_date=end_date,
                                           time_unit = "years",
                                           shrink="exponential",
                                           expo_factor = expo_factor)
    # variance adjusted
    var_adj_raw <- raw_return - lambda*var(rel_ret$ret)
    var_adj_raw_shrinked <- raw_return_shrinked - lambda*var(rel_ret$ret)
    
    # CAPM measures
    capm <- lm(I(ret-RF)~Mkt.RF, data=rel_ret)
    capm_a <- capm$coefficients[1]
    capm_b <- capm$coefficients[2]
    
    # fill tensor
    measurements[i, quarter, 1] <- raw_return
    measurements[i, quarter, 2] <- raw_return_shrinked
    measurements[i, quarter, 3] <- var_adj_raw
    measurements[i, quarter, 4] <- var_adj_raw_shrinked
    measurements[i, quarter, 5] <- capm_a
    measurements[i, quarter, 6] <- capm_b
    
    # update progress bar
    setTxtProgressBar(pb, i)
  }
}

# raw data extracted. Next: ranked data (custom quantiles)
measurements_ranked <- apply(measurements, c(2,3), rank_custom, na.last="keep", quantile=quantiles)

saveRDS(measurements, file=paste0("quarterly_measurements_factor_", factor_data_name,"_lambda_",lambda, "_expo_factor_", expo_factor,".RDS"))
saveRDS(measurements_ranked, file=paste0("quarterly_measurements_ranked_factor_", factor_data_name, "_lambda_",lambda, "_expo_factor_", expo_factor,".RDS"))

# decide which file to load
quarterly_measurements_selected <- readRDS("quarterly_measurements_ranked_factor_europe_lambda_0.5_expo_factor_0.5.RDS")

######

# plot evolution of measure/rank and calculate correlation

#####

trading_ids <- 1000
show_metrics <- c(4,5)

data <- quarterly_measurements_selected[trading_ids,,show_metrics]
par(mfrow=c(1, length(show_metrics)))
for(met in show_metrics){
  plot(NA, xlim=c(1,length(quarter_start)), 
       ylim=c(quantiles, 1), 
       xlab = "quarters", 
       ylab="rank", 
       main=paste("rank over time. metric: ", metrics[met]),
       xaxt="n")
  abline(h=quantiles/2, col="grey")
  for(id in trading_ids){
    lines(quarterly_measurements_selected[id,,met], type = "o", col=id, lwd=1.5, lty=id, pch=16)
  }
  xticks = seq(1, length(quarter_start), 5)
  axis(1, at=xticks,labels=FALSE)
  text(x=xticks, par("usr")[3]+4, labels=quarter(quarter_start[xticks], with_year=TRUE), srt=45, pos=2, xpd=TRUE)
  # legend(col=trading_ids)
}

  