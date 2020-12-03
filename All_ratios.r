####################

# File combining all the different ratios

####################

# Transition plot for time adjusted and standard sortino ratio

# !!! load all the packages from the Loop_v1.R file

# working directory automatically in right folder of r project
# if someone else than svkohl uses the script, make sure the working directory is set correctly

# source libraries
source("requirements.R")

# load functions file
source("functions_report.R")

# load the data
EuroStoxx <- read.dta13("EuroStoxx50.dta")
FactorReturns <- read.dta13("FactorReturns.dta")
# setting the name of the first column of the factor returns for later merging
colnames(FactorReturns)[1] <- "date"
# load the latest Wikifolio data
WikiDaily_01122020 <- read.dta13("WikiDaily_forFeedback.dta")

# manipulating data, make copy first
WikiDaily_mod <- WikiDaily_01122020
# extract number of different WikiIds and add column of facilitated Ids
IDs <- length(unique(WikiDaily_01122020$wikiId))
WikiDaily_mod$wiki_nr <- as.factor(WikiDaily_mod$wikiId)
levels(WikiDaily_mod$wiki_nr) <- c(1:IDs)
# decide which columns to keep
WikiDaily_selected <- WikiDaily_mod[,c("date", "wiki_nr", "ret", "close")]
# first return per IDs is NaN since no reference close price available. Fill with zero.
WikiDaily_selected <- na.zero(WikiDaily_selected)

###########

# get the average daily return

##########

# set up matrix with the right shape. #dates*#IDs. First get the earliest date from the Wikifolio data.
earliest <- as.Date(min(WikiDaily_selected$date))
latest <- as.Date(max(WikiDaily_selected$date))
dates <- seq(earliest, latest, "days")
all_ret <- data.frame(matrix(NA, ncol=1, nrow=length(dates)))
# set the first column as the dates to merge over
all_ret[,1] = dates
colnames(all_ret) <- c("date")

pb <- txtProgressBar(min = 0, max = IDs, style = 3, width=100)
for(i in 1:IDs){
  # get snipped of WikiDaily for single ID
  WikiDaily_Id <- WikiDaily_selected[WikiDaily_selected$wiki_nr==i,][c("date", "ret")]
  # assign distinct column name per ID
  names(WikiDaily_Id)[names(WikiDaily_Id) == "ret"] <- paste("ret_", i)
  # merge dates df with ret df of ID. all=TRUE to always keep whole date range.
  all_ret <- merge(all_ret, WikiDaily_Id, by="date", all=TRUE)
  setTxtProgressBar(pb, i)
}
# fill all the NaN with zeros
all_ret_filled <- na.zero(all_ret)
# produce new column with row (e.g. date) average with all investors which held assets then.
all_ret_filled$AVG_ret <- apply(all_ret_filled[,-1], 1, mean)
# save all returns
saveRDS(all_ret_filled, file="All_returns.RDS")
# save avg. daily returns
saveRDS(all_ret_filled[, c("date", "AVG_ret")], file="avg_daily_returns.RDS")

# IMPORTANT: load the result from previous run to avoid long calculation times
avg_daily_ret <- readRDS("avg_daily_returns.RDS")


#############

# get set of ratios for all IDs

############

#define global variables which are used for return and ratio calculations
days_ret <-  365 #calender days
days_vola <- 252 #average number of trading days in a year

# think about the ratios you want to extract. Initiate in the next few lines
nr_ratios <- 12

# initialize matrix to collect different ratios. expand columns if necessary
all_rat <- data.frame(matrix(data=NA, nrow = IDs, ncol = nr_ratios+1))
# assign column names for better usability
colnames(all_rat) <- c("ID", 
                       "Sharpe_normal", 
                       "Sharpe_time", 
                       "Sharpe_normal_shrink", 
                       "Sharpe_time_shrink", 
                       "Sortino_normal", 
                       "Sortino_time", 
                       "Sortino_normal_shrink",
                       "Sortino_time_shrink",
                       "G/L_normal", 
                       "G/L_time",
                       "G/L_normal_shrink",
                       "G/L_time_shrink")
#initiate first column as IDs
all_rat[,1] <- 1:IDs

pb <- txtProgressBar(min = 0, max = IDs, style = 3, width=100)
for(i in 1:IDs){
  # get dataframe of individual WikiId
  WikiDaily_Id <- WikiDaily_selected[WikiDaily_selected$wiki_nr==i,]
  # extract date
  start_date <- head(WikiDaily_Id$date, 1)
  end_date <- tail(WikiDaily_Id$date, 1)
  # check dates for hard starts/ends
  
  # merge the different dataframes on date
  WikiDaily_merge <- merge_mult(list_of_dfs=list(WikiDaily_Id, FactorReturns, EuroStoxx, avg_daily_ret), by=c("date"))
  
  # check if not empty
  if(nrow(WikiDaily_merge)==0){
    #print(sprintf("\n Dataframe for ID %i is empty. Time intervals do not intersect", i))
    next()
  }
  
  # calculate pre-defined ratios. blockwise.
  all_rat[i,2] <- sharpe_ratio(returns = WikiDaily_merge$ret, 
                               rf = WikiDaily_merge$rf, 
                               closing_prices = WikiDaily_merge$close, 
                               time_adj = FALSE,
                               time_unit = "years",
                               start_date = start_date,
                               end_date = end_date,
                               shrink = "None",
                               expo_factor=0.5)
  
  all_rat[i,3] <-  sharpe_ratio(returns = WikiDaily_merge$ret, 
                                rf = WikiDaily_merge$rf, 
                                closing_prices = WikiDaily_merge$close, 
                                time_adj = TRUE,
                                time_unit = "years",
                                start_date = start_date,
                                end_date = end_date,
                                shrink="None",
                                expo_factor=0.5)
  
  all_rat[i,4] <-  sharpe_ratio(returns = WikiDaily_merge$ret, 
                                rf = WikiDaily_merge$rf, 
                                closing_prices = WikiDaily_merge$close, 
                                time_adj = FALSE,
                                time_unit = "years",
                                start_date = start_date,
                                end_date = end_date,
                                shrink="exponential",
                                expo_factor=0.5)
  
  all_rat[i,5] <-  sharpe_ratio(returns = WikiDaily_merge$ret, 
                                rf = WikiDaily_merge$rf, 
                                closing_prices = WikiDaily_merge$close, 
                                time_adj = TRUE,
                                time_unit = "years",
                                start_date = start_date,
                                end_date = end_date,
                                shrink="exponential",
                                expo_factor=0.5)
  
  all_rat[i,6] <-  sortino_ratio(returns = WikiDaily_merge$ret, 
                                 rf = WikiDaily_merge$rf, 
                                 closing_prices = WikiDaily_merge$close, 
                                 time_adj = FALSE,
                                 time_unit = "years",
                                 start_date = start_date,
                                 end_date = end_date,
                                 minimum_acceptable_return = 0,
                                 shrink="None",
                                 expo_factor=0.5)
  
  all_rat[i,7] <-  sortino_ratio(returns = WikiDaily_merge$ret, 
                                 rf = WikiDaily_merge$rf, 
                                 closing_prices = WikiDaily_merge$close, 
                                 time_adj = TRUE,
                                 time_unit = "years",
                                 start_date = start_date,
                                 end_date = end_date,
                                 minimum_acceptable_return = 0,
                                 shrink="None",
                                 expo_factor=0.5)
  
  all_rat[i,8] <-  sortino_ratio(returns = WikiDaily_merge$ret, 
                                 rf = WikiDaily_merge$rf, 
                                 closing_prices = WikiDaily_merge$close, 
                                 time_adj = FALSE,
                                 time_unit = "years",
                                 start_date = start_date,
                                 end_date = end_date,
                                 minimum_acceptable_return = 0,
                                 shrink="exponential",
                                 expo_factor=0.5)
  
  all_rat[i,9] <-  sortino_ratio(returns = WikiDaily_merge$ret, 
                                 rf = WikiDaily_merge$rf, 
                                 closing_prices = WikiDaily_merge$close, 
                                 time_adj = TRUE,
                                 time_unit = "years",
                                 start_date = start_date,
                                 end_date = end_date,
                                 minimum_acceptable_return = 0,
                                 shrink="exponential",
                                 expo_factor=0.5)
  
  all_rat[i,10] <-  gain_loss_ratio(returns = WikiDaily_merge$ret, 
                                   rf = WikiDaily_merge$rf, 
                                   closing_prices = WikiDaily_merge$close,
                                   time_adj = FALSE,
                                   time_unit = "years",
                                   threshold = WikiDaily_merge$AVG_ret,
                                   start_date=start_date,
                                   end_date=end_date,
                                   shrink = "None",
                                   expo_factor=0.5)
  
  all_rat[i,11] <-  gain_loss_ratio(returns = WikiDaily_merge$ret, 
                                    rf = WikiDaily_merge$rf, 
                                    closing_prices = WikiDaily_merge$close,
                                    time_adj = TRUE,
                                    time_unit = "years",
                                    threshold = WikiDaily_merge$AVG_ret,
                                    start_date=start_date,
                                    end_date=end_date,
                                    shrink = "None",
                                    expo_factor=0.5)
  
  all_rat[i,12] <-  gain_loss_ratio(returns = WikiDaily_merge$ret, 
                                    rf = WikiDaily_merge$rf, 
                                    closing_prices = WikiDaily_merge$close,
                                    time_adj = FALSE,
                                    time_unit = "years",
                                    threshold = WikiDaily_merge$AVG_ret,
                                    start_date=start_date,
                                    end_date=end_date,
                                    shrink = "exponential",
                                    expo_factor=0.5)
  
  all_rat[i,13] <-  gain_loss_ratio(returns = WikiDaily_merge$ret, 
                                    rf = WikiDaily_merge$rf, 
                                    closing_prices = WikiDaily_merge$close,
                                    time_adj = TRUE,
                                    time_unit = "years",
                                    threshold = WikiDaily_merge$AVG_ret,
                                    start_date=start_date,
                                    end_date=end_date,
                                    shrink = "exponential",
                                    expo_factor=0.5)
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
}

# fill NA values
all_rat_clean <- na.omit(all_rat)
# remove rows with inf values
all_rat_clean <- all_rat_clean[is.finite(rowSums(all_rat_clean)),]
# save to avoid recalculating
saveRDS(all_rat_clean, file="All_ratios.RDS")

# IMPORTANT: load the result from previous run to avoid long calculation times
all_rat_clean <- readRDS("ALL_ratios.RDS")

##########

# start plotting ratios. One transition plot (ratios over zero), One histogram for ratios equal to zero.

#########

plot_ratios <- function(all_ratios, ratio_A, ratio_B){
  # initiate table to collect transition probabilities
  table <- matrix(data=0, nrow=10, ncol=10)
  colnames(table) <- 1:10
  rownames(table) <- 1:10
  
  # select the ratios to be compared
  ratios_compared <- all_ratios[,c(ratio_A, ratio_B)]
  
  # only compare inv IDs when ratio greater than zero.
  ratios_compared_above_zero <- ratios_compared[ratios_compared[1]>0,]
  
  # calculate deciles
  ratios_deciles <- data.frame(c(decile(ratios_compared_above_zero[,1])), c(decile(ratios_compared_above_zero[,2])))
  
  # fill transistion probabilities
  for(row in 1:nrow(ratios_deciles)){
    table[ratios_deciles[row,1], ratios_deciles[row,2]] <- table[ratios_deciles[row,1], ratios_deciles[row,2]] +1
  }
  
  # generate the plot
  hist3D(x=1:10, y=1:10, z = table/IDs, scale = FALSE, expand = 20, bty = "g", phi = 25,
         col = NULL, border = "black", shade = 0.2, ltheta = 90,
         space = 0.2, ticktype = "detailed", d = 2, colvar = table,
         xlab=paste("Decile " ,ratio_A) , ylab=paste("Decile ", ratio_B), main=paste("Transition of " , ratio_A , " to " , ratio_B))
  
}


#########

# choose 2 arbitrary ratios from columns(all_rat) and compare
plot_ratios(all_ratios=all_rat_clean, ratio_A = "Sharpe_normal", ratio_B ="Sharpe_time")


correlation_A_B <- function(all_ratios, ratio_A, ratio_B){
  return(cor(all_ratios[,c(ratio_A)], all_ratios[,c(ratio_B)]))
}

correlation_A_B(all_ratios=all_rat_clean, ratio_A = "Sharpe_normal", ratio_B ="Sharpe_time")
all_rat_clean[,c("Sharpe_normal")]





