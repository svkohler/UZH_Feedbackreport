# Transition plot for time adjusted and standard sortino ratio

# !!! load all the packages from the Loop_v1.R file

# set the necessary working directory
setwd("C:/Users/Sven Kohler/Desktop/UZH")

# load functions file
source("functions_report.R")

# load the data
library(readstata13)
EuroStoxx <- read.dta13("EuroStoxx50.dta")
FactorReturns <- read.dta13("FactorReturns.dta")
WikiDaily <- read.dta13("WikiDaily.dta")

# manipulating data
WikiDaily_mod <- WikiDaily
WikiDaily_mod <- WikiDaily_mod[,1:5]
WikiDaily_mod$wiki_nr <- as.factor(WikiDaily_mod$wikiId)
levels(WikiDaily_mod$wiki_nr) <- c(1:5880) # previous inspection: found 5880 different IDs.

# setting the name of the first column of the factor returns for later merging
colnames(FactorReturns)[1] <- "date"



limit <- 5880

Ids <- 1:limit

#define days
days_ret <-  365 #calender days
days_vola <- 252 #average number of trading days in a year

# initialize matrix to collect different sortino ratios
mat_sort_rat <- matrix(data=NA,nrow = limit, ncol = 3)
mat_sort_rat[,1] <- 1:limit


for(i in Ids){
  # get dataframe og individual WikiId
  WikiDaily_Id <- WikiDaily_mod[WikiDaily_mod$wiki_nr==i,]
  # extract date
  start_date <- head(WikiDaily_Id$date, 1)
  end_date <- tail(WikiDaily_Id$date, 1)
  # check dates for hard starts/ends
  
  # merge the different dataframes on date
  WikiDaily_merge <- merge_mult(list_of_dfs=list(WikiDaily_Id, FactorReturns, EuroStoxx), by=c("date"))
  
  # check if not empty
  if(nrow(WikiDaily_merge)==0){
    print(sprintf("Dataframe for ID %i is empty. Time intervals do not intersect", i))
    next()
  }
  
  # calculate sortino ratio
  mat_sort_rat[i,2] <- sortino_ratio(list_of_returns = WikiDaily_merge$ret, 
                               list_of_rf = WikiDaily_merge$rf, 
                               closing_prices = WikiDaily_merge$close, 
                               time_adj = FALSE,
                               time_unit = "years",
                               start_date = start_date,
                               end_date = end_date,
                               minimum_acceptable_return = 0)
  
  mat_sort_rat[i,3] <-  sortino_ratio(list_of_returns = WikiDaily_merge$ret, 
                                         list_of_rf = WikiDaily_merge$rf, 
                                         closing_prices = WikiDaily_merge$close, 
                                         time_adj = TRUE,
                                         time_unit = "years",
                                         start_date = start_date,
                                         end_date = end_date,
                                         minimum_acceptable_return = 0)
  
}

mat_sort_rat_clean <- na.omit(mat_sort_rat)

mat_sort_rat_deciles <- cbind(mat_sort_rat_clean, decile(mat_sort_rat_clean[,2]), decile(mat_sort_rat_clean[,3]))
colnames(mat_sort_rat_deciles) <- c("ID", "Sortino Ratio", "time-adj. Sortino Ratio", "Decile standard", "Decile adjusted")

table <- matrix(data=0, nrow=10, ncol=10)
colnames(table) <- 1:10
rownames(table) <- 1:10

for(row in 1:nrow(mat_sort_rat_deciles)){
  table[mat_sort_rat_deciles[row,4],mat_sort_rat_deciles[row,5]] <- table[mat_sort_rat_deciles[row,4],mat_sort_rat_deciles[row,5]] +1
}

library("plot3D")
hist3D(x=1:10, y=1:10, z = table/limit, scale = FALSE, expand = 20, bty = "g", phi = 25,
       col = NULL, border = "black", shade = 0.2, ltheta = 90,
       space = 0.2, ticktype = "detailed", d = 2, colvar = table,
       xlab="Decile standard Sortino", ylab="Decile time adjusted Sortino", main="Transition of standard vs. time adj. Sortino ratios")



