setwd("C:/Users/Sven Kohler/Desktop/UZH")

library(knitr)
library(markdown)
library(rmarkdown)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(lattice)
library(kableExtra)
library(scales)
library(lubridate)
library(StatMeasures)

# load functions file
source("functions_report.R")

# loading data
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

x <- 1:3

#define days
days_ret <-  365 #calender days
days_vola <- 252 #average number of trading days in a year


for(i in x){
  #get dataframe og individual WikiId
  WikiDaily_Id <- WikiDaily_mod[WikiDaily_mod$wiki_nr==i,]
  #extract date
  start_date <- head(WikiDaily_Id$date, 1)
  end_date <- tail(WikiDaily_Id$date, 1)
  #extract real WikiID
  ID <- head(WikiDaily_Id$wikiId, 1)
  
  # merge the different dataframes on date
  WikiDaily_merge <- merge_mult(list_of_dfs=list(WikiDaily_Id, FactorReturns, EuroStoxx), by=c("date"))
  
  # check if not empty
  if(nrow(WikiDaily_merge)==0){
    print(sprintf("Dataframe foür ID %i ist leer. Zeitabschnitte überschneiden sich nicht.", i))
    next()
  }
  
  rmarkdown::render(input = "C:/Users/Sven Kohler/Desktop/UZH/R project/UZH_Feedbackreport/Feedbackreport2_3_loop.Rmd", 
                    output_format = "pdf_document",
                    output_file = paste("test_report_", i,"_", Sys.Date(), ".pdf", sep=''),
                    output_dir = "C:/Users/Sven Kohler/Desktop/UZH/LoopReports",
                    params = list(date = paste(as.character(start_date),as.character(end_date), sep="-"), Id = ID))
}
