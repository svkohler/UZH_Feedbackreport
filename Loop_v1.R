setwd("C:/Users/Sven Kohler/Desktop/UZH")

library(knitr)
library(markdown)
library(rmarkdown)

# loading data
library(readstata13)
EuroStoxx <- read.dta13("EuroStoxx50.dta")
FactorReturns <- read.dta13("FactorReturns.dta")
WikiDaily <- read.dta13("WikiDaily.dta")

# manipulating data
WikiDaily_mod <- WikiDaily
WikiDaily_mod$wikiId <- as.factor(WikiDaily_mod$wikiId)
levels(WikiDaily_mod$wikiId) <- c(1:5880)
WikiDaily_mod <- WikiDaily_mod[,1:5]

x <- 1:3


for(i in x){
  #get individual WikiId
  WikiDaily_Id <- WikiDaily_mod[WikiDaily_mod$wikiId==i,]
  #extract date
  start_date <- head(WikiDaily_Id$date, 1)
  end_date <- tail(WikiDaily_Id$date, 1)
  
  rmarkdown::render(input = "C:/Users/Sven Kohler/Desktop/UZH/Feedbackreport2_3_loop.Rmd", 
                    output_format = "pdf_document",
                    output_file = paste("test_report_", i,"_", Sys.Date(), ".pdf", sep=''),
                    output_dir = "C:/Users/Sven Kohler/Desktop/UZH/LoopReports",
                    params = list(date = paste(as.character(start_date),as.character(end_date), sep="-")))
}

# TestTest