#this script scrapes NHL draft pick data from hockeydb.com
#for every NHL amateur draft from its first draft in 1963
#through its latest entry draft in 2019
#the NHL amateur draft was renamed to entry draft in 1978

#load required packages
library(tidyverse) #tidyverse for tidy data manipulation
library(rvest) #rvest for pulling HTML data

#write function to scrape NHL draft data by year from hockeydb.com
draft_scrape <- function(x, year){#x should be a df or null object, and year should be a 4 digit integer
  draft_type <- ifelse(year<=1978, 'a', 'e')#drafts before 1978 have 'a' suffix while all other have 'e' suffix
  
  
  url <- paste("https://www.hockeydb.com/ihdb/draft/nhl",#creating the url to pull data from by using 'year' input 
               year,draft_type,".html", sep = "") #as well as suffix previously determined
  page <- read_html(url) #reads in the html page from url
  df <- html_table(page, fill = TRUE)[[1]] #pull the first table in the data and convert to df
  
  #cleaning data
  colnames(df) <- make.names(df[1,])#take first row of table and use as colnames
  df <- df[-1,]#remove first row of data
  df <- df%>%
    mutate(Round = as.numeric(Round))%>% #converting Round to numeric coercing to NA's
    na.omit(Round)#omitting NA's in Round to remove unnecessary rows
  df$Draft.Year <- year #adding the year of the draft as a column in df
  
  x <- rbind(x, df) #binding rows of new df to existing df
  return(x) #return newly binded df
}

draft <- NULL #initialize draft object

years <- c(1963:2019) #set the years of the draft that you'd like to scrape

#loop scraping specific years of nhl draft
for(i in years){
draft <- draft_scrape(draft, i)#calling the scraping function for each specified year
}

draft_write_path <- "NHL Draft Data.csv" #path to write csv

write.csv(draft, draft_write_path, row.names = FALSE)#write draft df as csv
