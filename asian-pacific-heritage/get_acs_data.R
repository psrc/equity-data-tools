# Packages for Data Cleaning/Processing
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(tibble)

# Packages for Census Data
library(tidycensus)
library(censusapi)

library(openxlsx)

source("functions.R")
source("inputs.R")

Sys.setenv(CENSUS_KEY='c4780eb03010d73b7ae4e6894c1592375e545a21')
results <- NULL
setwd("~/GitHub/equity-data-tools/asian-pacific-heritage")

yrs<-c(2019)


for (c in names(census.tables)){
  yr_order <-0
  
  for (analysis.yr in yrs){
    yr_order<-yr_order+1
    c.yr <- as.character(analysis.yr)
    label.yr <- analysis.yr
    print(c.yr)
    print(c)
    print(census.tables[[c]][[2]])
    tbl<- get.county.census(c.type=census.tables[[c]][[3]], c.yr = analysis.yr, c.table = census.tables[[c]][[1]])
    
    temp_full<- tbl%>%
      #mutate(across(where(is.numeric), label_comma(accuracy = 1)))%>%
      mutate_if(is.character, str_replace_all, pattern=",", replacement= "")%>%
      mutate_at(vars(!Geography), as.numeric)
    
    #colnames(temp_full)[which(colnames(temp_full)=='Estimate:')]="Estimate Total"
    
    temp_full<-temp_full %>% 
      select_all(~gsub("Estimate:", "Estimate Total",.))
    # Only compute for things that naturally have shares
    # this is messy, I should figure out a better way to handle it
    
    if(!startsWith(census.tables[[c]][[2]], "Median")){
    temp_shares<-data.frame()
    for(county in counties){
      
      temp2<-temp_full %>%
        filter(Geography==county) %>%
        select(-Geography)%>%
        mutate(across(where(is.numeric)), Shares=. / `Estimate Total`) %>%
        add_column(Geography=county, .before = "Shares")
      
      temp_shares <- rbind(temp_shares,temp2)
      
      
      
      }
    }
    
    if(yr_order==1){
      if(!startsWith(census.tables[[c]][[2]], "Median")){
      temp_shares_first = temp_shares}
      temp_full_first = temp_full
    }
  
  }
  yrs_chars<-c(as.character(yrs[1]), as.character(yrs[2]))
  
  tbl_totals.merged<-merge(temp_full_first, temp_full, by ='Geography', suffixes = yrs_chars)
  write.csv(tbl_totals.merged, file=paste(census.tables[[c]][[2]],'totals', '.csv'))
  
  if(!startsWith(census.tables[[c]][[2]], "Median")){
  tbl_shares.merged<-merge(temp_shares_first, temp_shares, by = 'Geography', suffixes =yrs_chars)
  write.csv(tbl_shares.merged,file= paste(census.tables[[c]][[2]],'shares', '.csv'))}
  
} # end loop for tables



