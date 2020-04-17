#### Building and analyzing a price index with R ####
# Steve Martin
# March 30 2020

#---- Bring in libraries ----
library(dplyr)
library(ppd)

#---- Bring in data ----
source('https://raw.githubusercontent.com/marberts/H-PPD-04/master/get_data.R')

#---- Step 1: Make the weights ----
# Make product weights
weights_prod <- weights %>%
  group_by(year, province) %>%
  mutate(share_prod = weight/sum(weight)) %>%
  select(-weight)

#---- Step 9: Quarter the monthly indices ----
# Turn my monthly index into a quarterly index
index_stc <- aggregate(index ~ quarter, transform(quarter = year_quarter(period)), mean)