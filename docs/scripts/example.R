#### Building and analyzing a price index with R ####
# Steve Martin
# June 2020

#---- Bring in libraries ----
library(dplyr)
library(gpindex)

#---- Bring in data ----
source('https://raw.githubusercontent.com/marberts/H-PPD-04/master/get_data.R')

#---- Step 1: Make the weights ----
# Make product weights
weights_prov <- weights %>%
  group_by(year, province) %>%
  summarize(weight = sum(weight)) %>%
  mutate(share_prov = weights_scale(weight)) %>%
  select(-weight)