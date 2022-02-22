###SCRIPT FOR GENERATING SYNTHETIC DATA FOR MODULE 4

#package installation
install.packages("tidyr", "dplyr", "MASS", "devtools", "TimeWarp");
devtools::install_github("ppd-dpp/calpr");

#load packages
library(MASS);
library(tidyr);
library(dplyr);
library(calpr);
library(TimeWarp);


###################
###This portion of the script retrieves (or at least tries to retrieve) the original regression parameters used to create the synthetic data.

#load data
#These URLs *shouldn't* change, because that's not how GitHub works, just in case, you can download the old verisons of the data and load them from your local machine. I've commented out the code to do that below. (Set the working directory and rename the files appropriately before doing this.)
dat_gps <- read.csv("https://raw.githubusercontent.com/ppd-dpp/price-index-course/33c4ead4aa61ccb312a47a59efcb3028a193233f/csv/dat_gps.csv", stringsAsFactors = FALSE);
dat_micro <- read.csv("https://raw.githubusercontent.com/ppd-dpp/price-index-course/33c4ead4aa61ccb312a47a59efcb3028a193233f/csv/dat_micro.csv", stringsAsFactors = FALSE);
weights <- read.csv("https://raw.githubusercontent.com/ppd-dpp/price-index-course/33c4ead4aa61ccb312a47a59efcb3028a193233f/csv/weights.csv", stringsAsFactors = FALSE);
#dat_gps <- read.csv("dat_gps_old.csv", stringsAsFactors = FALSE);
#dat_micro <- read.csv("dat_micro_old.csv", stringsAsFactors = FALSE);
#weights <- read.csv("weights_old.csv", stringsAsFactors = FALSE);


#make transaction month variable for product K data
dat_micro$period <- cut(as.Date(dat_micro$period), "month"); 

#reshape data for products A to J
dat_gps_long <- dat_gps %>%
  pivot_longer(c(price1, price2, price3), values_to = "price") %>%
  select(!(name));

#bind the two data sets (columns automatically matched by name)
dat_all <- rbind(dat_gps_long, dat_micro);

#drop the link month data
dat_all <- dat_all %>%
  filter(!(period == "2019-01-01" & year == "2018-01-01"));

#convert the period to a sequential number from 0 to 23 for regression calculation
dat_all$periodnum <- with(dat_all,
                          (as.numeric(substr(period, 1, 4))-2018)*12
                          + as.numeric(substr(period, 6, 7))
                          -1
                          );

#take logarithms of prices
dat_all$logprice <- log(dat_all$price);

#split data by province and product
dat_all_split <- split(dat_all, list(dat_all$province, dat_all$product));

#run regressions to get parameters for each price process
logprice_regs <- lapply(dat_all_split, function(x) summary(lm(logprice ~ periodnum, data = x)));

#extract intercept term, slope coefficient, and standard error of regression
logprice_regs_params <- lapply(logprice_regs, function(x) {
  c(
    x$coefficients[1,1], x$coefficients[2,1], x$sigma
  );
});

#store as table
price_process_params_table <- as.data.frame(do.call(rbind, logprice_regs_params));
colnames(price_process_params_table) <- c("intercept", "slope", "ser"); #assign column names
price_process_params_table$province <- substr(rownames(price_process_params_table), 1, 2); #create province column
price_process_params_table$product <- substr(rownames(price_process_params_table), 4, 4); #create product column
price_process_params_table <- select(price_process_params_table, c(province, product, intercept, slope, ser)); #rearrange the columns
rownames(price_process_params_table) <- NULL; #delete unnecessary row names

#save price data table (make sure to set working directory appropriately before doing this)
saveRDS(price_process_params_table, "price_process_params_table.RDS");

#save weight data table (make sure to set working directory appropriately before doing this)
saveRDS(weights, "weights_table.RDS");



###############
###This portion of the script constructs the random price data from the parameters laid out in price_process_params_table.rds
#You can start from here if you already have the RDS file

#make sure working directory is appropriately set before running this

#read in the parameters table if you haven't run the previous portion of the script; otherwise, it'll keep what you've created
if(!(exists("price_process_params_table"))) {
  price_process_params_table <- readRDS("price_process_params_table.RDS");
}

#read in the parameters table if you haven't run the previous portion of the script; otherwise, it'll keep what you've created
if(!(exists("weights"))) {
  weights <- readRDS("weights_table.RDS");
}


#Procedure for generating random data
#1. Build VCM.
##a. 3 repetitions of A-J (100*3=300) and 20 repetitions of K (20*10=200), for a 500 by 500 matrix
##b. Take diagonal to be arithmetic average of variances outside of Prairies and Atlantic Canada, 1.1*var in Prairies, 0.9*var in Atlantic, repeated as described above
##c. Assign rownames and colnames to correspond to province.product and quote# (1-3 for A-J, 1-20 for K)
##d. Loop over provinces, and set all within-province-product covariances to 0.85 times var
##e. Loop over provinces, and set all within-province, cross-product covariances to 0.8 times var
##f. For Atlantic provinces, set cross-province covariances for each product (ignoring quote#) to 0.75 times var
##g. For Prairies, set Alberta's (48) covariance with Saskatchewan (47) to be -0.75 times var in Prairies
#2. Simulate 24 copies
#3. Apply regression parameters and exponentiate to generate prices
#4. Generate GPS data
##a. Delete third quotes for 59.B (third brand's version of product B never gained regulatory approval in B.C.)
##b. Generate link month data and merge
##c. Drop I for 2018 and J for 2019, except in link month (i.e. J in January 2019 with year set to 2018)
##d. Put data into proper format for export
##e. Save CSV
#5. Generate micro data
##a. generate key for keeping 5-20 transactions for each province-period tuple
##b. Remove quotes accordingly
##c. randomize dates within each month
##d. Generate link month data and merge back into original data set
##e. Refactor
##f. Shuffle
##g. Put into proper format for export
##h. Export the data
#6. Reformat weights table and export


#1. Build VCM
#grab arithmetic mean of regression variances (will be greater than geometric mean, but this is on purpose; want slightly higher variances than in previous data set)
avgvar <- mean(price_process_params_table$ser^2);

##b. Build diagonal
price_process_params_table <- price_process_params_table %>%
  mutate(variances = case_when(
    substr(province, 1, 1) == 1 ~ 0.9*avgvar,
    substr(province, 1, 1) == 4 ~ 1.1*avgvar,
    TRUE ~ avgvar
  ));

vcov_sim <- diag(
  c(
    rep(
      (price_process_params_table %>%
         filter(product != "K"))$variances,
      3
    ),
    rep(
      (price_process_params_table %>%
         filter(product == "K"))$variances,
      20
    )
  )
);

##c. Assign rownames and colnames
vcov_namevec <- c(
  apply(as.array(1:3), 1, function(x) paste(filter(price_process_params_table, product != "K")$province, filter(price_process_params_table, product != "K")$product, as.character(x), sep = ".")),
  apply(as.array(1:20), 1, function(x) paste(filter(price_process_params_table, product == "K")$province, filter(price_process_params_table, product == "K")$product, as.character(x), sep = "."))
);

rownames(vcov_sim) <- vcov_namevec;
colnames(vcov_sim) <- vcov_namevec;

##d through h
for(i in 1:length(vcov_namevec)) {
  rowsplit_name <- unlist(strsplit(vcov_namevec[i], ".", fixed = TRUE));
  
  for(j in 1:length(vcov_namevec)) {
    colsplit_name <- unlist(strsplit(vcov_namevec[j], ".", fixed = TRUE));
    
    if(rowsplit_name[1] == colsplit_name[1]) { #same province
      
      if(rowsplit_name[2] == colsplit_name[2]) { #same product
        
        if(rowsplit_name[3] != colsplit_name[3]) { #different quote
          vcov_sim[i,j] <- 0.85*avgvar;
        }
        
      } else { #different products
        
        vcov_sim[i,j] <- 0.8*avgvar;
        
      }
      
    } else if(substr(rowsplit_name[1],1,1) == "1" & substr(colsplit_name[1],1,1) == "1") { #atlantic canada (different provinces, since all pairs of same provinces captured above)
      vcov_sim[i,j] <- 0.75*avgvar
      
    } else if(xor(rowsplit_name[1] == "47", colsplit_name[1] == "47") & xor(rowsplit_name[1] == "48", colsplit_name[1] == "48")) { #one province is saskatchewan and the other is alberta
      vcov_sim[i,j] <- -0.75*avgvar;
    } 
  }
}


#2. simulate 24 copies of the noise components
#set random seed
set.seed(1639754421); #took UNIX timestamp at 2021-12-17 10:20:21 EST with as.numeric(Sys.time())

noise_components <- as.data.frame(t(mvrnorm(24, rep(0,500), vcov_sim))); #generate the random Gaussian errors according to the VCM defined above
colnames(noise_components) <- seq(as.Date("2018-01-01"), as.Date("2019-12-01"), "month");
noise_components$province <- substr(rownames(noise_components), 1, 2);
noise_components$product <- substr(rownames(noise_components), 4, 4);


#3. generate prices
prices <- apply(price_process_params_table, 1, function(x) {
  y <- noise_components %>%
    filter(province == x["province"], product == x["product"]) %>%
    select(-c(province, product));
  
  t(apply(y, 1, function(z) {
    round(exp(as.numeric(x["intercept"]) + as.numeric(x["slope"])*as.vector(1:24) + as.numeric(z)), 2); #prices rounded to two digits
  }));
});

allprices_table <- as.data.frame(do.call(rbind, prices)); #store in a single table
colnames(allprices_table) <- seq(as.Date("2018-01-01"), as.Date("2019-12-01"), "month");
allprices_table$province <- substr(rownames(allprices_table), 1, 2);
allprices_table$product <- substr(rownames(allprices_table), 4, 4);
allprices_table$quotenum <- paste("price", substr(rownames(allprices_table), 6, length(rownames(allprices_table))), sep = "");


#4. generate gps data
gps_prices <- allprices_table %>%
  filter(product != "K") %>%
  pivot_longer(!(c(province, product, quotenum)), names_to = "period") %>% 
  pivot_wider(names_from = quotenum, values_from = value) %>% #put each quote in its own column
  mutate(year = year(period)); #create year variable

#a. Delete third quotes for 59.B
gps_prices_remove <- gps_prices %>%
  mutate(price3 = ifelse(province == "59" & product == "B", NA, price3));

#b to d. generate link month data, merge, delete I from 2018 and delete J from 2019 (except link month), sort, put columns in proper format, and move columns
gps_data <- gps_prices_remove %>%
  filter(period == "2019-01-01") %>%
  mutate(year = year(period, shift = -12)) %>%
  rbind(gps_prices_remove) %>%
  filter(!(product == "J" & year == "2019-01-01")) %>%
  filter(!(product == "I" & year == "2018-01-01")) %>%
  arrange(year, period, province, product) %>%
  mutate(ea = paste(province, product, sep = "")) %>% #create elemental aggregate label variable that has product and province already merged
  mutate(period = paste(substr(period, 1, 4), substr(period, 6, 7), sep = "")) %>% #reformat period variable as YYYYMM
  mutate(basket = substr(year, 1, 4)) %>% #reformat year variable as YYYY
  select(ea, period, price1, price2, price3, basket);
  
#e. save the table as CSV
write.csv(gps_data, "gps_prices.csv", row.names = FALSE);


#5. generate micro data
micro_prices <- allprices_table %>%
  filter(product == "K") %>%
  pivot_longer(!(c(province, product, quotenum)), names_to = "period") %>% 
  pivot_wider(names_from = quotenum, values_from = value) %>% #put each quote in its own column
  mutate(year = year(period)); #create year variable

#a. randomly keep between 5 and 20 transactions for each province-period tuple
set.seed(1639757509); #took UNIX timestamp at 2021-12-17 11:11:49 EST with as.numeric(Sys.time())
keep_quotes <- ceiling(runif(dim(micro_prices)[1], 4, 20)); #generate a random variable telling how many to keep

#b. remove quotes as per the random vector
quotes_remaining <- t(apply(cbind(micro_prices[,4:23], keep_quotes), 1, function(x) {
  apply(as.array(1:20), 1, function(y) {
    ifelse(y <= x[21], x[y], NA)
  })
}));

micro_prices_remain <- cbind(micro_prices[, 1:3], quotes_remaining, micro_prices[, "year"]); #merge remaining quotes back into price table

#c. randomize dates within each month
set.seed(1639757968); #took UNIX timestamp at 2021-12-17 11:19:28 EST with as.numeric(Sys.time())
date_offset <- runif(dim(micro_prices_remain)[1]);
micro_prices_remain$period <- apply(as.array(seq_along(date_offset)), 1, function(i) {
  if(substr(micro_prices_remain[i, "period"], 6, 7) == "02") {
    offset <- floor(28*date_offset[i]);
  } else if (substr(micro_prices_remain[i, "period"], 6, 7) %in% c("04", "06", "09", "11")) {
    offset <- floor(30*date_offset[i]);
  } else {
    offset <- floor(31*date_offset[i]);
  }
  ifelse(offset == 0, micro_prices_remain[i, "period"], dateShift(micro_prices_remain[i, "period"], k.by = offset));
});

#d-g. generate link month data, refactor, sort, and move columns
micro_data <- micro_prices_remain %>%
  filter(substr(period, 1, 7) == "2019-01") %>% #grab the link month data to duplicate it
  mutate(year = year(period, shift = -12)) %>%
  rbind(micro_prices_remain) %>%
  pivot_longer(!(c(province, product, period, year)), names_to = "quotenum", values_to = "price") %>%
  na.omit() #remove non-existent prices

set.seed(1639758013); #took UNIX timestamp at 2021-12-17 11:20:13 EST with as.numeric(Sys.time())
micro_data <- micro_data[sample(nrow(micro_data)),] %>% #shuffle the rows, then sort by year and period, so provinces aren't all grouped together
  arrange(year) %>%
  select(-c(quotenum)) %>%
  mutate(basket = substr(year, 1, 4)) %>% #put year in YYYY format
  select(product, province, period, price, basket);

#h. save the table as CSV
write.csv(micro_data, "micro_prices.csv", row.names = FALSE);


#6. reformat weights table
weights_table <- weights %>%
  mutate(ea = paste(province, product, sep = "")) %>% #generate elemental aggregate label
  mutate(region = substr(province, 1, 1)) %>% #generate region variable
  mutate(year = substr(year, 1, 4)) %>% #put year in YYYY format
  mutate(country = 0) %>% #create top level of pias
  select(c(level1 = country, level2 = region, level3 = province, ea, weight, year));

#save the table as CSV
write.csv(weights_table, "weights.csv", row.names = FALSE);

