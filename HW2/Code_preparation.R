library(xts)
library(readr)
library(lubridate)
library(quantmod)
library(moments)

########################
### Data preparation ###
########################

### Loading the data ###

yahoo_data <- readRDS("list_yahoo.RData") #Adjusted close prices and volumes
market_cap <- readRDS("market_cap_data.RData") #Market capitalization
fffactors_monthly <- read_csv("F-F_Research_Data_Factors.CSV", skip = 2, n_max = 1149) #Need to limit the number of rows since the file contains yearly data as well
fffactors_daily <- read_csv("F-F_Research_Data_Factors_daily.CSV", skip = 3, n_max = 25210) #Last row is some copyright => skip
fffactors_monthly <- as.data.frame(fffactors_monthly) #Converting from tibble to a data frame
fffactors_daily <- as.data.frame(fffactors_daily) #Converting from tibble to a data frame
colnames(fffactors_daily)[1] <- "Date"
colnames(fffactors_monthly)[1] <- "Date"

#### Extracting the Adjusted Close Price ###

daily_adj_close <- lapply(yahoo_data, "[", , 6) #The unused argument is not an error, we have to skip it to specify columns
rm(yahoo_data) #Removing from the environment since is quite large and we do not need it

### Extracting market risk premium and the risk-free rate ###

fffactors_daily$Date <- as.Date(as.character(fffactors_daily$Date), "%Y%m%d") #Creating a date (daily)
fffactors_monthly$Date <-  as.Date(paste0(as.character(fffactors_monthly$Date),"01"), "%Y%m%d") + months(1) - days(1) #Creating a date (end of month)
market_prem_daily <- xts(fffactors_daily$`Mkt-RF`, order.by = fffactors_daily$Date)
names(market_prem_daily) <- "Mkt_RF_daily"
market_prem_monthly <- xts(fffactors_monthly$`Mkt-RF`, order.by = fffactors_monthly$Date)
names(market_prem_monthly) <- "Mkt_RF_monthly"
rf_daily <- xts(fffactors_daily$RF, order.by = fffactors_daily$Date)
names(rf_daily) <- "RF_daily"
rf_monthly <- xts(fffactors_monthly$RF, order.by = fffactors_monthly$Date)
names(rf_monthly) <- "RF_monthly"
rm(fffactors_daily, fffactors_monthly) #Removing from the environment since they are unnecessary

### Calculating excess returns ###

calc_excess_returns <- function(xts_object, daily = T) { #Defining a function to calculate excess returns
  if (daily == T) { #For daily returns
    result <- dailyReturn(xts_object)
    result <- result[2:nrow(result)] #Disregard the first observation (dailyReturn sets it to 0)
    risk_free_rate <- rf_daily[index(result)] #Define daily risk free rate
  } else { #For monthly returns
    result <- monthlyReturn(xts_object)
    risk_free_rate <- rf_monthly[index(result)]
  }
  return(result - risk_free_rate) #Substract risk free rate from the returns
}
#Daily excess returns
daily_returns <- lapply(daily_adj_close, calc_excess_returns) #Applying the function
daily_return_names <- gsub(".Adjusted", ".DailyReturns", unlist(lapply(daily_adj_close, names))) #New names
for (i in 1:length(daily_returns)) { #Renaming the columns for clarity
  names(daily_returns[[i]]) <- daily_return_names[i]
}
#Monthly excess returns
monthly_returns <- lapply(daily_adj_close, calc_excess_returns, daily = F)
monthly_return_names <- gsub(".Adjusted", ".MonthlyReturns", unlist(lapply(daily_adj_close, names))) #New names
for (i in 1:length(monthly_returns)) { #Renaming the columns for clarity
  names(monthly_returns[[i]]) <- monthly_return_names[i]
}
#Merge monthly excess returns
monthly_returns_merged <- do.call(merge.xts, monthly_returns)
saveRDS(monthly_returns_merged, file = "monthly_excess_returns.RData") #Saving for future use

### Defining a generic function to calculate a monthly measure from past 12 months of daily data ###

calc_measure <- function(xts_object, func, measure_name) { #Expects a series of monthly returns, the function to calculate therequired measure (skewness, beta..) and the name of the measure
  result <- xts(rep(NA, nrow(xts_object)), order.by = index(xts_object)) #Empty xts object for the results
  names(result) <- gsub("MonthlyReturns", measure_name, names(xts_object)) #Naming the resulting object for clarity 
  ind <- which(monthly_return_names == names(xts_object)) #Get the rank of the stock in the list (1-250)
  for (i in 1:nrow(xts_object)) { #Loop through the rows of the object
    end_date <- index(xts_object)[i] #The last day that the daily returns will be used for computations
    start_date <- end_date %m-% months(12) #The start date is 12 months back as required
    df_to_use <- daily_returns[[ind]][paste0(start_date, "/", end_date)] #Extracting the subset we will use for the computation (12 months of daily returns)
    if (length(df_to_use) < 200){ #We require at least 200 data points, otherwise we won't calculate
      next
    } else {
      result[i] <- func(na.omit(df_to_use)) #Apply the specified function
    }
  }
  return(result)
}

### Calculating skewness ###

skewness_data <- lapply(monthly_returns, calc_measure, func = skewness, measure_name = "Skewness") #Applying the function (skewness from the moments package)
#Merging skewness data
skewness_data_merged <- do.call(merge.xts, skewness_data)
saveRDS(skewness_data_merged, file = "skewness.RData") #Saving for future use

### Calculating betas ###

#Defining a function to calculate the beta (based on a series of daily returns)
calc_beta <- function(xts_object) {
  model_res <- lm(xts_object ~ market_prem_daily[index(xts_object)]) #Regress market premium on daily excess returns
  return(model_res$coefficients[[2]])
}
#Applying the function
betas <- lapply(monthly_returns, calc_measure, func = calc_beta, measure_name = "Beta")
#Merging the betas
betas_merged <- do.call(merge.xts, betas)
saveRDS(betas_merged, file = "betas.RData") #Saving for future use

### Extracting monthly market cap data ###

market_cap_monthly <- lapply(market_cap, to.monthly, OHLC = F, indexAt = "lastof") #Converting daily to monthly
market_cap_monthly_merged <- do.call(merge.xts, market_cap_monthly) #Merging
saveRDS(market_cap_monthly_merged, file = "market_cap_monthly.RData") #Saving for future use

### Calculating size ###

size <- log(market_cap_monthly_merged)
names(size) <- gsub(".Adjusted", ".Size", unlist(lapply(daily_adj_close, names))) #New names
saveRDS(size, file = "size.RData") #Saving for future use

#####################################
### Univariate portfolio analysis ###
#####################################

#Load relevant data
rm(list = ls()) #Removes everything from the environment
monthly_returns <- readRDS("monthly_excess_returns.RData")
skewness_data <- readRDS("skewness.RData")
betas <- readRDS("betas.RData")
market_cap <- readRDS("market_cap_monthly.RData")
size <- readRDS("size.RData")

### Defining a function to perform a univariate sort ###
univariate_sort <- function(sort_variable) {
  for (i in 1:nrow(data_to_use)) { #Looping through the rows (periods)
    
  }
}

##############################################################################################

#Checking NAs
for (i in list(monthly_returns, skewness_data, betas, market_cap, size)) {
  na_count <- apply(i, 2, function(x) {sum(is.na(x))})
  print(sort(na_count[na_count!=0], decreasing = T)) #Market cap (and thus size) have three stocks with many missing values. May need to disregard them
}
#Market cap stocks with many NAs
na_count_market_cap <- apply(market_cap, 2, function(x){sum(is.na(x))})
stocks_to_drop <- gsub(".Market_Cap", "", names(na_count_market_cap)[na_count_market_cap > 50])