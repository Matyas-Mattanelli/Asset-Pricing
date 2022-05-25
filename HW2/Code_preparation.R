library(xts)
library(readr)
library(lubridate)
library(quantmod)
library(moments)
library(sandwich)
library(lmtest)

########################
### Data preparation ###
########################

### Loading the data ###

yahoo_data <- readRDS("Raw data/list_yahoo.RData") #Adjusted close prices and volumes
market_cap <- readRDS("Raw data/market_cap_data.RData") #Market capitalization
fffactors_monthly <- read_csv("Raw data/F-F_Research_Data_Factors.CSV", skip = 2, n_max = 1149) #Need to limit the number of rows since the file contains yearly data as well
fffactors_daily <- read_csv("Raw data/F-F_Research_Data_Factors_daily.CSV", skip = 3, n_max = 25210) #Last row is some copyright => skip
momentum <- read_csv("Raw data/F-F_Momentum_Factor.CSV", skip = 12, n_max  = 1144, na = c("", "NA", -99.99, -999))
liquidity <- read.delim("Raw data/liq_data_1962_2021.txt", skip = 10)

### Defining a function to strip an xts object of lines with 0 observations (all NAs) ###

strip_empty_lines <- function(xts_object) {
  na_sums <- apply(xts_object, 1, function(x) {sum(is.na(x))})
  indic <- na_sums < ncol(xts_object)
  return(xts_object[indic])
}

#### Extracting the Adjusted Close Price ###

daily_adj_close <- lapply(yahoo_data, "[", , 6) #The unused argument is not an error, we have to skip it to specify columns

### Extracting market risk premium and the risk-free rate ###

fffactors_monthly <- as.data.frame(fffactors_monthly) #Converting from tibble to a data frame
fffactors_daily <- as.data.frame(fffactors_daily) #Converting from tibble to a data frame
colnames(fffactors_daily)[1] <- "Date"
colnames(fffactors_monthly)[1] <- "Date"
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
fffactors_monthly_xts <- as.xts(fffactors_monthly[, 2:5], order.by = fffactors_monthly$Date) #Converting monthly factors to xts
fffactors_monthly_xts <- fffactors_monthly_xts["2007/"] #Restricting the period since we do not need older data
#saveRDS(fffactors_monthly_xts, file = "Final data/fffactors_monthly.RData") #Saving for future use

### Extracting momentum ###

momentum <- as.data.frame(momentum)
colnames(momentum)[1] <- "Date"
momentum$Date <-  as.Date(paste0(as.character(momentum$Date[1]),"01"), "%Y%m%d") + months(1) - days(1) #Creating a date (end of month)
momentum_xts <- xts(momentum[, 2], order.by = momentum$Date) #Converting to xts
names(momentum_xts) <- "Momentum" #Renaming
momentum_xts <- momentum_xts["2007/"] #Restricting the period
#saveRDS(momentum_xts, "Final data/momentum.RData") #Saving for future use

### Extracting liquidity ###

colnames(liquidity)[1] <- "Date"
liquidity$Date <-  as.Date(paste0(as.character(liquidity$Date),"01"), "%Y%m%d") + months(1) - days(1) #Creating a date (end of month)
liquidity_xts <- xts(liquidity[, 2], order.by = liquidity$Date) #Converting to xts
names(liquidity_xts) <- "Liquidity" #Renaming
liquidity_xts <- liquidity_xts["2007/"] #Restricting the period
#saveRDS(liquidity_xts, "Final data/liquidity.RData") #Saving for future use

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
#saveRDS(monthly_returns_merged, file = "Final data/monthly_excess_returns.RData") #Saving for future use

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
skewness_data_merged <- strip_empty_lines(skewness_data_merged)
#saveRDS(skewness_data_merged, file = "Final data/skewness.RData") #Saving for future use

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
betas_merged <- strip_empty_lines(betas_merged)
#saveRDS(betas_merged, file = "Final data/betas.RData") #Saving for future use

### Extracting monthly market cap data ###

market_cap_monthly <- lapply(market_cap, to.monthly, OHLC = F, indexAt = "lastof") #Converting daily to monthly
market_cap_monthly_merged <- do.call(merge.xts, market_cap_monthly) #Merging
market_cap_monthly_merged <- strip_empty_lines(market_cap_monthly_merged)
#saveRDS(market_cap_monthly_merged, file = "Final data/market_cap_monthly.RData") #Saving for future use

### Calculating size ###

size <- log(market_cap_monthly_merged)
names(size) <- gsub(".Adjusted", ".Size", unlist(lapply(daily_adj_close, names))) #New names
size <- as.xts(apply(size, 2, function(x) {ifelse(is.finite(x), x, NA)}), order.by = index(size)) #Replace -Inf with NA
size <- strip_empty_lines(size) #Remove empty rows
#saveRDS(size, file = "Final data/size.RData") #Saving for future use

#####################################
### Univariate portfolio analysis ###
#####################################

#Load relevant data
rm(list = ls()) #Removes everything from the environment
monthly_returns <- readRDS("Final data/monthly_excess_returns.RData")
skewness_data <- readRDS("Final data/skewness.RData")
betas <- readRDS("Final data/betas.RData")
market_cap <- readRDS("Final data/market_cap_monthly.RData")
size <- readRDS("Final data/size.RData")
fffactors <- readRDS("Final data/fffactors_monthly.RData")
momentum <- readRDS("Final data/momentum.RData")
liquidity <- readRDS("Final data/liquidity.RData")

### Defining a function to perform a univariate sort ###

univariate_sort <- function(sort_variable, no_of_ports = 5, weighted = F) {
  sort_variable_name <- gsub("MDLZ.", "", colnames(sort_variable)[1]) #Extracting the name of the sorting variable
  final_ouptut <- as.data.frame(matrix(ncol = no_of_ports + 1, nrow = 5)) #Data frame for the final output
  colnames(final_ouptut) <- c(1:no_of_ports, paste(no_of_ports, 1, sep = "-")) #Column names for clarity
  row.names(final_ouptut) <- c(paste0("Mean of ", sort_variable_name), "Mean of one-ahead returns", "T-statistic", "Alpha", "T-statistic (alpha)")
  all_avg_sort_values <- c() #Empty vector to which we will append the results from each period (sort variable)
  all_avg_returns <- c() #Empty vector to which we will append the results from each period (returns)
  for (i in 1:nrow(sort_variable)) { #Loop through the rows (periods)
    current_period <- index(sort_variable)[i] #Store the current period
    next_period <- as.Date(as.yearmon(current_period %m+% months(1)), frac = 1) #Store the next period (for the 1-ahead returns)
    if (!next_period %in% index(monthly_returns)) { #In case we do not have 1-ahead returns, skip the period
      next
    }
    average_sort_values <- rep(NA, no_of_ports) #Place holder for the cross-sectional average values of the sort variable
    average_returns <- xts(matrix(nrow = 1, ncol = no_of_ports + 1), order.by = current_period) #Place holder for the cross-sectional average values of the 1-ahead returns
    breakpoints <- quantile(sort_variable[current_period], probs = seq(1/no_of_ports, 1 - 1/no_of_ports, 1/no_of_ports), na.rm = T) #Find the breakpoints
    for (j in 1:no_of_ports) { #Looping through the portfolios
      if (j == 1) { #For the first portfolio we have only one condition
        indic <- which(sort_variable[current_period] <= breakpoints[j]) #Find the stocks in the portfolio and save their column indices
      } else if (j == no_of_ports) { #For the last portfolio we have only one condition as well
        indic <- which(sort_variable[current_period] >= breakpoints[j -  1])
      } else { #The rest of the portfolios (two conditions)
        indic <- which(sort_variable[current_period] >= breakpoints[j - 1] & sort_variable[current_period] <= breakpoints[j]) #"=" at both inequalities to prevent empty portfolios
      }
      if (weighted == T) { #Market cap weighted average
        mc_weights <- as.numeric(market_cap[current_period, indic]) #Storing the weights
        if (length(mc_weights) == 0) { #If we do not have the market cap data, we have to skip
          next
        }
        mc_weights[is.na(mc_weights)] <- 0 #0 value for missings = they have no weight
        average_returns[, j] <- weighted.mean(as.numeric(monthly_returns[next_period, indic]), w = mc_weights, na.rm = T)
      } else { #Equally-weighted average
        average_returns[, j] <- mean(monthly_returns[next_period, indic], na.rm = T) #Calculate the average of 1-ahead returns
      }
      average_sort_values[j] <- mean(sort_variable[current_period, indic], na.rm = T) #Calculate the average of the sort variable for the given portfolio
    }
    average_returns[, no_of_ports + 1] <- average_returns[, no_of_ports] - average_returns[, 1] #The difference portfolio
    all_avg_sort_values <- rbind(all_avg_sort_values, average_sort_values) #Append the results
    all_avg_returns <- rbind(all_avg_returns, average_returns) #Append the results
  }
  final_ouptut[1, ] <- c(apply(all_avg_sort_values, 2, mean, na.rm = T) , NA) #Final average values of the sort variable
  for (i in 1:(no_of_ports + 1)) { #Loop through the portfolios and get the time series avg of returns, t-stat and alpha
    model_avg <- lm(na.omit(all_avg_returns[, i]) ~ 1) #Regress returns of each portfolio on the intercept (for weighted portfolio there may be missing values and coeftest cannot handle them => remove)
    model_avg_adj <- coeftest(model_avg, vcov = NeweyWest(model_avg, lag = 6)) #Make Newey-West adjustment of std errors
    final_ouptut[2, i] <- model_avg_adj[1, 1] #Store the intercept (=time series average)
    final_ouptut[3, i] <- model_avg_adj[1, 3] #Store the t-statistic
    #Regress the returns on the five factors
    factor_data <- na.omit(merge.xts(all_avg_returns[, i], fffactors[, 1:3], momentum, liquidity)) #Merge the data needed for the factor model (to secure the same number of obs) + remove NAs since coeftest cannot handle it
    factor_model <- lm(factor_data[, 1] ~ factor_data[, 2] + factor_data[, 3] + factor_data[, 4] + factor_data[, 5] + 
                         factor_data[, 6])
    factor_model_adj <- coeftest(factor_model, vcov = NeweyWest(factor_model, lag = 6)) #Make Newey-West adjustment
    final_ouptut[4, i] <- factor_model_adj[1, 1] #Store the intercept (alpha)
    final_ouptut[5, i] <- factor_model_adj[1, 3] #Store the t-statistic
  }
  return(final_ouptut)
}

### Univariate sort on betas ###

univariate_sort_betas <- univariate_sort(betas) #Equally weighted
debug(univariate_sort)
univariate_sort_weighted_betas <- univariate_sort(betas, weighted = T) #Market cap weighted (rbind warning should be okay, just NAs)

### Univariate sort on size ###

univariate_sort_size <- univariate_sort(size) #Equally weighted
univariate_sort_weighted_size <- univariate_sort(size, weighted = T) #Market cap weighted

### Univariate sort on skewness ###

univariate_sort_skewness <- univariate_sort(skewness_data) #Equally weighted
univariate_sort_weighted_skewness <- univariate_sort(skewness_data, weighted = T) #Market cap weighted (rbind warning should be okay, just NAs)

####################################
### Bivariate portfolio analysis ###
####################################

### Defining a function to perform a bivariate sort (independent and weighted) ### IN PROGRESS (adjusting the univariate sort function)

bivariate_sort <- function(sort_variable1, sort_variable2, no_of_ports1 = 3, no_of_ports2 = 3) {
  sort_variable1_name <- gsub("MDLZ.", "", colnames(sort_variable1)[1]) #Extracting the name of the first sorting variable
  sort_variable2_name <- gsub("MDLZ.", "", colnames(sort_variable2)[1]) #Extracting the name of the second sorting variable
  final_ouptut <- as.data.frame(matrix(ncol = no_of_ports1 + 1, nrow = 4*(no_of_ports2 + 1))) #Data frame for the final output
  colnames(final_ouptut) <- c(paste0(sort_variable1, " ", 1:no_of_ports1), paste0(sort_variable1, " ", paste0(no_of_ports1, "-", 1))) #Column names for clarity
  sort_varibale2_ports <- c(paste0(sort_variable2, " ", 1:no_of_ports2), paste0(sort_variable2, " ", paste0(no_of_ports2, "-", 1))) #Portfolios of the second sort variable stored to use for row names of the final output
  statistics <- c("Mean", "T-statistic", "Alpha", "T-statistic (alpha)") #Statistics to paste for row names
  row.names(final_ouptut) <- as.vector(t(outer(sort_varibale2_ports, statistics, FUN = "paste", sep = " - "))) #Row names for clarity
  all_avg_returns <- c() #Empty vector to which we will append the results from each period (returns)
  for (i in 1:nrow(sort_variable1)) { #Loop through the rows (periods) of the second variable (we are interested in the intersection of the periods so we can loop through just one of the indices)
    current_period <- index(sort_variable1)[i] #Store the current period
    next_period <- as.Date(as.yearmon(current_period %m+% months(1)), frac = 1) #Store the next period (for the 1-ahead returns)
    if (!next_period %in% index(monthly_returns) | !next_period %in% index(sort_variable2)) { #In case we do not have 1-ahead returns or the values for the second variable, skip the period
      next
    }
    average_returns <- xts(matrix(nrow = 1, ncol = (no_of_ports1 + 1)*(no_of_ports2 + 2)), order.by = current_period) #Place holder for the cross-sectional average values of the 1-ahead returns
    breakpoints1 <- quantile(sort_variable1[current_period], probs = c(0.3, 0.7), na.rm = T) #Find the breakpoints for the first sort variable (percentiles hardcoded for now)
    breakpoints2 <- quantile(sort_variable2[current_period], probs = c(0.3, 0.7), na.rm = T) #Find the breakpoints for the second sort variable (percentiles hardcoded for now)
    for (j in 1:(no_of_ports1 * no_of_ports2)) { #Looping through the portfolios
      if (j == 1) { #For the first portfolio we have only one condition
        indic <- which(sort_variable1[current_period] <= breakpoints1[j] & sort_variable2[current_period] <= breakpoints2[j] ) #Find the stocks in the portfolio and save their column indices
      } else if (j == no_of_ports) { #For the last portfolio we have only one condition as well ### END OF ADJUSTMENTS SO FAR ###
        indic <- which(sort_variable[current_period] >= breakpoints[j -  1])
      } else { #The rest of the portfolios (two conditions)
        indic <- which(sort_variable[current_period] >= breakpoints[j - 1] & sort_variable[current_period] <= breakpoints[j]) #"=" at both inequalities to prevent empty portfolios
      }
      if (weighted == T) { #Market cap weighted average
        mc_weights <- as.numeric(market_cap[current_period, indic]) #Storing the weights
        if (length(mc_weights) == 0) { #If we do not have the market cap data, we have to skip
          next
        }
        mc_weights[is.na(mc_weights)] <- 0 #0 value for missings = they have no weight
        average_returns[, j] <- weighted.mean(as.numeric(monthly_returns[next_period, indic]), w = mc_weights, na.rm = T)
      } else { #Equally-weighted average
        average_returns[, j] <- mean(monthly_returns[next_period, indic], na.rm = T) #Calculate the average of 1-ahead returns
      }
      average_sort_values[j] <- mean(sort_variable[current_period, indic], na.rm = T) #Calculate the average of the sort variable for the given portfolio
    }
    average_returns[, no_of_ports + 1] <- average_returns[, no_of_ports] - average_returns[, 1] #The difference portfolio
    all_avg_sort_values <- rbind(all_avg_sort_values, average_sort_values) #Append the results
    all_avg_returns <- rbind(all_avg_returns, average_returns) #Append the results
  }
  final_ouptut[1, ] <- c(apply(all_avg_sort_values, 2, mean, na.rm = T) , NA) #Final average values of the sort variable
  for (i in 1:(no_of_ports + 1)) { #Loop through the portfolios and get the time series avg of returns, t-stat and alpha
    model_avg <- lm(na.omit(all_avg_returns[, i]) ~ 1) #Regress returns of each portfolio on the intercept (for weighted portfolio there may be missing values and coeftest cannot handle them => remove)
    model_avg_adj <- coeftest(model_avg, vcov = NeweyWest(model_avg, lag = 6)) #Make Newey-West adjustment of std errors
    final_ouptut[2, i] <- model_avg_adj[1, 1] #Store the intercept (=time series average)
    final_ouptut[3, i] <- model_avg_adj[1, 3] #Store the t-statistic
    #Regress the returns on the five factors
    factor_data <- na.omit(merge.xts(all_avg_returns[, i], fffactors[, 1:3], momentum, liquidity)) #Merge the data needed for the factor model (to secure the same number of obs) + remove NAs since coeftest cannot handle it
    factor_model <- lm(factor_data[, 1] ~ factor_data[, 2] + factor_data[, 3] + factor_data[, 4] + factor_data[, 5] + 
                         factor_data[, 6])
    factor_model_adj <- coeftest(factor_model, vcov = NeweyWest(factor_model, lag = 6)) #Make Newey-West adjustment
    final_ouptut[4, i] <- factor_model_adj[1, 1] #Store the intercept (alpha)
    final_ouptut[5, i] <- factor_model_adj[1, 3] #Store the t-statistic
  }
  return(final_ouptut)
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

#Checking the merge for empty lines (may wanna add that to the function above)
x <- merge.xts(lag(monthly_returns, k = -1), skewness_data)
x_filtered <- x[rowSums(is.na(x[, 1:250])) != 250 & rowSums(is.na(x[, 251:500])) != 250 ,]
nrow(x)
nrow(x_filtered)