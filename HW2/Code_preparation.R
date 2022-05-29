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
liquidity <- read.delim("Raw data/liq_data_1962_2021.txt", skip = 10, na = c("", "NA", -99.99, -999, -99))

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
liquidity_xts <- xts(liquidity[, 4], order.by = liquidity$Date) #Converting to xts
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

########################
### Data description ###
########################

### Load relevant data ###
rm(list = ls()) #Removes everything from the environment
monthly_returns <- readRDS("Final data/monthly_excess_returns.RData")
skewness_data <- readRDS("Final data/skewness.RData")
betas <- readRDS("Final data/betas.RData")
market_cap <- readRDS("Final data/market_cap_monthly.RData")
size <- readRDS("Final data/size.RData")
fffactors <- readRDS("Final data/fffactors_monthly.RData")
momentum <- readRDS("Final data/momentum.RData")
liquidity <- readRDS("Final data/liquidity.RData")

#Restricting the period (01/01/2007 - 28/02/2022) but market cap starts at 28/02/2009 anyway and liquidity ends December 2021
market_cap <- market_cap["/2022-02-28"]
size <- size["/2022-02-28"]
fffactors <- fffactors["/2022-02-28"]
momentum <- momentum["/2022-02-28"]

### Summary statistics ###

#Defining a function to calculate the summary statistics
calc_sum_stats <- function(var_data) {
  period_stats <- matrix(ncol = 12, nrow = nrow(var_data)) #A matrix for the statistics from each period
  perc_5 <- function(x, na.rm) {return(quantile(x, probs = 0.05, na.rm = na.rm))} #Function to calculate the 5th percentile of data
  perc_25 <- function(x, na.rm) {return(quantile(x, probs = 0.25, na.rm = na.rm))} #Function to calculate the 25th percentile of data
  perc_75 <- function(x, na.rm) {return(quantile(x, probs = 0.75, na.rm = na.rm))} #Function to calculate the 75th percentile of data
  perc_95 <- function(x, na.rm) {return(quantile(x, probs = 0.95, na.rm = na.rm))} #Function to calculate the 25th percentile of data
  calc_n <- function(x, na.rm) {return(sum(!is.na(x)))} #Function to calculate the number of observations
  funcs <- list(mean, sd, skewness, kurtosis, min, perc_5, perc_25, median, perc_75, perc_95, max, calc_n) #Store the functions we will use
  for (i in 1:nrow(var_data)) { #Loop through the periods
    for (j in 1:length(funcs)) { #Loop through the functions
      period_stats[i, j] <- funcs[[j]](as.numeric(var_data[i, ]), na.rm = T) #Apply each function and store the result 
    }
  }
  time_avg <- apply(period_stats, 2, mean, na.rm = T) #Take a time series mean of each statistic
  return(round(time_avg, 2)) #Round to two decimals and return the result
}

#Applying the function
sum_stats <- as.data.frame(matrix(nrow = 5, ncol = 12)) #Place holder for the final summary statistics
colnames(sum_stats) <- c("Mean", "Sd", "Skewness", "Kurtosis", "Min", "5%", "25%", "Median", "75%", "95%", "Max", "n") #Rename the columns as the statistics
var_names <- c("Returns", "Skewness", "Beta", "Size", "Market cap") #Define a vector of variable names
row.names(sum_stats) <- var_names #Rename the row names as the variables
data_list <- list(monthly_returns, skewness_data, betas, size, market_cap) #Store the data in a list for looping
for (i in 1:5) { #Loop through the variables and apply the function
  sum_stats[i, ] <- calc_sum_stats(data_list[[i]])
}
sum_stats

### Correlations ###

#Defining a function to calculate the average correlation between two variables
calc_corr <- function(var1, var2, method = "pearson") { #Expects data for both variables and an indicator for type (default is pearson)
  var1 <- var1[index(var1) %in% index(var2)] #Filter only data available for both variables
  cross_sec_corrs <- rep(NA, nrow(var1)) #Empty vector the results of each period
  for (i in 1:nrow(var1)) { #Loop through the periods available for the first variable
    cross_sec_corrs[i] <- cor(as.numeric(var1[i]), as.numeric(var2[index(var1)[i]]), use = "pairwise.complete.obs", method = method) #Calculate the correlation for each period
  }
  return(round(mean(cross_sec_corrs, na.rm = T), 2)) #Return the rounded time series average of the correlations
}

#Applying the function
corrs <- as.data.frame(matrix(nrow = 5, ncol = 5)) #Empty matrix for the results
colnames(corrs) <- var_names #Column names
row.names(corrs) <- var_names #Row names
indices <- combn(1:5, 2) #Get the indices for each pair (unique and excluding pairing variables with themselves)
for (i in 1:ncol(indices)) { #Loop through the pairs
  corrs[indices[1, i], indices[2, i]] <- calc_corr(data_list[[indices[1, i]]], data_list[[indices[2, i]]], method = "spearman") #Spearman correlation in the upper triangle
  corrs[indices[2, i], indices[1, i]] <- calc_corr(data_list[[indices[2, i]]], data_list[[indices[1, i]]]) #Pearson correlation in the lower triangle
}
corrs

### Persistence analysis ###

#Defining a function to calculate persistence of a variable
calc_pers <- function(var_data, lags = 5) { #Expects an xts object and the number of lags how far to calculate the correlations
  final_persist <- rep(NA, lags) #A vector to store the results 
  for (i in 1:lags) { #Loop through the lags
    cross_persist <- rep(NA, nrow(var_data)) #A vector to store the results from each period
    for (j in 1 :nrow(var_data)) { #Loop through the periods
      current_period <- index(var_data)[j] #Store the current period
      lagged_period <- as.Date(as.yearmon(current_period %m-% months(i)), frac = 1) #Derive the lagged period
      if (!lagged_period %in% index(var_data)) { #If the lag is unavailable, skip
        next
      }
      cross_persist[j] <- cor(as.numeric(var_data[j]), as.numeric(var_data[lagged_period]), use = "pairwise.complete.obs") #Calculate Pearson between the variable and the appropriate lag
    }
    final_persist[i] <- mean(cross_persist, na.rm = T) #Calculate the time series mean
  }
  return(round(final_persist, 3)) #Return the rounded correlations
}

#Apply the function
lags <- 5 #Specify the required number of lags
persistence <- as.data.frame(matrix(ncol = lags, nrow = 5)) #Data frame for the results (lags in columns, vars in rows)
colnames(persistence) <- paste("Lag ", 1:lags) #Rename the columns with lag numbers
row.names(persistence) <- var_names #Rename the rows with variable names
for (i in 1:nrow(persistence)) { #Loop through the variables and calculate their persistence
  persistence[i, ] <- calc_pers(data_list[[i]], lags = lags) #Apply the function
}
persistence

#####################################
### Univariate portfolio analysis ###
#####################################

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
univariate_sort_betas
univariate_sort_weighted_betas <- univariate_sort(betas, weighted = T) #Market cap weighted (rbind warning should be okay, just NAs)
univariate_sort_weighted_betas

### Univariate sort on size ###

univariate_sort_size <- univariate_sort(size) #Equally weighted
univariate_sort_size
univariate_sort_weighted_size <- univariate_sort(size, weighted = T) #Market cap weighted
univariate_sort_weighted_size

### Univariate sort on skewness ###

univariate_sort_skewness <- univariate_sort(skewness_data) #Equally weighted
univariate_sort_skewness
univariate_sort_weighted_skewness <- univariate_sort(skewness_data, weighted = T) #Market cap weighted (rbind warning should be okay, just NAs)
univariate_sort_weighted_skewness

####################################
### Bivariate portfolio analysis ###
####################################

### Defining a function to perform a bivariate sort (independent and weighted) ### IN PROGRESS (adjusting the univariate sort function)

bivariate_sort <- function(sort_variable1, sort_variable2, percs1 = c(0.3, 0.7), percs2 = c(0.3, 0.7)) { #Accepts two sort variables data and required percentiles for the portfolios
  no_of_ports1 <- length(percs1) + 1 #Storing the number of portfolios of the first sort variable
  no_of_ports2 <- length(percs2) + 1 #Storing the number of portfolios of the second sort variable
  sort_variable1_name <- gsub("MDLZ.", "", colnames(sort_variable1)[1]) #Extracting the name of the first sorting variable
  sort_variable2_name <- gsub("MDLZ.", "", colnames(sort_variable2)[1]) #Extracting the name of the second sorting variable
  final_ouptut <- as.data.frame(matrix(ncol = no_of_ports1 + 1, nrow = 4*(no_of_ports2 + 1))) #Data frame for the final output
  colnames(final_ouptut) <- c(paste0(sort_variable1_name, " ", 1:no_of_ports1), paste0(sort_variable1_name, " ", paste0(no_of_ports1, "-", 1))) #Column names for clarity
  sort_variable2_ports <- c(paste0(sort_variable2_name, " ", 1:no_of_ports2), paste0(sort_variable2_name, " ", paste0(no_of_ports2, "-", 1))) #Portfolios of the second sort variable stored to use for row names of the final output
  statistics <- c("Mean", "T-statistic", "Alpha", "T-statistic (alpha)") #Statistics to paste for row names
  row.names(final_ouptut) <- as.vector(t(outer(sort_variable2_ports, statistics, FUN = "paste", sep = " - "))) #Row names for clarity
  all_avg_returns <- list() #Empty vector to which we will append the matrices of results from each period (returns)
  find_indics <- function(xts_object, percs) { #A function that returns boolean vectors for given quantiles of given data
    breakpoints <- quantile(xts_object, probs = percs, na.rm = T) #Find the breakpoints
    no_of_quants <- length(breakpoints) + 1 #Store the number of quantiles
    indics <- vector("list", no_of_quants) #Empty list to store the final indices
    for (i in 1:no_of_quants) { #Loop through the quantiles
      if (i == 1) { #For the first quantile we have only one condition
        indic <- xts_object <= breakpoints[i] #Find the stocks in the first quantile
      } else if (i == no_of_quants) { #For the last quantile we have only one condition as well
        indic <- xts_object >= breakpoints[i -  1]
      } else { #The rest of the quantiles (two conditions)
        indic <- xts_object >= breakpoints[i - 1] & xts_object <= breakpoints[i] #"=" at both inequalities to prevent empty quantiles (see book)
      }
      indics[[i]] <- indic
    }
    return(indics)
  }
  for (i in 1:nrow(sort_variable1)) { #Loop through the rows (periods) of the first sorting variable (we are interested in the intersection of the periods so we can loop through just one of the indices)
    current_period <- index(sort_variable1)[i] #Store the current period
    next_period <- as.Date(as.yearmon(current_period %m+% months(1)), frac = 1) #Store the next period (for the 1-ahead returns)
    if (!next_period %in% index(monthly_returns) | !current_period %in% index(sort_variable2)) { #In case we do not have 1-ahead returns or the values for the second variable, skip the period
      next
    }
    average_returns <- matrix(nrow = no_of_ports2 + 1, ncol = no_of_ports1 + 1) #Place holder for the cross-sectional average values of the 1-ahead returns
    indics1 <- find_indics(sort_variable1[current_period], percs1) #Divide the first sorting variable into quantiles (get the boolean values for the stocks)
    indics2 <- find_indics(sort_variable2[current_period], percs2) #Divide the second sorting variable into quantiles (get the boolean values for the stocks)
    indics_comb <- expand.grid(indics1, indics2) #Make all possible combinations for all quantiles of both variables
    indics_for_mat <- expand.grid(1:no_of_ports1, 1:no_of_ports2) #Indices for writing the returns in the matrix for each period
    for (j in 1:nrow(indics_comb)) { #Loop through the portfolios
      final_indics <- which(unlist(indics_comb[j, 1]) & unlist(indics_comb[j, 2])) #Find the column indices for a given portfolio
      mc_weights <- as.numeric(market_cap[current_period, final_indics]) #Storing the market cap weights
      if (length(mc_weights) == 0) { #If we do not have the market cap data, we have to skip
        next
      }
      mc_weights[is.na(mc_weights)] <- 0 #0 value for missings = they have no weight
      average_returns[indics_for_mat[j, 2], indics_for_mat[j, 1]] <- weighted.mean(as.numeric(monthly_returns[next_period, final_indics]), w = mc_weights, na.rm = T)  
    }
    for (j in 1:(ncol(average_returns)- 1)) { #Calculate the difference portfolios of the second sorting variable (last row)
      average_returns[nrow(average_returns), j] <- average_returns[nrow(average_returns) - 1, j] - average_returns[1, j] 
    }
    for (j in 1:nrow(average_returns)) { #Calculate the difference portfolios of the first sorting variable + the diff in diff portfolio (last column)
      average_returns[j, ncol(average_returns)] <- average_returns[j, ncol(average_returns) - 1] - average_returns[j, 1]
    }
    all_avg_returns[[length(all_avg_returns) + 1]] <- average_returns #Append the matrix with results for the current period
    names(all_avg_returns)[length(all_avg_returns)] <- as.character(current_period) #Name the element in the list with the period for later usage
  }
  indics_for_ports <- expand.grid(1:(no_of_ports1 + 1), 1:(no_of_ports2 + 1)) #Store the indices for each portfolio in the returns matrix for the loop
  final_returns <- rep(NA, ((no_of_ports1 + 1)*(no_of_ports2 + 1))) #A vector to store the final returns for each portfolio
  final_t_stats <- rep(NA, ((no_of_ports1 + 1)*(no_of_ports2 + 1))) #A vector to store the final t-statistics for each portfolio
  final_alphas <- rep(NA, ((no_of_ports1 + 1)*(no_of_ports2 + 1))) #A vector to store the final alphas for each portfolio
  final_t_stats_alphas <- rep(NA, ((no_of_ports1 + 1)*(no_of_ports2 + 1))) #A vector to store the final t-statistics for alphas for each portfolio
  for (i in 1:((no_of_ports1 + 1)*(no_of_ports2 + 1))) { #Loop through the portfolios and get the time series avg of returns, t-stat and alpha
    data_to_use <- xts(unlist(lapply(all_avg_returns, "[", indics_for_ports[i, 2], indics_for_ports[i, 1])), order.by = as.Date(names(all_avg_returns))) #Get the time series for each portfolio
    model_avg <- lm(na.omit(data_to_use) ~ 1) #Regress returns of each portfolio on the intercept (for weighted portfolio there may be missing values and coeftest cannot handle them => remove)
    model_avg_adj <- coeftest(model_avg, vcov = NeweyWest(model_avg, lag = 6)) #Make Newey-West adjustment of std errors
    final_returns[i] <- model_avg_adj[1, 1] #Store the intercept (=time series average)
    final_t_stats[i] <- model_avg_adj[1, 3] #Store the t-statistic
    #Regress the returns on the five factors
    factor_data <- na.omit(merge.xts(data_to_use, fffactors[, 1:3], momentum, liquidity)) #Merge the data needed for the factor model (to secure the same number of obs) + remove NAs since coeftest cannot handle it
    factor_model <- lm(factor_data[, 1] ~ factor_data[, 2] + factor_data[, 3] + factor_data[, 4] + factor_data[, 5] + 
                         factor_data[, 6])
    factor_model_adj <- coeftest(factor_model, vcov = NeweyWest(factor_model, lag = 6)) #Make Newey-West adjustment
    final_alphas[i] <- factor_model_adj[1, 1] #Store the intercept (alpha)
    final_t_stats_alphas[i] <- factor_model_adj[1, 3] #Store the t-statistic
  }
  final_result <- list(final_returns, final_t_stats, final_alphas, final_t_stats_alphas)
  for (i in 1:4) { #Loop through the list of results
    results_mat <- matrix(final_result[[i]], ncol = no_of_ports1 + 1, byrow = T) #Reshape the vector to matrix to facilitate filling in the final output
    final_ouptut[seq(i, nrow(final_ouptut), 4), ] <- results_mat #Fill the appropriate rows with results for each statistic
  }
  return(final_ouptut)
}

### Bivariate sort on size and beta ###

bivariate_sort_size_beta <- bivariate_sort(size, betas)
bivariate_sort_size_beta

### Bivariate sort on size and skewness ###

bivariate_sort_size_skewness <- bivariate_sort(size, skewness_data)
bivariate_sort_size_skewness

###############################
### Fama-Macbeth regression ###
###############################

fama_macbeth <- function(indep_var) { #Expects a list of xts objects with independent variables
  cross_sec_coefs <- c() #Empty vector to which we will append the coefficients for each period
  for (i in 1:nrow(monthly_returns)) { #Loop through the periods
    next_period <- index(monthly_returns)[i] #Store the period from which the one ahead returns come
    current_period <- as.Date(as.yearmon(next_period %m-% months(1)), frac = 1) #Store the period for the independent variables
    move_to_next_period <- F #Setting up an indicator for incomplete periods
    for (j in 1:length(indep_var)) { #Check if the current period is contained in all independent variables
      if (current_period %in% index(indep_var[[j]])) { #If it is, move to next variable
        next
      } else { #If it is not, set the indicator and break
        move_to_next_period <- T
        break
      }
    }
    if (move_to_next_period == T) { #If one of the independent variables does not contain the required period, move to next period
      next
    }
    cross_sec_data <- as.data.frame(matrix(ncol = length(indep_var) + 1, nrow = ncol(monthly_returns))) #Data holder for each period
    colnames(cross_sec_data) <- c("Returns", names(indep_var)) #Column names that will be used in the formula
    cross_sec_data[, 1] <- as.numeric(monthly_returns[next_period]) #Fill in the data for the dependent variable
    for (j in 1:length(indep_var)) { #Fill in the data for each independent variable
      cross_sec_data[, j + 1] <- as.numeric(indep_var[[j]][current_period])
    }
    form <- paste("Returns", paste(colnames(cross_sec_data)[2:ncol(cross_sec_data)], collapse = "+"), sep = "~") #Define the formula
    cross_sec_model <- lm(form, data = cross_sec_data) #Estimate the model
    cross_sec_coefs <- rbind(cross_sec_coefs, cross_sec_model$coefficients) #Append the coefficients
  }
  final_output <- as.data.frame(matrix(nrow = 2, ncol = ncol(cross_sec_coefs))) #Place holder to store the final output (time series means and t-statistics)
  colnames(final_output) <- colnames(cross_sec_coefs) #Rename the columns for clarity
  row.names(final_output) <- c("Mean", "T-statistic") #Rename the rows for clarity
  for (i in 1:ncol(cross_sec_coefs)) { #Calculate the time series mean and t-statistics for each coefficient
    time_model <- lm(cross_sec_coefs[, i] ~ 1) #Regress each time series on a constant to get the mean estimate
    time_model_adj <- coeftest(time_model, vcov = NeweyWest(time_model, lag = 4)) #Newey-West adjustment of the errors
    final_output[1, i] <- time_model_adj[1, 1] #Store the mean
    final_output[2, i] <- time_model_adj[1, 3] #Store the standard error
  }
  return(final_output)
}

### Fama-Macbeth for skewness alone ###

FM_skewness <- fama_macbeth(list(Skewness = skewness_data))
FM_skewness

### Fama-Macbeth for skewness and 5-factors ### This is some kind of bullshit, the factors are the same for all stocks => the cross-sectional regressions do not make sense

indep_var <- list(Skewness = skewness_data, MKT = fffactors[, 1], HML = fffactors[, 3], SMB = fffactors[, 2], Momentum = momentum,
                  Liquidity = liquidity)
FM_skewness_and_factors <- fama_macbeth(indep_var)
FM_skewness

### Fama-Macbeth for the three sort variables ###

indep_var <- list(Skewness = skewness_data, Size = size, Beta = betas)
FM_three_sorts <- fama_macbeth(indep_var)
FM_three_sorts

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