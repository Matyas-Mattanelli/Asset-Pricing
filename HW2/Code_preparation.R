library(xts)
library(readr)
library(lubridate)
library(quantmod)
library(moments)

########################
### Data preparation ###
########################

#Loading the data
yahoo_data <- readRDS("list_yahoo.RData") #Adjusted close prices and volumes
market_cap <- readRDS("market_cap_data.RData") #Market capitalization
fffactors <- read_csv("F-F_Research_Data_Factors.CSV", skip = 2)

#Extracting the Adjusted Close Price
daily_adj_close <- lapply(yahoo_data, "[", ,6)

#Remove unnecessary objects from the environment to preserve memory
rm(yahoo_data)

#Calculating daily returns
daily_returns <- lapply(daily_adj_close, dailyReturn)
daily_return_names <- gsub(".Adjusted", ".DailyReturns", unlist(lapply(daily_adj_close, names))) #New names
for (i in 1:length(daily_returns)) { #Renaming the columns for clarity
  names(daily_returns[[i]]) <- daily_return_names[i]
}

#Calculating monthly returns
monthly_returns <- lapply(daily_adj_close, monthlyReturn)
monthly_return_names <- gsub(".Adjusted", ".MonthlyReturns", unlist(lapply(daily_adj_close, names))) #New names
for (i in 1:length(monthly_returns)) { #Renaming the columns for clarity
  names(monthly_returns[[i]]) <- monthly_return_names[i]
}

#Calculate Skewness (it was assigned to our group as the 3rd sorting variable)
calc_skew <- function(xts_object) { #Function to calculate skewness for each row of a provided xts object
  result <- xts(rep(NA, nrow(xts_object)), order.by = index(xts_object))
  names(result) <- gsub("MonthlyReturns", "Skewness", names(xts_object))
  ind <- which(monthly_return_names == names(xts_object)) #Get the rank in the list
  for (i in 1:nrow(xts_object)) { #Loop through the rows of the object
    end_date <- index(xts_object)[i] #The last day that the daily returns will be used for computations
    start_date <- end_date %m-% months(12) #The start date is 12 months back as required
    df_to_use <- daily_returns[[ind]][paste0(start_date, "/", end_date)] #Extracting the subset we will use for the computation
    if (length(df_to_use)==0){ #If the data is not available, we cannot calculate the skewness
      result[i] <- NA
    } else {
      result[i] <- skewness(df_to_use) 
    }
  }
  return(result)
}
skewness_data <- lapply(monthly_returns, calc_skew) #Applying the function
saveRDS(skewness_data, file = "skewness_data.RData") #Saving skewness for later 
