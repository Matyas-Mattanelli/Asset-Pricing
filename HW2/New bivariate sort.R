bivariate_sort <- function(sort_variable1, sort_variable2, percs1 = c(0.3, 0.7), percs2 = c(0.3, 0.7)) { #Accepts two sort variables data and required percentiles for the portfolios
  no_of_ports1 <- length(percs1) + 1 #Storing the number of portfolios of the first sort variable
  no_of_ports2 <- length(percs2) + 1 #Storing the number of portfolios of the second sort variable
  sort_variable1_name <- gsub("MDLZ.", "", colnames(sort_variable1)[1]) #Extracting the name of the first sorting variable
  sort_variable2_name <- gsub("MDLZ.", "", colnames(sort_variable2)[1]) #Extracting the name of the second sorting variable
  final_ouptut <- as.data.frame(matrix(ncol = no_of_ports1 + 2, nrow = 4*(no_of_ports2 + 2))) #Data frame for the final output
  colnames(final_ouptut) <- c(paste0(sort_variable1_name, " ", 1:no_of_ports1), paste(sort_variable1_name, "Avg"), paste0(sort_variable1_name, " ", paste0(no_of_ports1, "-", 1))) #Column names for clarity
  sort_variable2_ports <- c(paste0(sort_variable2_name, " ", 1:no_of_ports2), paste(sort_variable2_name, "Avg"), paste0(sort_variable2_name, " ", paste0(no_of_ports2, "-", 1))) #Portfolios of the second sort variable stored to use for row names of the final output
  statistics <- c("Mean", "t-statistic", "Alpha", "t-statistic (alpha)") #Statistics to paste for row names
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
    average_returns <- matrix(nrow = no_of_ports2 + 2, ncol = no_of_ports1 + 2) #Place holder for the cross-sectional average values of the 1-ahead returns
    indics1 <- find_indics(sort_variable1[current_period], percs1) #Divide the first sorting variable into quantiles (get the boolean values for the stocks)
    indics2 <- find_indics(sort_variable2[current_period], percs2) #Divide the second sorting variable into quantiles (get the boolean values for the stocks)
    indics_comb <- expand.grid(indics1, indics2) #Make all possible combinations for all quantiles of both variables
    indics_for_mat <- expand.grid(1:no_of_ports1, 1:no_of_ports2) #Indices for writing the returns in the matrix for each period
    for (j in 1:nrow(indics_comb)) { #Loop through the portfolios
      final_indics <- which(unlist(indics_comb[j, 1]) & unlist(indics_comb[j, 2])) #Find the column indices for a given portfolio
      if (length(final_indics) == 0) { #If there are no stocks in the portfolio, skip
        next
      }
      mc_weights <- as.numeric(market_cap[current_period, final_indics]) #Storing the market cap weights
      if (length(mc_weights) == 0) { #If we do not have the market cap data, we have to skip
        next
      }
      mc_weights[is.na(mc_weights)] <- 0 #0 value for missings = they have no weight
      average_returns[indics_for_mat[j, 2], indics_for_mat[j, 1]] <- weighted.mean(as.numeric(monthly_returns[next_period, final_indics]), w = mc_weights, na.rm = T)  
    }
    for (j in 1:(ncol(average_returns) - 2)) { #Calculate the average portfolio for the second sorting variable
      average_returns[nrow(average_returns) - 1, j] <- mean(average_returns[1:(nrow(average_returns) - 2), j], na.rm = T)
    }
    for (j in 1:(nrow(average_returns) - 2)) { #Calculate the average portfolio for the first sorting variable
      average_returns[j, ncol(average_returns) - 1] <- mean(average_returns[j, 1:(ncol(average_returns)-2)], na.rm = T)
    }
    for (j in 1:(ncol(average_returns)- 1)) { #Calculate the difference portfolios of the second sorting variable (last row)
      average_returns[nrow(average_returns), j] <- average_returns[nrow(average_returns) - 2, j] - average_returns[1, j] 
    }
    for (j in 1:nrow(average_returns)) { #Calculate the difference portfolios of the first sorting variable + the diff in diff portfolio (last column)
      average_returns[j, ncol(average_returns)] <- average_returns[j, ncol(average_returns) - 2] - average_returns[j, 1]
    }
    all_avg_returns[[length(all_avg_returns) + 1]] <- average_returns #Append the matrix with results for the current period
    names(all_avg_returns)[length(all_avg_returns)] <- as.character(current_period) #Name the element in the list with the period for later usage
  }
  indics_for_ports <- expand.grid(1:(no_of_ports1 + 2), 1:(no_of_ports2 + 2)) #Store the indices for each portfolio in the returns matrix for the loop
  final_returns <- rep(NA, ((no_of_ports1 + 2)*(no_of_ports2 + 2))) #A vector to store the final returns for each portfolio
  final_t_stats <- rep(NA, ((no_of_ports1 + 2)*(no_of_ports2 + 2))) #A vector to store the final t-statistics for each portfolio
  final_alphas <- rep(NA, ((no_of_ports1 + 2)*(no_of_ports2 + 2))) #A vector to store the final alphas for each portfolio
  final_t_stats_alphas <- rep(NA, ((no_of_ports1 + 2)*(no_of_ports2 + 2))) #A vector to store the final t-statistics for alphas for each portfolio
  for (i in 1:((no_of_ports1 + 2)*(no_of_ports2 + 2))) { #Loop through the portfolios and get the time series avg of returns, t-stat and alpha
    if (indics_for_ports[i, 1] == (no_of_ports1 + 1) & indics_for_ports[i, 2] == (no_of_ports2 + 1)) { #Skip the average of averages portfolio
      next
    }
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
    results_mat <- matrix(final_result[[i]], ncol = no_of_ports1 + 2, byrow = T) #Reshape the vector to matrix to facilitate filling in the final output
    if (i %in% c(2, 4)) { #Wrap the t-statistics in brackets for nicer output
      final_ouptut[seq(i, nrow(final_ouptut), 4), ] <- paste0("(", round(results_mat, 3), ")")  
    } else {
      final_ouptut[seq(i, nrow(final_ouptut), 4), ] <- round(results_mat, 3) #Fill the appropriate rows with results for each statistic
    }
  }
  return(final_ouptut)
}
