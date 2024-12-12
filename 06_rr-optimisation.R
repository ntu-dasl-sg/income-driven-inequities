#######################
### RR OPTIMISATION ###
#########################

## Code Description:
## Recovery rates are optimised here such that well-being losses are minimised.
## This code solves for recovery rates + well-being losses, then uses recovery rates to compute asset losses

# Variables to change: filepath / FIES.data / save path at end to province or municipality + 17 (mun) / 87 (province)

source("~/00_functions.R") # replace with actual filepath to functions

# Load libraries
library(dplyr)
library(raster)
library(readr)
library(janitor)


## Mean consumption values [correct as at 24 Apr <- using this one]
### 30m resolution
# 312.8738 (000 PHP) for the Philippines
# 400.0725 (000 PHP) for N14E120
mean_consumption <- 312.8738 # This is for the entire PH
# mean_consumption <- 400.0725 # For manila N14E120 tile only

# Constants
t = 0:20 # time period after disaster where we want to track recovery
discount_rate = 0.10 # assume 10%; developing countries usually have rates between 7-15%, higher values reflect greater emphases on immediate needs.
avg_prod_cap = 0.23 # average productivity of capital, taken from Walsh & Hallegatte (2019)'s paper on the Philippines
elasticity = 1.5 # 0.5-2.0 in literature, usually 1.2 or 1.5 in welfare loss models
building_count = 1 # one household

# Read income and savings data from FIES
# filepath <- list.files(path = "replace with file paths to csvs containing income and savings distributions for province",
#                        pattern = ".csv",
#                        full.names = T)
# 
# FIES.data <- lapply(filepath, read_csv) # read in indiv csv files for provinces
# # 
filepath <- list.files(path = "replace with file paths to csvs containing income and savings distributions for HUC/municipality",
                       pattern = ".csv",
                       full.names = T)

FIES.data <- lapply(filepath, read_csv) # read in indiv csv files for municipalities



# # Read in damage fractions and save all unique values [RUN ONCE ONLY]
# df20.list <- list.files(path="replace with path to damage fraction maps (2020)",
#                                pattern = ".tif$",
#                                full.names = T)
# 
# df20_rasters <- lapply(df20.list, raster)
# 
# df50.list <- list.files(path="replace with path to damage fraction maps (2050)",
#                                pattern = ".tif$",
#                                full.names = T)
# 
# df50_rasters <- lapply(df50.list, raster)
# 
# 
# # Initialize an empty vector to store unique values
# all_unique_values <- NULL
# 
# # Iterate over each raster in df20.list
# for (i in seq_along(df20_rasters)) {
#   # Get unique values of the current raster
#   unique_values <- unique(df20_rasters[[i]])
#   # Combine unique values with the previous ones
#   all_unique_values <- union(all_unique_values, unique_values)
# }
# 
# 
# 
# all_unique_values50 <- NULL
# for (i in seq_along(df50_rasters)) {
#   # Get unique values of the current raster
#   unique_values <- unique(df50_rasters[[i]])
#   # Combine unique values with the previous ones
#   all_unique_values50 <- union(all_unique_values50, unique_values)
# }
# 
# # get all possible damage fraction values
# df.vals <- union(all_unique_values, all_unique_values50)


# compute results
province_names <- tools::file_path_sans_ext(basename(filepath)) # get province names
province_names <- gsub("_avg_IES", "", province_names) # remove "_avg_IES" from names (only for PROVINCE)
province_IDs <- c(1:17) # change this number to 17 for municipality, 87 for province

# create empty list to combine results
combined_results <- list()

# Loop through provinces
for(provinceID in seq_along(FIES.data)){
  start_time <- Sys.time() # record start time
  
  province <- province_names[[provinceID]] #  get name of province we are working on
  province_ID <- province_IDs[[provinceID]]
  
  # create empty list to store results for each decile of the current province
  province_results <- list()
  
  # Loop through deciles
  for(decile in 1:10){
    # Read in FIES data for current province's decile
    get.data <- FIES.data[[provinceID]]
    avg_hh_income <- get.data$avg_hh_income[decile] # not necessary, just for reference
    labour_income <- get.data$labour_income[decile]
    other_income <- get.data$other_income[decile]
    total_savings <- get.data$total_savings[decile]
    
    # PRE-DISASTER CALCULATIONS 
    predisaster_results <- unlist(predisasterFun(labour_income, other_income, avg_prod_cap, elasticity))
    
    # assign predisaster results to vars
    for(i in names(predisaster_results)){
      assign(i, predisaster_results[[i]])
    }
    
    # OPTIMISE RECOVERY RATES + POST-DISASTER RES
    results <- data.frame(damage_fraction = numeric(length(df.vals)),
                          optimal_rate = numeric(length(df.vals)),
                          welfare_loss = numeric(length(df.vals)))
    # Set range of recovery rates
    lower_bound <- 0 
    upper_bound <- 4
    
    # Loop over df to find optimal rate than minimises welfare losses
    for(j in seq_along(df.vals)){
      damage_fraction <- df.vals[j]
      
      while(TRUE){
        # define objective fucntion using current upper bound
        objectiveFun <- function(recovery_rate) {
          welfarelossFun(labour_income, avg_prod_cap, damage_fraction, recovery_rate, t, replacement_cost,discount_rate, total_savings, mean_consumption)
        }
        optimal_rate <- optimise(objectiveFun, interval = c(lower_bound, upper_bound))
        welfare_loss <- objectiveFun(optimal_rate$minimum)
        
        # check if welfare loss is NaN
        if (!is.nan(welfare_loss)) {
          results[j,] <- c(damage_fraction, optimal_rate$minimum, welfare_loss)
          break # Exit loop if valid welfare loss is found
        } else {
          upper_bound <- upper_bound - 0.01 # Adjust upper bound if welfare loss is NaN
          # cat("Adjusting upper bound to:", upper_bound, '\n')
        }
      }
    }
    # Store results for current decile in list
    varname <- paste0("optimres_", province, "_", decile)
    province_results[[decile]] <- cbind( province = rep(province, length(df.vals)),
                                         provinceID= rep(province_ID, length(df.vals)),
                                         rr_upperbound = rep(upper_bound, length(df.vals)),
                                         results,
                                         income_decile = rep(decile, length(df.vals)),
                                         avg_hh_income = rep(avg_hh_income, length(df.vals)),
                                         lab_income = rep(labour_income, length(df.vals)),
                                         income0 = rep(income0, length(df.vals)),
                                         consumption0 = rep(consumption0, length(df.vals)),
                                         utility0 = rep(utility0, length(df.vals)),
                                         savings = rep(total_savings, length(df.vals)),
                                         replacement_cost = rep(replacement_cost, length(df.vals))
                                         )
    # Remove the optimres_xxxxxxx dataframes from the environment
    rm(list = varname)
  }
  # Combine results for all deciles of current province
  combined_results[[provinceID]] <- do.call(rbind, province_results)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time,2)
  
  cat(province, "completed at:", format(end_time), "\n")
}

# function to compute asset and consumption losses
compute_losses <- function(df) {
  AL <- c()
  CL <- c()
  for (i in 1:nrow(df)){
    labour_income <- df$lab_income[i]
    damage_fraction <- df$damage_fraction[i]
    recovery_rate <- df$optimal_rate[i]
    replacement_cost <- df$replacement_cost[i]
    total_savings <- df$savings[i]
    AL[i] <- assetlossFun(recovery_rate, damage_fraction, replacement_cost, discount_rate, t)
    CL[i] <- consumptionlossFun(labour_income, avg_prod_cap, damage_fraction, replacement_cost, recovery_rate, t, discount_rate, total_savings)
  }
  
  # Add AL and CL as new columns to the dataframe
  df <- cbind(df, AL = unlist(AL), CL = unlist(CL))
  
  return(df)
}


# ASSET AND CONSUMPTION LOSSES

for(provinceID in seq_along(combined_results)){
  province <- province_names[[provinceID]]
  province_df <- combined_results[[provinceID]]
  
  
  # Compute losses for the current province dataframe
  province_df <- compute_losses(province_df)
  
  # Clean column names
  province_df <- province_df %>% clean_names()
  
  # Rename columns
  province_df <- province_df %>%
    dplyr::rename(asset_loss = al,
           consumption_loss = cl)
  
  # Adjust welfare loss where asset loss is 0
  province_df$welfare_loss[province_df$asset_loss == 0] <- 0
  
  # Update province dataframe in province_results
  province_results[[provinceID]] <- province_df
  
  cat(province, "completed\n")
}




# Save results

for (provinceID in seq_along(province_results)) {
  province <- province_names[[provinceID]]
  
  # Extract data for the current province
  province_data <- province_results[[provinceID]]
  
  # Define the file name with underscores instead of spaces
  path <- "replace with output path" # Repeat for province/HUC
  filename <- paste0(path,"results_", province, ".csv")
  
  # Write data to CSV
  write.csv(province_data, filename, row.names = FALSE)
}

