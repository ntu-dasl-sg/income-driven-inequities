##############################
### FUNCTIONS FOR ANALYSES ###
##############################

# See README.md for information

# This file contains functions required for the calculation of asset and well-being losses.


############################
#### 1. DAMAGE FRACTION ####
############################

# To calculate damage fraction, we need the following functions
## First we need to read the damage-depth function and interpolate between values
open_damage_function <- function(df_file){ # opens the damage function file and interpolates between values
  df <- read.csv(file = df_file)
  df_interp <- approx(unlist(df[1]), unlist(df[2]), seq(0, 10, by = 0.01)) # df[1]= water depth (wd), #df[2]= damage fraction (df)
  return(df_interp$y) # returns vector of damage fractions
}

## Apply damage function to flood maps (rasters), used in damage_fraction_map()
apply_damage_function <- function(wd){ # wd = water depth; note that rasters generated from Flow-Tub have water depths in cm.
  #index = round(wd*100,0)+1 # for water depth maps in m (which usually is the case), need to multiply wd by 100
  index <- round(wd, 0) + 1 
  return(df[index])
}


## To produce damage fraction map
damage_fraction_map <- function(water_depth_map){ # raster map
  water_depth_map[water_depth_map < 0] = 0 # removes negative wd values
  water_depth_map[water_depth_map > 500] = 500 # if water depth > 5m (500 cm), assume completely flooded 
  water_depth_map[is.infinite(water_depth_map)] = 0 # removes infinite values
  
  damage_fraction <- calc(water_depth_map, apply_damage_function) # applies damage function to water depth map
  return(damage_fraction)
}

###################################
#### 2. EXPECTED ANNUAL DAMAGE ####
###################################

# Used to calculate the average annual losses (applies to both asset and well-being losses)
expected_annual_damage <- function(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000) {
  
  EAD10000 <- 0.0001 * RP10000
  EAD2000 <- 0.5 * (RP10000 - RP2000) * (0.0005 - 0.0001) + (0.0005 - 0.0001) * RP2000
  EAD1000 <- 0.5 * (RP2000 - RP1000) * (0.001 - 0.0005) + (0.001 - 0.0005) * RP1000
  EAD500 <- 0.5 * (RP1000 - RP500) * (0.002 - 0.001) + (0.002 - 0.001) * RP500
  EAD250 <- 0.5 * (RP500 - RP250) * (0.004 - 0.002) + (0.004 - 0.002) * RP250
  EAD100 <- 0.5 * (RP250 - RP100) * (0.01 - 0.004) + (0.01 - 0.004) * RP100
  EAD50 <- 0.5 * (RP100 - RP50) * (0.02 - 0.01) + (0.02 - 0.01) * RP50
  EAD25 <- 0.5 * (RP50 - RP25) * (0.04 - 0.02) + (0.04 - 0.02) * RP25
  EAD10 <- 0.5 * (RP25 - RP10) * (0.1 - 0.04) + (0.1 - 0.04) * RP10
  EAD5 <- 0.5 * (RP10 - RP5) * (0.2 - 0.1) + (0.2 - 0.1) * RP5
  EAD2 <- 0.5 * (RP5 - RP2) * (0.5 - 0.2) + (0.5 - 0.2) * RP2
  EAD1 <- 0.5 * (RP2 - RP1) * (1 - 0.5) + (1 - 0.5) * RP1
  
  # damages = c(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP1000,RP2000,RP10000)
  # probs = c(1,0.5,0.2,0.1,0.04,0.02,0.01,0.004,0.002,0.001,0.0005,0.0001,0)
  return(EAD10000 + EAD2000 + EAD1000 + EAD500 + EAD250 + EAD100 + EAD50 + EAD25 + EAD10 + EAD5 + EAD2 + EAD1)
  # return(AUC(probs, damages))
}

##########################################
#### 3. ASSET & WELL-BEING LOSS MODEL ####
##########################################

##### 3.1 CONSTANTS #####
t=0:20 # time, 20 year period - change this according to the time period over which to calculate losses.
avg_prod_cap = 0.23 # We use 0.23 based on previous well-being loss studies on the Philippines, change accordingly.
elasticity = 1.5 # Based on previous studies in the Philippines. 
discount_rate = 0.10 # We use a discount rate of 10% to highlight the fact that future costs and benefits are less important than present ones; characteristic of developing nations. Rates are usually lower in developed countries. 
mean_consumption = 312.8738 # This was calculated based on the initial level of consumption (consumption0) of the entire Philippines (See 05_calc_mean_consumption)


##### 3.2 UTILITY FUNCTION #####
# Base function for well-being loss model
utilityFun <- function(consumption, elasticity) {
  utility <- ((((consumption)^(1 - elasticity)) / (1 - elasticity))) + 4 # add constant to prevent -ve utility numbers
  return(utility)
}


##### 3.3 WELL-BEING LOSSES #####

wlFun <- function(labour_income, other_income, avg_prod_cap, damage_fraction, total_savings, recovery_rate, t, elasticity, discount_rate, mean_consumption){
  
  ###### 3.3.1 PRE-DISASTER INCOME, CONSUMPTION & UTILITY #####
  market_value = 3.089 * labour_income + 0.492  # estimated based on 20-year mortgage repayment period, 8.5% fixed 20-year interest rate 
  replacement_cost = market_value * 0.9  # Replacement cost is typically lower than market value
  
  # Non-monetary benefit generated from having a house to live in
  housing_income <- replacement_cost * avg_prod_cap
  
  # Pre-disaster income
  income0 <- labour_income + housing_income + other_income
  
  # Pre-disaster consumption
  mortgage <- labour_income * 0.10  # Average household spends 10% on mortgage
  consumption0 <- income0 - mortgage
  
  # Pre-disaster utility
  utility0 <- utilityFun(consumption0, elasticity)
  
  ###### 3.3.2 POST-DISASTER INCOME LOSSES, CONSUMPTION LOSSES #####
 
  # Estimated annual income losses
  ## Note: We conducted a sensitivity analysis on the these proportions and found that changing the percentage loss affected the magnitude of losses but the relative differences remained roughly the same (See supp info)
  labour_income_losses <- c(rep(0, length(t)))
  labour_income_losses[1] <-  0.8 * damage_fraction * labour_income # we assume hh lose a large portion of their labour income in the year of disaster, proportional to damage fraction
  labour_income_losses[2] <- 0.3 * damage_fraction * labour_income # in the subsequent year, they lose less
  
  # Get total household income and income losses at each time step (t)
  alternative_housing_cost <- c() # Cost of finding alternative housing
  income_losses_t <- c() # Income losses at time, t
  income_t <- c() # Income at time, t
  
  for(i in 1:length(t)){
    alternative_housing_cost[i] <- avg_prod_cap * replacement_cost * exp(-recovery_rate * t[i])
    income_losses_t[i] <- labour_income_losses[i] + alternative_housing_cost[i]
    income_t[i] <- income0 - income_losses_t[i]
  }
  
  # Get consumption losses before the use of savings
  reconstruction_cost_t <- c() # In this model, this is equivalent to the asset losses
  disc_asset_losses_t <- c() # We discount these losses over time
  consumption_losses_t <- c() 
  
  for(i in 1:length(t)){
    reconstruction_cost_t[i] <- recovery_rate * damage_fraction * replacement_cost * exp(-recovery_rate * t[i]) # Also Asset Losses
    disc_asset_losses_t[i] <- reconstruction_cost_t[i] * exp(-discount_rate*t[i]) # Discounted asset losses 
    consumption_losses_t[i] <- income_losses_t[i] + reconstruction_cost_t[i] # Consumption losses over time
  }
  
  # Total discounted asset losses
  total_asset_losses <- sum(disc_asset_losses_t) 
  #return(total_asset_losses) # Use this value for the total asset losses.

  # Total consumption losses before considering savings (not discounted because of the next step)
  total_consumption_losses <- sum(consumption_losses_t)

  # Consumption Losses after considering savings
  ## Get available savings
  savings_available <- rep(0, length(t)) # initialise empty vector to save results
  savings_available[1] <- total_savings
  
  for(i in 1:length(t)){
    savings_available[i+1] <- max(savings_available[i] - consumption_losses_t[i], 0) # find savings available at each time step; if negative, replace w/ 0 as savings cannot be negative. 
  }
  savings_available <- savings_available[-length(savings_available)] # remove last element because it's beyond the 20 year time step (due to prev step)
  
  ## Compute consumption losses considering savings and discount rate
  new_consumption_losses_t <- c() # consumption loss after taking into account savings
  disc_consumption_losses_t <- c() # discounted consumption losses after taking into account savings
  savings_offset_t <- savings_available - consumption_losses_t
  
  for (i in 1:length(t)){
    new_consumption_losses_t[i] <- ifelse(savings_offset_t[i] < 0 & !is.na(savings_offset_t[i]), -1*savings_offset_t[i], 0)
    # If savings available < consumption losses, then new closs = -savings offset 
    # but if savings available >= 0, then new consumption losses is 0 because there is sufficient savings to cover the losses.
    disc_consumption_losses_t[i] <- new_consumption_losses_t[i] * exp(-discount_rate * t[i]) #apply discount
  }
  
  ## savings used up
  savings_used <- ifelse(total_savings <= total_consumption_losses, # we calculate this because a drawing on savings reduces well-being
                         total_savings,
                         total_consumption_losses)
  
  # Total discounted consumption losses w/ savings
  total_disc_consumption_losses <- sum(disc_consumption_losses_t) + savings_used 
  
  # Consumption at time, t
  consumption_t <- consumption0 - new_consumption_losses_t # need to use the non-discounted one because we are going to discount utility loss later
  
  # Utility losses at time, t
  utility_t <- utilityFun(consumption_t, elasticity)
  disc_utility_losses_t <- (utility0 - utility_t) * exp(-discount_rate*t) # discounted utility losses at time t
  
  # Total discounted utility losses
  total_utility_losses <- sum(disc_utility_losses_t) + savings_used * consumption0^(-elasticity)
  
  # Total well-being (welfare) losses (utility losses converted to an 'equivalent consumption' in monetary terms)
  ## Also the well-being loss with respect to the household with the mean consumption 
  welfare_losses <- total_utility_losses/(mean_consumption^(-elasticity)) # equivalent consumption loss of the average household is the well-being loss
  
  return(welfare_losses)
}


