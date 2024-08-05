###################
#### FUNCTIONS ####
###################


#### 1. DAMAGE FRACTION ####
# to apply damage function to flood maps, used in another function
apply_damage_function <- function(wd){ 
  #index = round(wd*100,0)+1 # for water depth maps in m (which usually is the case), need to multiply wd by 100
  index <- round(wd, 0) + 1 # damage function wd is in m
  return(df[index])
}

# to read damage function
open_damage_function <- function(df_file){ # opens the damage function file and interpolates between values
  df <- read.csv(file = df_file)
  df_interp <- approx(unlist(df[1]), unlist(df[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
  return(df_interp$y) # returns damage fractions
}

# to produce damage fraction map
damage_fraction_map <- function(water_depth_map){ # input: raster map
  water_depth_map[water_depth_map < 0] = 0 # removes negative wd values
  water_depth_map[water_depth_map > 500] = 500 # if water depth > 5m (500 cm), assume completely flooded
                                               # Flow-tub flood maps have depths in cm
  water_depth_map[is.infinite(water_depth_map)] = 0 # removes infinite values
  
  damage_fraction <- calc(water_depth_map, apply_damage_function) # applies damage function to water depth map
  return(damage_fraction) # returns map of damage fractions
}

#### 2. EXPECTED ANNUAL DAMAGE ####
# function to compute average annual losses 
expected_annual_damage <- function(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000) {
  # add exponential integration
  
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

#### 3. UTILITY FUNCTION ####
utilityFun <- function(consumption, elasticity) { # elasticity is a constant (we use 1.5)
  utility <- ((((consumption)^(1 - elasticity)) / (1 - elasticity))) + 4 # add constant to prevent negative utility numbers
  return(utility)
}

#### 4. PRE-DISASTER CALCS FUNCTION ####
predisasterFun <- function(labour_income, other_income, avg_prod_cap, elasticity){
  market_value = 2.08*labour_income - 0.000056 # Asset(housing) capital. Derived by fitting a line through two points (lowest and highest income hh), based on assumption of an 10-year mortgage repayment period & 8% bank interest rate.
  replacement_cost = market_value * 0.9 # Replacement cost is usually lower than the market value of a home. Where land is scarce, the value of 0.9 will be smaller. Building replacement cost.  
  # Huizinga et al. (2017) provides a country-level replacement cost, but not all houses have the same replacement value. 
  
  housing_income <- replacement_cost * avg_prod_cap # non-monetary benefit generated from living in a house, estimated at 0.23 (https://github.com/walshb1/resilience_indicator_phl/blob/master/socio_economic_capacity.ipynb)
  
  # Pre-disaster income 
  income0 <- labour_income + housing_income + other_income
  
  # Pre-diaster consumption 
  mortgage <- labour_income * 0.10 # Average household spends about 10% of income on mortgage according to the FIES, 2018 housing affordability report
  consumption0 <- income0 - mortgage
  
  # Pre-disaster utility
  utility0 <- utilityFun(consumption0, elasticity)
  
  return(list(replacement_cost = replacement_cost,
         income0 = income0,
         consumption0 = consumption0,
         utility0 = utility0))
}

#### 5. ASSET LOSS FUNCTION ####

assetlossFun <- function(recovery_rate, damage_fraction, replacement_cost, discount_rate, t){
  #initialise empty vectors
  reconstruction_cost_t <- c()
  disc_asset_losses_t <- c()
  
  for(i in 1:length(t)){
    reconstruction_cost_t[i] <- recovery_rate * damage_fraction * replacement_cost * exp(-recovery_rate * t[i])
    disc_asset_losses_t[i] <- reconstruction_cost_t[i] * exp(-discount_rate*t[i])
  }
  
  # total discounted asset losses
  total_asset_losses <- sum(disc_asset_losses_t)
  return(total_asset_losses)
}

#### 6. CONSUMPTION LOSS FUNCTION ####
consumptionlossFun <- function(labour_income, avg_prod_cap, damage_fraction, replacement_cost, recovery_rate, t, discount_rate, total_savings){
  # yearly labour income losses
  labour_income_losses <- c(rep(0, length(t)))
  labour_income_losses[1] <-  0.8 * damage_fraction * labour_income # we assume hh lose a large portion of their labour income in the year of disaster, proportional to damage fraction
  labour_income_losses[2] <- 0.3 * damage_fraction * labour_income # in the subsequent year, they lose less
  
  # total income and income losses at time t
  alternative_housing_cost <- c()
  income_losses_t <- c()
  income_t <- c()
  
  for(i in 1:length(t)){
    alternative_housing_cost[i] <- avg_prod_cap * replacement_cost * exp(-recovery_rate * t[i])
    income_losses_t[i] <- labour_income_losses[i] + alternative_housing_cost[i]
    income_t[i] <- income0 - income_losses_t[i]
  }
  
  # CONSUMPTION LOSSES WITHOUT SAVINGS 
  reconstruction_cost_t <- c()
  disc_asset_losses_t <- c()
  consumption_losses_t <- c()
  
  for(i in 1:length(t)){
    reconstruction_cost_t[i] <- recovery_rate * damage_fraction * replacement_cost * exp(-recovery_rate * t[i]) # asset losses
    # disc_asset_losses_t[i] <- reconstruction_cost_t[i] * exp(-discount_rate*t[i]) # discounted asset losses
    consumption_losses_t[i] <- income_losses_t[i] + reconstruction_cost_t[i] 
  }
  
  # total consumption losses w/o savings, not discounted
  total_consumption_losses <- sum(consumption_losses_t)
  
  # CONSUMPTION LOSSES WITH SAVINGS
  ## find savings available
  savings_available <- rep(0, length(t)) # initialise empty vector to save results
  savings_available[1] <- total_savings
  
  for(i in 1:length(t)){
    savings_available[i+1] <- max(savings_available[i] - consumption_losses_t[i], 0)# find savings available at each time step; if negative, replace w/ 0 as savings cannot be negative. 
  }
  savings_available <- savings_available[-length(savings_available)] # remove last element because it's beyond the 20 year time step (due to prev step)
  
  ## compute consumption losses considering savings and discount rate
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
  savings_used <- ifelse(total_savings <= total_consumption_losses, # we calculate this because a drop in savings also has impact on wellbeing.
                         total_savings,
                         total_consumption_losses)
  
  # total discounted consumption losses w/ savings
  total_disc_consumption_losses <- sum(disc_consumption_losses_t) + savings_used 
  
  return(total_disc_consumption_losses)
}

#### 7. UTILITY LOSS FUNCTION ####
utilitylossFun <- function(labour_income, avg_prod_cap, damage_fraction, recovery_rate, t, replacement_cost, discount_rate, total_savings, mean_consumption){
  # yearly labour income losses
  labour_income_losses <- c(rep(0, length(t)))
  labour_income_losses[1] <-  0.8 * damage_fraction * labour_income # we assume hh lose a large portion of their labour income in the year of disaster, proportional to damage fraction
  labour_income_losses[2] <- 0.3 * damage_fraction * labour_income # in the subsequent year, they lose less
  
  # total income and income losses at time t
  alternative_housing_cost <- c()
  income_losses_t <- c()
  income_t <- c()
  
  for(i in 1:length(t)){
    alternative_housing_cost[i] <- avg_prod_cap * replacement_cost * exp(-recovery_rate * t[i])
    income_losses_t[i] <- labour_income_losses[i] + alternative_housing_cost[i]
    income_t[i] <- income0 - income_losses_t[i]
  }
  
  # CONSUMPTION LOSSES WITHOUT SAVINGS 
  reconstruction_cost_t <- c()
  disc_asset_losses_t <- c()
  consumption_losses_t <- c()
  
  for(i in 1:length(t)){
    reconstruction_cost_t[i] <- recovery_rate * damage_fraction * replacement_cost * exp(-recovery_rate * t[i]) # asset losses
    disc_asset_losses_t[i] <- reconstruction_cost_t[i] * exp(-discount_rate*t[i]) # discounted asset losses
    consumption_losses_t[i] <- income_losses_t[i] + reconstruction_cost_t[i] 
  }
  
  # total consumption losses w/o savings, not discounted
  total_consumption_losses <- sum(consumption_losses_t)
  # total discounted asset losses
  total_asset_losses <- sum(disc_asset_losses_t)
  
  # CONSUMPTION LOSSES WITH SAVINGS
  ## find savings available
  savings_available <- rep(0, length(t)) # initialise empty vector to save results
  savings_available[1] <- total_savings
  
  for(i in 1:length(t)){
    savings_available[i+1] <- max(savings_available[i] - consumption_losses_t[i], 0)# find savings available at each time step; if negative, replace w/ 0 as savings cannot be negative. 
  }
  savings_available <- savings_available[-length(savings_available)] # remove last element because it's beyond the 20 year time step (due to prev step)
  
  ## compute consumption losses considering savings and discount rate
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
  savings_used <- ifelse(total_savings <= total_consumption_losses, # we calculate this because a drop in savings also has impact on wellbeing.
                         total_savings,
                         total_consumption_losses)
  
  # total discounted consumption losses w/ savings
  total_disc_consumption_losses <- sum(disc_consumption_losses_t) + savings_used 
  
  # consumption at time, t
  consumption_t <- consumption0 - new_consumption_losses_t # need to use the non-discounted one because we are going to discount utility loss later
  
  # utility losses at time, t
  utility_t <- utilityFun(consumption_t, elasticity)
  disc_utility_losses_t <- (utility0 - utility_t) * exp(-discount_rate*t) # discounted utility losses at time t
  
  # total discounted utility losses
  total_utility_losses <- sum(disc_utility_losses_t) + savings_used * consumption0^(-elasticity)
  
  return(total_utility_losses) 
}

#### 8. WELFARE LOSS FUNCTION ####

welfarelossFun <- function(labour_income, avg_prod_cap, damage_fraction, recovery_rate, t, replacement_cost, discount_rate, total_savings, mean_consumption){
  # yearly labour income losses
  labour_income_losses <- c(rep(0, length(t)))
  labour_income_losses[1] <-  0.8 * damage_fraction * labour_income # we assume hh lose a large portion of their labour income in the year of disaster, proportional to damage fraction
  labour_income_losses[2] <- 0.3 * damage_fraction * labour_income # in the subsequent year, they lose less
  
  # total income and income losses at time t
  alternative_housing_cost <- c()
  income_losses_t <- c()
  income_t <- c()
  
  for(i in 1:length(t)){
    alternative_housing_cost[i] <- avg_prod_cap * replacement_cost * exp(-recovery_rate * t[i])
    income_losses_t[i] <- labour_income_losses[i] + alternative_housing_cost[i]
    income_t[i] <- income0 - income_losses_t[i]
  }
  
  # CONSUMPTION LOSSES WITHOUT SAVINGS 
  reconstruction_cost_t <- c()
  disc_asset_losses_t <- c()
  consumption_losses_t <- c()
  
  for(i in 1:length(t)){
    reconstruction_cost_t[i] <- recovery_rate * damage_fraction * replacement_cost * exp(-recovery_rate * t[i]) # asset losses
    disc_asset_losses_t[i] <- reconstruction_cost_t[i] * exp(-discount_rate*t[i]) # discounted asset losses
    consumption_losses_t[i] <- income_losses_t[i] + reconstruction_cost_t[i] 
  }
  
  # total consumption losses w/o savings, not discounted
  total_consumption_losses <- sum(consumption_losses_t)
  # total discounted asset losses
  total_asset_losses <- sum(disc_asset_losses_t)
  
  # CONSUMPTION LOSSES WITH SAVINGS
  ## find savings available
  savings_available <- rep(0, length(t)) # initialise empty vector to save results
  savings_available[1] <- total_savings
  
  for(i in 1:length(t)){
    savings_available[i+1] <- max(savings_available[i] - consumption_losses_t[i], 0)# find savings available at each time step; if negative, replace w/ 0 as savings cannot be negative. 
  }
  savings_available <- savings_available[-length(savings_available)] # remove last element because it's beyond the 20 year time step (due to prev step)
  
  ## compute consumption losses considering savings and discount rate
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
  savings_used <- ifelse(total_savings <= total_consumption_losses, # we calculate this because a drop in savings also has impact on wellbeing.
                         total_savings,
                         total_consumption_losses)
  
  # total discounted consumption losses w/ savings
  total_disc_consumption_losses <- sum(disc_consumption_losses_t) + savings_used 
  
  # consumption at time, t
  consumption_t <- consumption0 - new_consumption_losses_t # need to use the non-discounted one because we are going to discount utility loss later
  
  # utility losses at time, t
  utility_t <- utilityFun(consumption_t, elasticity)
  disc_utility_losses_t <- (utility0 - utility_t) * exp(-discount_rate*t) # discounted utility losses at time t
  
  # total discounted utility losses
  total_utility_losses <- sum(disc_utility_losses_t) + savings_used * consumption0^(-elasticity)
  
  # total welfare losses 
  ## also utility losses converted to monetary value
  ## and welfare losses wrt the household with the mean consumption 
  welfare_losses <- total_utility_losses/(mean_consumption^(-elasticity)) # equivalent consumption loss of the average household is the welfare loss
  
  return(welfare_losses)
}

