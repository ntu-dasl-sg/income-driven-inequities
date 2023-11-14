#######################
### RR OPTIMISATION ###
#########################
### VERSION 4, 10 NOV ###
#########################

source("~/Documents/02_Code/functions.R")

#### 1. LIBRARIES & FILE SET UP ####
library(dplyr)
library(raster)
library(readr)
library(janitor)

## mean consumption of households in 2021, in either PH and Manila Bay area 
## mean consumption follows area of analysis
# mean_consumption <- mean_consumption_PH
mean_consumption <- mean_consumption_MANILA 

regions <- c("ARMM", "CAR", "NCR", "I", "II", "III", "IVA", "IVB", "IX", "V", "VI", "VII", "VIII", "X", "XI", "XII", "XIII")
regionID <- 3 # current region we're working on 
region <- regions[[regionID]]
decile <- 1 # current decile we're working on 

##### 1.1 INPUTS & CONSTANTS #####

###### 1.1.1 CONSTANTS ######
t = 0:20 # time period after disaster where we want to track recovery
discount_rate = 0.10 # assume 10%; developing countries usually have rates between 7-15%, higher values reflect greater emphases on immediate needs.
avg_prod_cap = 0.23 # average productivity of capital, taken from Walsh & Hallegatte (2019)'s paper on the Philippines
elasticity = 1.5 # 0.5-2.0 in literature, usually 1.2 or 1.5 in welfare loss models
building_count = 1 # one household

###### 1.1.2 READ INCOME & SAVINGS DATA ######
filenames <- list.files(path = "/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021",
                        pattern = "IES.csv", 
                        full.names = TRUE) # list file paths to income, expenditure and savings data

FIES.data <- lapply(filenames, read_csv) # read in income and savings data for all 17 regions

## this part is not really necessary ## 
# region.names <- paste0("FIES_", region) # create variable names to assign csv file w income and savings
# assign(region.names, FIES.data[[regionID]]) # assign income & savings csv to variable

get.data <- FIES.data[[regionID]]
labour_income <- get.data$labour_income[decile] # labour income, '000 pesos
other_income <- get.data$other_income[decile] # other income, '000 pesos
total_savings <- get.data$total_savings[decile] # total savings, '000 pesos

#### 2. PRE-DISASTER INCOME, CONSUMPTION, UTILITY####

predisaster_results <- unlist(predisasterFun(labour_income, other_income, avg_prod_cap, elasticity))
# returns replacement cost, income0, consumption0, utility0

for(i in names(predisaster_results)){ # assign results to variables
  assign(i, predisaster_results[[i]])
}


#### 3. POST-DISASTER WELFARE LOSSES ####

##### 3.1 OPTIMISATION FUNCTION #####
# define a function to pass optimise(), which only takes in one variable - recovery_rate
objectiveFun <- function(recovery_rate){
  welfarelossFun(labour_income, avg_prod_cap, damage_fraction, recovery_rate, t, replacement_cost, discount_rate, total_savings, mean_consumption)
}

##### 3.2 READ DAMAGE FRACTIONS #####
# df20 <- raster("/Users/jeancjw/Documents/00_Data/Damage Fraction/merged_RCP45/DF_Y2020_PH_RP10000.tif")
# df50 <- raster("/Users/jeancjw/Documents/00_Data/Damage Fraction/merged_RCP45/DF_Y2050_PH_RP10000.tif")
# # is.element(unique(df20), unique(df50)) # check if df50 contains all values in df20
# # in any case, we can just get all the unique values, just in case we might have missed something
# df.vals <- union(unique(df20), unique(df50))


##### 3.3 OPTIMISE RECOVERY RATES #####
# initialise dataframe to store results
results <- data.frame(damage_fraction = numeric(length(df.vals)),
                      optimal_rate = numeric(length(df.vals)), 
                      welfare_loss = numeric(length(df.vals)))


# set range of recovery rates
lower_bound <- 0
upper_bound <- 4 # upper bound needs to be adjusted dynamically such that consumption_t > 0 (i.e. utility losses not Inf or NaN)

# Loop over damage fractions and find optimal rate that minimises utility losses
for (i in seq_along(df.vals)){
  damage_fraction <- df.vals[i]
  
  while (TRUE) {
    # Define the objective function using the current upper_bound
    objectiveFun <- function(recovery_rate){
      welfarelossFun(labour_income, avg_prod_cap, damage_fraction, recovery_rate, t, replacement_cost, discount_rate, total_savings, mean_consumption)
    }
    
    # Perform optimization with the current upper_bound
    optimal_rate <- optimise(objectiveFun, interval = c(lower_bound, upper_bound))
    
    # Calculate welfare losses using the obtained optimal_rate
    welfare_loss <- objectiveFun(optimal_rate$minimum)
    
    # Check if the utility loss is NaN
    if (!is.nan(welfare_loss)) {
      results[i,] <- c(damage_fraction, optimal_rate$minimum, welfare_loss)
      break  # Exit the loop if a valid utility loss is found
    } else {
      upper_bound <- upper_bound - 0.01  # Adjust the upper_bound if welfare loss is NaN
      cat("Adjusting upper_bound to:", upper_bound, "\n")
    }
  }
}


# assign results to specific region and decile
varname <- paste0("optimres_", region, "_", decile) # create var name
assign(varname, cbind( region = rep(region, length(df.vals)),
                       regionID= rep(regionID, length(df.vals)),
                       rr_upperbound = rep(upper_bound, length(df.vals)),
                       results,
                       income_decile = rep(decile, length(df.vals)),
                       lab_income = rep(labour_income, length(df.vals)),
                       income0 = rep(income0, length(df.vals)),
                       consumption0 = rep(consumption0, length(df.vals)),
                       utility0 = rep(utility0, length(df.vals)),
                       savings = rep(total_savings, length(df.vals)),
                       replacement_cost = rep(replacement_cost, length(df.vals))
))



#### 4. COMBINE OPTIMISED RESULTS ####
optimRR_NCR<- rbind(optimres_NCR_1,
                    optimres_NCR_2,
                    optimres_NCR_3,
                    optimres_NCR_4,
                    optimres_NCR_5,
                    optimres_NCR_6,
                    optimres_NCR_7,
                    optimres_NCR_8,
                    optimres_NCR_9,
                    optimres_NCR_10)


#### 5. COMPUTE ASSET AND CONSUMPTION LOSSES ####

# after running rr-optimisation
AL <- c()
CL <- c()
for (i in 1:nrow(optimRR_NCR)){
  labour_income <- optimRR_NCR$lab_income[i]
  damage_fraction <- optimRR_NCR$damage_fraction[i]
  recovery_rate <- optimRR_NCR$optimal_rate[i]
  replacement_cost <- optimRR_NCR$replacement_cost[i]
  total_savings <- optimRR_NCR$savings[i]
  AL[i] <- assetlossFun(recovery_rate, damage_fraction, replacement_cost, discount_rate, t)
  CL[i] <- consumptionlossFun(labour_income, avg_prod_cap, damage_fraction, replacement_cost, recovery_rate, t, discount_rate, total_savings)
}

# join AL and CL to optimised RR and welfare loss results
optimRR_NCR <- cbind(optimRR_NCR, as.data.frame(unlist(AL)),as.data.frame(unlist(CL)))


optimRR_NCR <- optimRR_NCR %>%
  clean_names() %>%
  rename(asset_loss = unlist_al, # final losses here are all discounted (AL, WL, CL)
         consumption_loss = unlist_cl)

optimRR_NCR$welfare_loss[optimRR_NCR$asset_loss == 0] <- 0 # if damage frac is 0, asset loss is 0 and welfare loss shld be 0.

head(optimRR_NCR)

#### 6. SAVE RESULTS ####
write_csv(optimRR_NCR, file = "/Users/jeancjw/Documents/03_Results/results_NCR_N14E120.csv")
