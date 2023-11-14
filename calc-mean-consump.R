#############################
### CALC MEAN CONSUMPTION ###
#############################
### VERSION 2, 10 NOV ###
#########################

#### LOAD LIBRARIES ####
library(dplyr)
library(raster)
library(readr)
library(ggplot2)

#### 1. CONSTANTS ####
discount_rate = 0.10 # assume 10% discount rate; developing countries usually have rates between 7-15%, where higher values reflect greater emphasis on immediate needs
avg_prod_cap = 0.23 # average productivity of capital; taken from Walsh & Hallegatte (2019)'s paper on the Philippines
elasticity = 1.5 # usually about 1.2 or 1.5 in literature
t = 0:20 # time period in years after the disaster that we're interested in

#### 2. FUNCTIONS ####
##### 2.1 UTILITY FUNCTION #####
utilityFun <- function(consumption, elasticity) {
  utility <- ((((consumption)^(1 - elasticity)) / (1 - elasticity))) + 4 # add constant to prevent -ve utility numbers
  return(utility)
}

##### 2.2 PRE-DISASTER CALCS FUNCTION #####
predisasterFun <- function(labour_income, other_income, avg_prod_cap, elasticity){
  market_value = 2.08*labour_income - 0.000056 # also housing capital. derived by fitting a line through two points (lowest and highest income hh)
  replacement_cost = market_value * 0.9 # where land is scarce, this will be smaller. Building replacement cost. Instead of using the building replacement cost from the global dmg fn, we tag the cost to the hh income through market value of the home. 
  
  housing_income <- replacement_cost * avg_prod_cap # non-monetary income generated from living in a house
  
  # Pre-disaster income 
  income0 <- labour_income + housing_income + other_income
  
  # Pre-diaster consumption 
  mortgage <- labour_income * 0.10 # Average household spends about 10% of income on mortgage according to the FIES, 2018 housing affordability report
  consumption0 <- income0 - mortgage
  
  # Pre-disaster utility
  utility0 <- utilityFun(consumption0, elasticity)
  
  return(consumption0)
}


  
  

#### 3. MEAN CONSUMPTION ####
##### 3.1 PHILIPPINES #####
labour_income <- raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/labour_income.tif")
other_income <- raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/other_income.tif")
building_rast <- raster("/Users/jeancjw/Documents/00_Data/Buildings/buildingcount_90m.tif")

# compute consumption0 
consumption0 <- predisasterFun(labour_income, other_income, avg_prod_cap, elasticity)

# get total consumption of all buildings 
tot_consumption0 <- consumption0 * building_rast # because each cell has diff no. of buildings
# get sum of all consumption 
tot_consumption <- cellStats(tot_consumption0, "sum")
# get sum of number of buildings
num_buildings <- cellStats(building_rast, "sum")
# get mean consumption of the PH
mean_consumption_PH <- tot_consumption/num_buildings
print(round(mean_consumption_PH,2))

##### 3.2 MANILA BAY, N14E120 #####
extent_mask <- raster("/Users/jeancjw/Documents/00_Data/Coastal flood maps/Philippines/RCP45/Y2050/N14E120_Y2050_RP10000.tif") # any flood map for the required tile
labour_income <- crop(raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/labour_income.tif"), extent_mask)
other_income <- crop(raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/other_income.tif"), extent_mask)
building_rast <- crop(raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/other_income.tif"), extent_mask)

# compute consumption0
consumption0 <- predisasterFun(labour_income, other_income, avg_prod_cap, elasticity)

# get total consumption of all buildings 
tot_consumption0 <- consumption0 * building_rast # because each cell has diff no. of buildings
# get sum of all consumption 
tot_consumption <- cellStats(tot_consumption0, "sum")
# get sum of number of buildings
num_buildings <- cellStats(building_rast, "sum")
# get mean consumption of the PH
mean_consumption_MANILA <- tot_consumption/num_buildings

print(round(mean_consumption_MANILA,2))


