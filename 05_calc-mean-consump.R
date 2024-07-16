#############################
### CALC MEAN CONSUMPTION ###
#################################
### FINAL VERSION, APR 2024   ###
#################################

# Part of the well-being loss model requires transforming utility losses into the equivalent consumption losses of the average household
# This requires the consumption of the average household, which we calculate here
# Note that this mean is for the Philippines. If the analysis is done on a smaller geographical area, the mean consumption should be calculated for that area


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

# read building raster tiles [30m]
buildings_list <- list.files(path = "D:/Jeanette/00_Data/Buildings/Tiles/",
                             pattern = ".tif",
                             full.names = T)

buildings_rasters <- lapply(buildings_list, raster) #  these are same res and ext and flood maps, so we can use this as template


# read in labour and other income
labour_income_PH <- raster("D:/Jeanette/Test_Region_IES/FIES rasters/labour_income_PH.tif")
other_income_PH <- raster("D:/Jeanette/Test_Region_IES/FIES rasters/other_income_PH.tif")


tilenames <- tools::file_path_sans_ext(basename(buildings_list))
tilenames

total_consumption <- c()
num_buildings_list <- c()


for (i in seq_along(tilenames)){
  start <- Sys.time()

  building_rast <- buildings_rasters[[i]]
  # building_rast[is.na(building_rast)] <- 0
  
  # Get labour income and other income
  labour_income <- crop(labour_income_PH, building_rast)
  other_income <- crop(other_income_PH, building_rast)
  
  # resample 
  labour_income <- resample(labour_income, building_rast, method = "ngb")
  other_income <- resample(other_income, building_rast,method = "ngb")  

  
  # compute consumption0
  consumption0 <- predisasterFun(labour_income, other_income, avg_prod_cap, elasticity)
  
  # get total consumption of all buildings in each tile
  tot_consumption0 <- consumption0 * building_rast # because each cell has diff no. of buildings

  # get sum of all consumption in each tile 
  tot_consumption <- cellStats(tot_consumption0, "sum")
  
  # save to list 
  total_consumption[i] <- tot_consumption
  
  # compute number of buildings in each tile
  num_buildings_list[i] <- cellStats(building_rast, "sum") 
  
  end <- Sys.time()
  duration <- round(difftime(end,start, units="secs"),2)
  cat(tilenames[i], "completed in", duration, "secs \n")
}

# get total consumption for PH
consumption0_PH <- sum(total_consumption)
consumption0_PH
# get total num of buildings
num_buildings_PH <- sum(num_buildings_list)
num_buildings_PH
# get mean consumption of PH
mean_consumption_PH <- consumption0_PH/num_buildings_PH

print(round(mean_consumption_PH,2))

##### 3.2 MANILA #####
i = 67
building_rast <- buildings_rasters[[i]]
# Get labour income and other income
labour_income <- crop(labour_income_PH, building_rast)
other_income <- crop(other_income_PH, building_rast)

# resample 
labour_income <- resample(labour_income, building_rast, method = "ngb")
other_income <- resample(other_income, building_rast,method = "ngb")  


# compute consumption0
consumption0 <- predisasterFun(labour_income, other_income, avg_prod_cap, elasticity)

# get total consumption of all buildings in each tile
tot_consumption0 <- consumption0 * building_rast # because each cell has diff no. of buildings

# get sum of all consumption in each tile 
tot_consumption <- cellStats(tot_consumption0, "sum")

# get total num of buildings
num_buildings_M <- cellStats(building_rast, "sum")

# get mean consumption for manila bay
mean_consumption_M <- tot_consumption/num_buildings_M

cat("Mean consumption PH:", mean_consumption_PH,"\n",
    "Mean consumption M:", mean_consumption_M, "\n")

# Results using regional income distributions:
# Mean consumption PH: 310.2425 
# Mean consumption Manila Bay (N14E120): 416.0527 

# Results using province/HUC income distributions:
# CAA: 24 Apr 2024 [Used in study]
# PH: 312.8738
# Manila Bay (N14E120): 400.0725


