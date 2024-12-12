######################
### ASSIGN INCOMES ###
######################

# Code Description:
## After assigning income deciles in 01_assign-deciles.R, we now match these deciles to actual incomes/savings values
## Note that these values were pre-calculated based on income and expenditure data and the percentage of each income source from the Philippine Statistics Authority
## The well-being loss model requires the following inputs: labour income, other income and savings. The process of assigning each of these values is the same.

###################
#### 1. SET UP ####
###################

library(raster)
library(readr)
library(plyr)


# Load income and savings data (Source: Philippine Statistics Authority, Family Income & Expenditure Survey 2021)
## csv files of labour income, other income, savings distributions (by decile) for each province or HUC
FIES.paths <- list.files(path = "change this to your file path where csvs for each province are stored", # change to province/municipality (HUC)
                         pattern = ".csv$",
                         full.names = T)
FIES.data <- lapply(FIES.paths, read_csv)


# Load income decile rasters
decile.paths <- list.files(path = "file paths to income decile rasters",# change to province/municipality
                           full.names = TRUE)
decile.paths <- decile.paths[grep("\\.tif$", decile.paths, ignore.case = TRUE)]

decile.rasters <- lapply(decile.paths, raster)

# Extract province/HUC names
names <- tools::file_path_sans_ext(basename(decile.paths)) 
names


####################################
#### 2. ASSIGN INCOME & SAVINGS ####
####################################

for(i in 1:length(decile.rasters)){
  start_time <- Sys.time()
  
  #  Get decile raster for that province
  decile_raster <- decile.rasters[[i]]
  
  #  Get income & savings data for that province
  FIES_data <- FIES.data[[i]]
  FIES_data <- FIES_data[,2] # number corresponds to the column (change according to your own column): 2=hh income, 6=labour income, 7=other income, 5=total savings
  FIES_data$income_decile <- c(1:10)
  
  # Match and replace deciles with values in csv  
  FIES_values <- FIES_data$avg_hh_income[match(values(decile_raster), FIES_data$income_decile)]
  FIES_values[is.na(FIES_values)] <- 0 # remove NAs
  
  # Assign values to raster
  new_raster <- decile_raster # use decile raster as template
  new_raster[] <- FIES_values # replace decile values with income values
  new_raster[new_raster == 0] <- NA # remove 0s
  plot(new_raster)
  # Write raster to folder
  saved.path <- "replace with path where you want to save your rasters" #  change to prov / mun
  writeRaster(new_raster, filename = paste0(saved.path, names[i], ".tif"), format = "GTiff", overwrite =T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time, 3)
  
  cat(paste(names[[i]], "completed in", duration, "secs.", "\n"))
}

## Repeat this to assign savings etc.