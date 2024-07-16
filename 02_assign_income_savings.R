######################
### ASSIGN INCOMES ###
######################

# This code is for assigning labour_income, savings, other_income to provinces and municipalities (HUCs)
# For assigning incomes (savings and other income as well) on a regional level, see assign_income.R

library(raster)
library(readr)
library(plyr)


# Load FIES data (Source: Philippine Statistics Authority, Family Income & Expenditure Survey 2021)
# These are csv files of labour income, other income, savings distributions (by decile) for each province or HUC
FIES.paths <- list.files(path = "D:/Jeanette/00_Data/FIES 2021/PROVINCE_IES", # change to province/municipality (HUC)
                         pattern = ".csv$",
                         full.names = T)
FIES.data <- lapply(FIES.paths, read_csv)


# Read income decile rasters
decile.paths <- list.files(path = "D:/Jeanette/00_Data/Income decile/rasters_province",# change to province/municipality
                           full.names = TRUE)
decile.paths <- decile.paths[grep("\\.tif$", decile.paths, ignore.case = TRUE)]

decile.rasters <- lapply(decile.paths, raster)

names <- tools::file_path_sans_ext(basename(decile.paths)) # extract municipality/province names
names


# assign income and savings

#### 1. AVG HH INCOME ####
## CHECK 25_CEBU again 
for(i in 1:length(decile.rasters)){
  start_time <- Sys.time()
  #i = 25
  #  get decile raster
  decile_raster <- decile.rasters[[i]]
  
  # get FIES data
  FIES_data <- FIES.data[[i]]
  FIES_data <- FIES_data[,2] # change accordingly: 2=hh income, 6=labour income, 7=other income, 5=total savings
  FIES_data$income_decile <- c(1:10)
  
  # match and replace deciles w FIES values 
  FIES_values <- FIES_data$avg_hh_income[match(values(decile_raster), FIES_data$income_decile)]
  FIES_values[is.na(FIES_values)] <- 0 # remove NAs
  
  # Assign values to raster
  new_raster <- decile_raster # use decile raster as template
  new_raster[] <- FIES_values # replace decile values with income values
  new_raster[new_raster == 0] <- NA # remove 0s
  plot(new_raster)
  # Write raster to folder
  saved.path <- "D:/Jeanette/00_Data/FIES 2021/rasters/HH income/Province/" #  change to prov / mun
  writeRaster(new_raster, filename = paste0(saved.path, names[i], "1.tif"), format = "GTiff", overwrite =T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time, 3)
  
  cat(paste(names[[i]], "completed in", duration, "secs.", "\n"))
}


#### 2. LABOUR INCOME ####
for(i in 1:length(decile.rasters)){
  start_time <- Sys.time()
  
  #  get decile raster
  decile_raster <- decile.rasters[[i]]
  
  # get FIES data
  FIES_data <- FIES.data[[i]]
  FIES_data <- FIES_data[,6] # change accordingly: 2=hh income, 6=labour income, 7=other income, 5=total savings
  FIES_data$income_decile <- c(1:10)
  
  # match and replace deciles w FIES values 
  FIES_values <- FIES_data$labour_income[match(values(decile_raster), FIES_data$income_decile)]
  FIES_values[is.na(FIES_values)] <- 0 # remove NAs
  
  # Assign values to raster
  new_raster <- decile_raster # use decile raster as template
  new_raster[] <- FIES_values # replace decile values with income values
  new_raster[new_raster == 0] <- NA # remove 0s
  plot(new_raster)
  # Write raster to folder
  saved.path <- "D:/Jeanette/00_Data/FIES 2021/rasters/Labour income/Province/" #  change to prov / mun
  writeRaster(new_raster, filename = paste0(saved.path, names[i], ".tif"), format = "GTiff", overwrite =T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time, 3)
  
  cat(paste(names[[i]], "completed in", duration, "secs.", "\n"))
}


#### 3. OTHER INCOME ####

for(i in 1:length(decile.rasters)){
  start_time <- Sys.time()
  
  #  get decile raster
  decile_raster <- decile.rasters[[i]]
  
  # get FIES data
  FIES_data <- FIES.data[[i]]
  FIES_data <- FIES_data[,7] # change accordingly: 2=hh income, 6=labour income, 7=other income, 5=total savings
  FIES_data$income_decile <- c(1:10)
  
  # match and replace deciles w FIES values 
  FIES_values <- FIES_data$other_income[match(values(decile_raster), FIES_data$income_decile)]
  FIES_values[is.na(FIES_values)] <- 0 # remove NAs
  
  # Assign values to raster
  new_raster <- decile_raster # use decile raster as template
  new_raster[] <- FIES_values # replace decile values with income values
  new_raster[new_raster == 0] <- NA # remove 0s
  
  # Write raster to folder
  saved.path <- "D:/Jeanette/00_Data/FIES 2021/rasters/Other income/Province/" #  change to prov / mun
  writeRaster(new_raster, filename = paste0(saved.path, names[i], ".tif"), format = "GTiff", overwrite =T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time, 3)
  
  cat(paste(names[[i]], "completed in", duration, "secs.", "\n"))
}

#### 4. TOTAL SAVINGS ####


for(i in 1:length(decile.rasters)){
  start_time <- Sys.time()
  
  #  get decile raster
  decile_raster <- decile.rasters[[i]]
  
  # get FIES data
  FIES_data <- FIES.data[[i]]
  FIES_data <- FIES_data[,5] # change accordingly: 2=hh income, 6=labour income, 7=other income, 5=total savings
  FIES_data$income_decile <- c(1:10)
  
  # match and replace deciles w FIES values 
  FIES_values <- FIES_data$total_savings[match(values(decile_raster), FIES_data$income_decile)]
  FIES_values[is.na(FIES_values)] <- 0 # remove NAs
  
  # Assign values to raster
  new_raster <- decile_raster # use decile raster as template
  new_raster[] <- FIES_values # replace decile values with income values
  new_raster[new_raster == 0] <- NA # remove 0s
  
  # Write raster to folder
  saved.path <- "D:/Jeanette/00_Data/FIES 2021/rasters/Total savings/Province/" #  change to prov / mun
  writeRaster(new_raster, filename = paste0(saved.path, names[i], ".tif"), format = "GTiff", overwrite =T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time, 3)
  
  cat(paste(names[[i]], "completed in", duration, "secs.", "\n"))
}
