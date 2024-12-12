##########################################
#### MASK MUNICIPALITIES TO PROVINCES ####
##########################################

## Code description:
# This code applies mainly to the Philippines dataset.
# Highly urbanised cities (HUCs) are municipalities which are within provinces. 
# We do this step because sometimes data is available for HUCs.
## E.g. We originally assign income/savings to Province A, but we also did so for HUC X which is within Province A. In this case, values assigned for HUC X will be replaced by HUC-level income distribution data.
## In this code 'municipality' is used interchangeably with HUC.

library(raster)
library(readr)
library(janitor)
library(tidyverse)


# Load path directories
directory <- "replace with your directory to income/savings rasters"

# Read filenames csv 
filenames <- read_csv("D:/Jeanette/00_Data/Admin boundaries/rast_ADM3/ADM3_ID.csv") # contains list of HUCs, IDs and corresponding provinces + IDs (Self-created, you can create your own system)

# Format filenames to match actual names in folders
filenames$ADM3_EN <- gsub(" ", "_", toupper(filenames$ADM3_EN)) 
filenames <- filenames %>% 
  clean_names()

filenames$adm2_en_4 <-  gsub(" ", "_", toupper(filenames$adm2_en_4)) # adm2_en_4 = province names
filenames$id <- sprintf("%02d", filenames$id) # %02d specifies that the numbers should have at least 2 digits (add leading zeros to single digits)
filenames$adm2_en_6 <- sprintf("%02d", filenames$adm2_en_6) # adm2_en_6 = province ID

# Build actual filenames
filenames$adm3_filename <- paste0(filenames$id, "_", filenames$adm3_en)
filenames$adm2_filename <- paste0(filenames$adm2_en_6, "_", filenames$adm2_en_4)

filenames$adm2_filename[11] <- "25_CEBU_new" # Cebu has multiple HUCs
filenames$adm2_filename[13] <- "25_CEBU_new_new"

filenames <- filenames %>% select(adm3_filename, adm2_filename)
filenames



# Define a custom function to replace values
replace_values <- function(x, y) {
  # Replace values from x with y where y is not NA
  result <- ifelse(!is.na(y), y, x)
  return(result)
}


# Variable categories: We need to repeat the process for the different income and savings rasters
category <- "Labour income" # hh income, labour income, other income, total savings (these depend on your folder names)
# loop through all province# loop through all municipalities
for(i in 1:length(filenames$adm3_filename)){
  start_time <- Sys.time()
  
  municipality <- raster(paste0(directory,category, "/Municipality/", filenames$adm3_filename[i], ".tif"))
  province <- raster(paste0(directory,category, "/Province/", filenames$adm2_filename[i], ".tif"))
  
  # resample municipality
  municipality_resamp <- resample(municipality, province, method = "bilinear")
  
  # Use# Use# Use overlay to apply custom function to each cell
  province_masked <- overlay(province, municipality_resamp, fun = replace_values)
  
  # write to folder
  writeRaster(province_masked, 
              filename = paste0(directory,category, "/Province/", filenames$adm2_filename[i], "_new.tif"),
              format = "GTiff",
              overwrite = T)
  
  end_time <- Sys.time()
  duration <- round(end_time-start_time,3)

  cat(paste("Processed", filenames$adm3_filename[i], " | ", filenames$adm2_filename[i],"in", duration, "secs", "\n"))
}


