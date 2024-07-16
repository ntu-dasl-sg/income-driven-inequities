######################
### ASSIGN DECILES ###
######################

# READ: This code is for assigning income deciles to raster cells.
## We do this by province/HUC since income distributions are given by province/HUC

library(raster)
library(dplyr)

setwd("D:/Jeanette")

#### 1. PROVINCE ####
# File paths
RWI_path <- "00_Data/Relative wealth index/rwi_province" # rasters converted from Chi et al. (2022)'s global poverty map/relative wealth indices
POP_path <- "00_Data/Population/adm2_resamp" # population rasters from Worldpop
# Read in raster files
RWI_filenames <- list.files(path = RWI_path, pattern = '.tif$', full.names = TRUE) # relative wealth index raster file paths
POP_filenames <- list.files(path = POP_path, pattern = '.tif$', full.names = TRUE) # population raster file paths

# Check if rasters are same resolution and extent 
# compareRaster(raster(RWI_filenames[[2]]), raster(POP_filenames[[2]]))

# Extract province names from RWI filenames
province_names <- tools::file_path_sans_ext(basename(RWI_filenames))

# Path to save output
output_path <- "00_Data/Income decile/rasters_province_exceptions/"

# Set the starting index 
start_index <- 1  # To resume processing mid-way (due to errors), change this index to where you want to resume 

# no. 11 Batanes is empty - no population 

# Loop through each province
for (i in start_index:length(RWI_filenames)){
  start_time <- Sys.time()
  
  cat("Processing Province:", province_names[i], "\n")
  # Read RWI and POP raster
  rwi <- raster(RWI_filenames[[i]])
  pop <- raster(POP_filenames[[i]])
 

  # Convert to points/data frame
  rwi_df <- rasterToPoints(rwi)
  colnames(rwi_df) <- c("x", "y", "rwi")
  pop_df <- rasterToPoints(pop)
  colnames(pop_df) <- c("x", "y", "pop")
  
  
  # merge rwi and pop data frames
  data <- merge(rwi_df, pop_df, by = c("x","y"))

  
   # Sort combined df by RWI values in ascending order
  data <- data[order(data$rwi), ]
  
  # Calculated cumulative sum of population 
  data$cum_pop <- cumsum(data$pop)
  
  # Calculate the total population
  total_pop <- sum(data$pop)
  
  # Define no. of ppl in each decile
  decile_pop <- total_pop / 10
  
  # Calculate the decile thresholds based on equal population count
  n_deciles <- 10
  decile_thresholds <- seq(0, 1, length.out = n_deciles + 1) * total_pop
  
  # Assign income decile values to each row based on population count
  data$income_decile <- cut(data$cum_pop, breaks = decile_thresholds, labels = FALSE, include.lowest = TRUE)
  
  # Convert the income decile data frame back to a raster
  decile_raster <- rasterFromXYZ(data[, c("x", "y", "income_decile")])
 
  
  # Write raster to file with province name
  writeRaster(decile_raster, filename = paste0(output_path, province_names[i], ".tif"), format = "GTiff", overwrite = TRUE)

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)
  
  cat(province_names[i], "completed in", duration, "mins. \n")
}

