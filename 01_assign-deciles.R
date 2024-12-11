######################
### ASSIGN DECILES ###
######################

## Code description: 
### The Philippine Statistics Authority provides income distributions on the Province/Highly-Urbanised City level, categorised by income deciles.
### To assign these incomes spatially, we first need to assign income deciles to each province/HUC (because decile 1 in province A is not the same as decile 1 in province B)
### We use a global poverty map comprising spatially-distributed relative wealth indices (RWI) [from Chi et al., 2022] to spatially distribute income.
### RWI were converted to rasters and clipped to each province/HUC using QGIS


library(raster)
library(dplyr)

# Read file paths
RWI_path <- "replace with your own filepath to RWI rasters for each province/HUC"
POP_path <- "replace with your own filepath to population rasters for each province/HUC" # we used Worldpop data

# Read individual raster file paths
RWI_filenames <- list.files(path = RWI_path, pattern = '.tif$', full.names = TRUE) # relative wealth index raster file paths
POP_filenames <- list.files(path = POP_path, pattern = '.tif$', full.names = TRUE) # population raster file paths

# Check if rasters are same resolution and extent 
# compareRaster(raster(RWI_filenames[[2]]), raster(POP_filenames[[2]]))

# Extract province names
province_names <- tools::file_path_sans_ext(basename(RWI_filenames))

# Path to save output
output_path <- "replace with your own file path where you want to save your work"


# Loop through each province
for (i in 1:length(RWI_filenames)){
  start_time <- Sys.time()
  
  cat("Processing Province:", province_names[i], "\n")
  
  # Read RWI and POP raster
  rwi <- raster(RWI_filenames[[i]])
  pop <- raster(POP_filenames[[i]])
 

  # Convert to points
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
  
  # Define no. of people in each decile (Each decile should have an equal number of people)
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

