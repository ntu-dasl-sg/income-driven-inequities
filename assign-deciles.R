######################
### ASSIGN DECILES ###
######################

### This code is for assigning deciles to rasters

library(raster)

# File paths
# RWI_path <- "/Users/jeancjw/Documents/00_Data/Relative wealth index/adm2_clip_rast"
RWI_path <- "/Users/jeancjw/Documents/00_Data/Relative wealth index/adm3_clip_rast" 
# POP_path <- "/Users/jeancjw/Documents/00_Data/Population/adm2_clip_rast"
POP_path <- "/Users/jeancjw/Documents/00_Data/Population/adm3_clip_rast"
# output_path <- "/Users/jeancjw/Documents/00_Data/Income decile/Province/" 
output_path <- "/Users/jeancjw/Documents/00_Data/Income decile/Municipality/"

# List all raster files
RWI_filenames <- list.files(path = RWI_path, pattern = '.tif$', full.names = TRUE) # relative wealth index raster file paths
POP_filenames <- list.files(path = POP_path, pattern = '.tif$', full.names = TRUE) # population raster file paths

# Extract province names from RWI filenames
province_names <- tools::file_path_sans_ext(basename(RWI_filenames))

# Set the starting index to resume processing [[for errors midway]]
start_index <- 1  # Change this to the index where you want to resume processing when you meet w/ errors midway


# Loop through each province
for (i in start_index:length(RWI_filenames)) {
  cat("Processing Province:", province_names[i], "\n")
  
  # Read RWI and POP raster
  rwi <- raster(RWI_filenames[i])
  pop <- raster(POP_filenames[i])
  
  # Resample POP raster
  pop <- resample(pop, rwi, method = "bilinear")
  
  # Extract values from rwi and pop
  rwi_df <- as.data.frame(rwi, xy = TRUE) 
  colnames(rwi_df) <- c("x", "y", "rwi") # need to rename because it takes the province name
  pop_df <- as.data.frame(pop, xy = TRUE) 
  colnames(pop_df) <- c("x", "y", "pop")
  
  # Combine dfs
  data <- merge(rwi_df, pop_df, by = c("x", "y"))
  
  # Remove NA pop vals because we're not interested in areas w no people
  data <- data[!is.na(data$pop) & data$pop != 0, ]
  
  # Remove NA rwi values
  data <- data[!is.na(data$rwi), ]
  
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
}


##################################################
#### CODE BELOW IS FOR JUST ONE PROVINCE/AREA ####
##################################################

# library(raster)
# library(dplyr)
# library(sf)
# library(janitor)

# ## READ FULL RASTERS AND CONVERT TO SAME EXTENT
# rwi_PH <- raster("/Users/jeancjw/Documents/00_Data/Relative wealth index/rwi_PH.tif")
# pop_PH <- raster("/Users/jeancjw/Documents/00_Data/Population/phl_ppp_2020.tif")
# 
# pop_resamp <- resample(pop_PH, rwi_PH, method = "bilinear") # convert to same extent


# #### 1. READ RWI AND POP RASTERS
# ## RWI
# RWI_path <- "/Users/jeancjw/Documents/00_Data/Admin boundaries/Philippines/raw phl shapefiles/adm2_split_rast" # change for adm3
# RWI_filenames <- list.files(path = RWI_path, 
#                             pattern = '.tif$',
#                             full.names = T)
# RWI_list <- lapply(RWI_filenames, raster) # apply raster function to read in all raster files 
# 
# ## POPULATION
# POP_path <- "/Users/jeancjw/Documents/00_Data/Population/adm2_clip_rast"
# POP_filenames <- list.files(path = POP_path, 
#                             pattern = '.tif$', 
#                             full.names = T)
# POP_list <- lapply(POP_filenames, raster)

# #### 2. SELECT PROVINCE 
# ADM_num <- 5
# rwi <- RWI_list[[ADM_num]] # Select RWI raster from list
# pop <- POP_list[[ADM_num]] # Crop population raster to RWI raster
# 
# pop <- resample(pop, rwi)
# 
# df <- data.frame(x = xyFromCell(rwi, 1:ncell(rwi))[,1],
#                  y = xyFromCell(rwi, 1:ncell(rwi))[,2],
#                  rwi = extract(rwi, xyFromCell(rwi, 1:ncell(rwi))),
#                  pop = extract(pop, xyFromCell(pop, 1:ncell(pop))))
# 
# df <- df %>% 
#   filter(pop != 0 & !is.na(pop)) %>% #filter out cells with no people
#   filter(!is.na(rwi)) # filter out cells with no rwi
# 
# #### 3. ASSIGN DECILES
# # GET POPULATION COUNT AND THRESHOLD
# total_pop <- sum(df$pop)
# decile_pop_threshold <- total_pop/10
# 
# 
# df <- df %>% 
#   arrange(rwi) # arrange rwi in ascending order
# 
# # Calculate total population
# total_pop <- sum(df$pop)
# # Decile popoulation threshold
# pop_threshold <- total_pop/10
# 
# #cumulative pop
# df$cum_pop <- cumsum(df$pop)
# 
# # initialise vector to store income deciles
# df$income_decile <- NA
# 
# # Determine the decile breaks based on the cumulative population
# decile_breaks <- c(0, seq(pop_threshold, total_pop, by = pop_threshold))
# 
# # Assign deciles using cut function
# df$income_decile <- cut(df$cum_pop, breaks = decile_breaks, labels = FALSE)
# 
# # Ensure that deciles are within 1 to 10 range
# df$income_decile <- pmin(df$income_decile, 10)
# 
# #### 4. CONVERT BACK TO RASTER AND SAVE
# # Create an empty raster with the same extent and resolution as 'rwi'
# template_rast <- raster(rwi)
# template_rast[] <- NA
# 
# # Optionally, set the CRS of 'decile_rast' to match 'rwi'
# projection(template_rast) <- projection(rwi)
# # Convert points to raster
# 
# decile_rast <- rasterize(df[, c("x", "y")], template_rast, field = df$income_decile)
# 
# # Plot the rasterized points
# plot(decile_rast)
# 
# # Extract province name from the filename
# province_name <- tools::file_path_sans_ext(basename(RWI_filenames[ADM_num]))
# 
# # Write raster to file with province name
# writeRaster(decile_rast, filename = paste0("/Users/jeancjw/Documents/00_Data/Income decile/Province/", province_name, ".tif"), format = "GTiff", overwrite =T)
