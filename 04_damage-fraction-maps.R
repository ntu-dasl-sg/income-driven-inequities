###################################
#### MAKE DAMAGE FRACTION MAPS ####
###################################


library(raster)
library(dplyr)

#### SET UP ####
# Source functions(apply_damage_function, damage_fraction_map)
source("Your file path/00_functions.R")

# Read damage-depth functions
df1 <- read.csv("your file path to low-income damage-depth function")$df  # low income
df2 <- read.csv("your file path to middle-income damage-depth function")$df # middle income
df3 <- read.csv("your file path to high-income damage-depth function")$df # high income

# Read income raster (whole PH)
income_rast <- raster("path to income raster") # note, this is for the entire Philippines. 

################### LOOP START ###################

# We make damage fraction maps for each flood return period.
# The code below assigns damage fractions by raster tiles. 

#### RP1 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "replace with path to your rasters",
                       pattern = "RP1.tif", # return period = 1 year
                       full.names = T)

flood_rasters <- lapply(filelist, raster) # read flood rasters
tilenames <- basename(filelist) # get names of tiles 

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
  start <- Sys.time()
  # Get tile name
  tilename <- tilenames[[i]]
  
  # Get flood rast
  flood_rast <- flood_rasters[[i]]
  
  # Read and process income map
  income_map <- crop(income_rast, flood_rast)
  income_map <- resample(income_rast, flood_rast, method = "ngb")
  
  # Get damage fraction map for each damage function
  df <- df1 # Low income
  dfmap1 <- damage_fraction_map(flood_rast)
  
  df <- df2 # Middle income
  dfmap2 <- damage_fraction_map(flood_rast)
  
  df <- df3 # High income
  dfmap3 <- damage_fraction_map(flood_rast)
  
  # Initialise empty raster
  df_rast <- flood_rast
  df_rast[] <- NA
  
  # Assign damage fractions based on income
  df_rast[income_map == 1] <- dfmap1[income_map == 1]
  df_rast[income_map == 2] <- dfmap2[income_map == 2]
  df_rast[income_map == 3] <- dfmap3[income_map == 3]
  
  # Save the resulting raster
  output_filename <- paste0("replace with output folder where you want to save your damage fraction map", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  dur <- difftime(end,start, units=c("secs"))
  cat(tilename,"completed in", dur, "secs.\n")
}


## Repeat for other return periods (in this case, RP2,5,10,25,50,100,250,500,1000,2000,10000) - change line 29 accordingly.