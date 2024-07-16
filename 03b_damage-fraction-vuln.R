# Get damage fraction maps based on income differentiated vulnerability curves

library(raster)
library(dplyr)

#### SET UP ####
# Set working directory
setwd("D:/Jeanette")

# Source functions(apply_damage_function, damage_fraction_map)
source("02_Code/00_functions.R")

# Read damage functions for each income group
df1 <- read.csv("00_Data/Damage-depth function/dmg_fn_low.csv")$df # low income
df2 <- read.csv("00_Data/Damage-depth function/dmg_fn_mid.csv")$df # middle income
df3 <- read.csv("00_Data/Damage-depth function/dmg_fn_high.csv")$df # high income

# Read income raster (whole PH)
income_rast <- raster("00_Data/FIES 2021/rasters/HH_income_reclass/hh_income_reclass.tif")

######################
##### LOOP START #####
######################


#### RP1 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP1.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  dur <- difftime(end,start, units=c("secs"))
  cat(tilename,"completed in", dur, "secs.\n")
}

#### RP2 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP2.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP5 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP5.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP10 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP10.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP25 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP25.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP50 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP50.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP100 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP100.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){

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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP250 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP250.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP500 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP500.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP1000 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP1000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP2000 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP2000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}

#### RP10000 ####
# Read in flood rasters (all tiles for specified RP)
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                       pattern = "RP10000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)
tilenames <- basename(filelist)

# Loop through each flood raster
for(i in 1:length(flood_rasters)){
 
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
  output_filename <- paste0("00_Data/Damage Fraction/diff_vuln/2020/", tilename, ".tif")
  writeRaster(df_rast, filename = output_filename, format = "GTiff", overwrite = TRUE)
  
  end <- Sys.time()
  cat(tilename,"completed at", format(end), ".\n")
}
####################
##### LOOP END #####
####################