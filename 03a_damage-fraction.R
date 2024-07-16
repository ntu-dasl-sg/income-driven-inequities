##########################
#### DAMAGE FRACTIONS ####
##########################
# READ: Creates damage fraction maps based on a single continent-level vulnerability curve from Huizinga et al. (2017) 

library(raster)
library(tidyverse)
library(janitor)
library(readr)

setwd("D:/Jeanette") # change to your own working directory
source("02_Code/00_functions.R") # get functions

## We do this by return period to reduce computational time.

#### RP 1 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP1.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 10 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP10.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 100 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP100.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 1000 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP1000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 10000 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP10000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 2 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP2.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 2000 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP2000.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 25 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP25.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 250 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP250.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 5 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP5.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 50 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP50.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}

#### RP 500 ####
# Read flood maps 
filelist <- list.files(path = "00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                       pattern = "RP500.tif",
                       full.names = T)

flood_rasters <- lapply(filelist, raster)


# Read damage function and interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv")

# Make damage fraction maps 
df_list <- lapply(flood_rasters, damage_fraction_map)

# save rasters
folder_path <- "00_Data/Damage Fraction/2050/tiles/" # define folder path to save rasters
tilenames <- basename(filelist) # get tile names without folder path but with extension

for(i in seq_along(df_list)){
  df_rast <- df_list[[i]]
  filename <- paste0(folder_path, tilenames[[i]])
  
  writeRaster(df_rast, filename=filename, format = "GTiff")
  
  cat(filename, "completed.\n")
}


