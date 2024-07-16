# For rwi, we just resample it to flood map, method = ngb, but takes at least 30min for entire PH

## Increase resolution of population data
## For population, we use disaggregate() function, then we reproject to the extent of rwi (which has been aligned to flood rasters)

library(raster)

#### 1. PROVINCE ####

# Read raster 
rwi.list <- list.files(path = "D:/Jeanette/Test/rwi_province",
                       pattern = ".tif$",
                       full.names = T)
# Get province or municipality names
names <- tools::file_path_sans_ext(basename(rwi.list))


pop.list <- list.files(path = "D:/Jeanette/Test/Population/adm2_clip_rast",
                       pattern = ".tif$",
                       full.names = T)

for (i in seq_along(names)){
  start <- Sys.time()

  # Get province/municipality name
  name <- names[i]
  
  # Get rwi
  rwi <- raster(rwi.list[[i]])
  
  # Get pop 
  pop <- raster(pop.list[[i]])
  
  # Increase resolution from 90m to 30m, then spread cell values equally in new cells
  pop30m <- disaggregate(pop, fact = 3)/9

  # Align pop raster w rwi
  pop30m <- resample(pop30m, rwi, method = "ngb")

writeRaster(pop30m, filename = paste0("D:/Jeanette/Test/Population/adm2_resamp/",name, ".tif"), format = "GTiff", overwrite =T)
  
  end <- Sys.time()
  duration <- round(difftime(end,start,units="secs"),2)
  cat(name, "completed in", duration, "secs. \n")  
}


#### 2. MUNICIPALITY ####

# Read raster 
rwi.list <- list.files(path = "D:/Jeanette/Test/rwi_municipality",
                       pattern = ".tif$",
                       full.names = T)
# Get province or municipality names
names <- tools::file_path_sans_ext(basename(rwi.list))


pop.list <- list.files(path = "D:/Jeanette/Test/Population/adm3_clip_rast",
                       pattern = ".tif$",
                       full.names = T)

for (i in seq_along(names)){
  start <- Sys.time()
  
  # Get province/municipality name
  name <- names[i]
  
  # Get rwi
  rwi <- raster(rwi.list[[i]])
  
  # Get pop 
  pop <- raster(pop.list[[i]])
  
  # Increase resolution from 90m to 30m, then spread cell values equally in new cells
  pop30m <- disaggregate(pop, fact = 3)/9
  
  # Align pop raster w rwi
  pop30m <- projectRaster(pop30m, rwi) # alternative method: resample
  
  
  writeRaster(pop30m, filename = paste0("D:/Jeanette/Test/Population/adm3_resamp/",name, ".tif"), format = "GTiff", overwrite =T)
  
  end <- Sys.time()
  duration <- round(difftime(end,start,units="secs"),2)
  cat(name, "completed in", duration, "secs. \n")  
}
