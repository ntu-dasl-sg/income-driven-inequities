# Merge rasters to form the whole of Philippines
## takes time to run

library(raster)
#### NEW ASSET LOSSES ####

AL20_new <- list.files(path = "D:/Jeanette/03_Results/new_asset_loss/Y2020",
                       pattern = ".tif$",
                       full.names = T)
AL50_new <- list.files(path = "D:/Jeanette/03_Results/new_asset_loss/Y2050",
                       pattern = ".tif$",
                       full.names = T)

AL20_new_rast <- lapply(AL20_new, raster)
AL50_new_rast <- lapply(AL50_new, raster)

AL20_PH_new <- do.call(merge, AL20_new_rast)
AL50_PH_new <- do.call(merge, AL50_new_rast)

writeRaster(AL20_PH_new, filename = "D:/Jeanette/03_Results/rasters/AL_2020_PH_new.tif", format = "GTiff", overwrite = T)
writeRaster(AL50_PH_new, filename = "D:/Jeanette/03_Results/rasters/AL_2050_PH_new.tif", format = "GTiff", overwrite = T)

#### EAD ####
AL20.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2020/AL/tiles/EAD",
                        pattern = ".tif$",
                        full.names = T)

WL20.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2020/WL/tiles/EAD",
                        pattern = ".tif$",
                        full.names = T)


AL50.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2050/AL/tiles/EAD",
                        pattern = ".tif$",
                        full.names = T)


WL50.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2050/WL/tiles/EAD",
                        pattern = ".tif$",
                        full.names = T)

AL20_rasters <- lapply(AL20.list,raster)
WL20_rasters <- lapply(WL20.list,raster)
AL50_rasters <- lapply(AL50.list,raster)
WL50_rasters <- lapply(WL50.list,raster)

AL20_PH <- do.call(merge, AL20_rasters)
WL20_PH <- do.call(merge, WL20_rasters)
AL50_PH <- do.call(merge, AL50_rasters)
WL50_PH <- do.call(merge, WL50_rasters)


writeRaster(AL20_PH, filename = "D:/Jeanette/03_Results/rasters/AL_2020_PH.tif", format = "GTiff", overwrite = T)
writeRaster(WL20_PH, filename = "D:/Jeanette/03_Results/rasters/WL_2020_PH.tif", format = "GTiff", overwrite = T)
writeRaster(AL50_PH, filename = "D:/Jeanette/03_Results/rasters/AL_2050_PH.tif", format = "GTiff", overwrite = T)
writeRaster(WL50_PH, filename = "D:/Jeanette/03_Results/rasters/WL_2050_PH.tif", format = "GTiff", overwrite = T)

#### FIES ####

# Read in file paths
HH.list <- list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/HH income/Province",
                          pattern = ".tif",
                          full.names = T)

labour.list <- list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/Labour income/Province",
                          pattern = ".tif",
                          full.names = T)

other.list <- list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/Other income/Province",
                         pattern = ".tif",
                         full.names = T)

savings.list <- list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/Total savings/Province",
                           pattern = ".tif",
                           full.names = T)
# Load rasters
hh_rasters <- lapply(HH.list, raster)
labour_rasters <- lapply(labour.list, raster)
other_rasters <- lapply(other.list, raster)
savings_rasters <- lapply(savings.list, raster)

# Merge rasters in parallel
hh_income_PH <- do.call(merge, hh_rasters)
labour_income_PH <- do.call(merge, labour_rasters)
other_income_PH <- do.call(merge, other_rasters)
total_savings_PH <- do.call(merge, savings_rasters)

plot(hh_income_PH)
cellStats(total_savings_PH, "min")

# Save rasters
writeRaster(hh_income_PH, filename = "D:/Jeanette/00_Data/FIES 2021/rasters/hh_income_PH.tif", format = "GTiff", overwrite = T)
writeRaster(labour_income_PH, filename = "D:/Jeanette/00_Data/FIES 2021/rasters/labour_income_PH.tif", format = "GTiff", overwrite = T)
writeRaster(other_income_PH, filename = "D:/Jeanette/00_Data/FIES 2021/rasters/other_income_PH.tif", format = "GTiff", overwrite = T)
writeRaster(total_savings_PH, filename = "D:/Jeanette/00_Data/FIES 2021/rasters/total_savings_PH.tif", format = "GTiff", overwrite = T)
#
# cellStats(total_savings_PH, "min")
# total_savings_PH[total_savings_PH <0] <- 0 # some negative values (typos in csv)
# writeRaster(total_savings_PH, filename = "D:/Jeanette/00_Data/FIES 2021/rasters/total_savings_PH.tif", format= "GTiff", overwrite = T)
#
