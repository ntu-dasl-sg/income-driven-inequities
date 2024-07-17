#### Compute Expected Annual Damages ####
library(raster)

#need to read functions.R first
source("D:/Jeanette/02_Code/00_functions.R")

# read in building raster tiles 
# we need to remove N17E121 from building.rasters because it's landlocked (moved to 'Landlocked' folder)
building.list <- list.files(path = "D:/Jeanette/00_Data/Buildings/Tiles", 
                            pattern = ".tif$", 
                            full.names = T)

building.rasters <- lapply(building.list, raster)

## Get tilenames
tilenames <- tools::file_path_sans_ext(basename(building.list))
# tilenames <- tilenames[-2] # remove N04E120 because it's empty 
tilenames

path = "D:/Jeanette/03_Results/diff_vuln/RESULTS_RP/"
year = "2020" # change to 2020 or 2050
loss = "AL" # AL or WL

# Running: WL20 {Done: AL50, AL50, WL50}

for (i in 1:length(tilenames)){
# Get tile 
tile <- tilenames[i]
tile
# Get asset loss rasters
loss_rasters <- lapply(list.files(path = paste0(path, year,"/", loss), 
                                pattern = tile,
                                full.names = TRUE),
                     raster)

buildings <- building.rasters[[i]]
buildings[is.na(buildings)] <- 0 # change all NAs to 0
buildings <- resample(buildings, loss_rasters[[1]], method = "ngb")
plot(buildings)

ras_stack <- stack(loss_rasters)
ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
  r <- ras_stack[[i]]
  r[is.na(r)] <- 0
  return(r)
}))

# Get AAL
ead <- overlay(ras_stack, fun=expected_annual_damage)

# Get AAL accounting for number of buildings
ead_w_buildings <- ead*buildings
ead_w_buildings[ead_w_buildings==0] <- NA # change all cells that are 0 to NA 

plot(ead_w_buildings)
# Save results
save.path <- "D:/Jeanette/03_Results/diff_vuln/RESULTS_EAD/2020/"
writeRaster(ead_w_buildings, filename = paste0(save.path, loss, "/",loss,"_", tile, "_", year,".tif"), 
            format = "GTiff", 
            overwrite=TRUE)

end <- Sys.time()
cat(tile, "completed", format(end), "\n")

}



loss = "WL" # AL or WL

# Running: WL20 {Done: AL50, AL50, WL50}

for (i in 1:length(tilenames)){
  start <- Sys.time()
  
  # Get tile 
  tile <- tilenames[i]
  tile
  # Get asset loss rasters
  loss_rasters <- lapply(list.files(path = paste0(path, year,"/", loss), 
                                    pattern = tile,
                                    full.names = TRUE),
                         raster)
  
  buildings <- building.rasters[[i]]
  buildings[is.na(buildings)] <- 0 # change all NAs to 0
  buildings <- resample(buildings, loss_rasters[[1]], method = "ngb")
  
  ras_stack <- stack(loss_rasters)
  ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
    r <- ras_stack[[i]]
    r[is.na(r)] <- 0
    return(r)
  }))
  
  # Get AAL
  ead <- overlay(ras_stack, fun=expected_annual_damage)
  
  # Get AAL accounting for number of buildings
  ead_w_buildings <- ead*buildings
  ead_w_buildings[ead_w_buildings==0] <- NA # change all cells that are 0 to NA 
  
  plot(ead_w_buildings)
  # Save results
  save.path <- "D:/Jeanette/03_Results/diff_vuln/RESULTS_EAD/2020/"
  writeRaster(ead_w_buildings, filename = paste0(save.path, loss, "/",loss,"_", tile, "_", year,".tif"), 
              format = "GTiff", 
              overwrite=TRUE)
  
  end <- Sys.time()
  cat(tile, "completed", format(end), "\n")
  
}




# for (i in 1:length(tilenames)){
#   start <- Sys.time()
#   # GET TILE
#   tile <- tilenames[i]
#   
#   # GET ASSET LOSS RASTER FOR 12 RP
#   AL.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2050/AL",  # CHANGE YEAR ACCORDINGLY
#                         pattern = tile,
#                         full.names = T)
#   
#   AL.rasters <- lapply(AL.list, raster) # list of 12 RP
# 
#   
#    # GET WELFARE LOSS RASTER FOR 12 RP
#   WL.list <- list.files(path = "D:/Jeanette/03_Results/rasters/Y2050/WL", # CHANGE YEAR ACCORDINGLY
#                         pattern = tile, 
#                         full.names = T)
#   WL.rasters <- lapply(WL.list, raster) # list of 12 RP
#   
#   # GET BUILDING RASTER
#   building_rast <- building.rasters[[i]]
#   building_rast[is.na(building_rast)] <- 0 # change all NA to 0 
#   
#   # resample building raster to loss rasters, which follow the flood maps
#   building_rast <- resample(building_rast, AL.rasters[[1]], method = "bilinear") # any one of the 12
#   
#   
#   #### 1. Asset losses ####
#   # reorder loss rasters
#   RP1 <- AL.rasters[[1]]*building_rast
#   RP10 <- AL.rasters[[2]]*building_rast
#   RP100 <-AL.rasters[[3]]*building_rast
#   RP1000 <- AL.rasters[[4]]*building_rast  
#   RP10000 <- AL.rasters[[5]]*building_rast  
#   RP2 <- AL.rasters[[6]]*building_rast  
#   RP2000 <- AL.rasters[[7]]*building_rast 
#   RP25 <- AL.rasters[[8]]*building_rast  
#   RP250 <- AL.rasters[[9]]*building_rast  
#   RP5 <- AL.rasters[[10]]*building_rast  
#   RP50 <- AL.rasters[[11]]*building_rast  
#   RP500 <- AL.rasters[[12]]*building_rast  
#   
#   # Create raster stack to apply EAD function later 
#   ras_stack <- stack(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000)
#   ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
#     r <- ras_stack[[i]]
#     r[is.na(r)] <- 0
#     return(r)
#   }))
#   
#   # Compute average annual asset losses
#   asset_loss <- overlay(ras_stack, fun = expected_annual_damage) # returns total losses for all buildings
#   asset_loss[asset_loss==0] <-NA # remove NAs
#   plot(asset_loss)
#   
#   
#   #### 2.  Well-being losses ####
#   # reorder loss rasters
#   RP1 <- WL.rasters[[1]]*building_rast
#   RP10 <- WL.rasters[[2]]*building_rast
#   RP100 <-WL.rasters[[3]]*building_rast
#   RP1000 <- WL.rasters[[4]]*building_rast  
#   RP10000 <- WL.rasters[[5]]*building_rast  
#   RP2 <- WL.rasters[[6]]*building_rast  
#   RP2000 <- WL.rasters[[7]]*building_rast 
#   RP25 <- WL.rasters[[8]]*building_rast  
#   RP250 <- WL.rasters[[9]]*building_rast  
#   RP5 <- WL.rasters[[10]]*building_rast  
#   RP50 <- WL.rasters[[11]]*building_rast  
#   RP500 <- WL.rasters[[12]]*building_rast  
#   
#   # Create raster stack to apply EAD function later 
#   ras_stack <- stack(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000)
#   ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
#     r <- ras_stack[[i]]
#     r[is.na(r)] <- 0
#     return(r)
#   }))
#   
#   # Compute average annual welfare losses
#   welfare_loss <- overlay(ras_stack, fun = expected_annual_damage) # returns total losses for all buildings
#   welfare_loss[welfare_loss==0] <-NA # remove NAs
#   plot(welfare_loss)
#   
#   # save rasters
#   AL.path = "D:/Jeanette/03_Results/rasters/Y2050/AL/EAD/AL_"
#   WL.path = "D:/Jeanette/03_Results/rasters/Y2050/WL/EAD/AL_"
#   year <- "2050"
#   writeRaster(asset_loss, filename = paste0(AL.path, tile, "_",year, ".tif"), format = "GTiff", overwrite = T)
#   writeRaster(welfare_loss, filename = paste0(WL.path, tile, "_",year, ".tif"), format = "GTiff", overwrite = T)
#   
#   
#   end <- Sys.time()
#   duration <- round(difftime(end, start, units = "secs"),2)
#   cat(tile, "completed in", duration, "secs. \n")
# }
# 
# 
# 
# 
