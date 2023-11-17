###########################################
### EXPECTED ANNUAL DAMAGE - MANILA BAY ###
###########################################

source("~/Documents/02_Code/functions.R") # read in functions

# tile <- 78 
# tile.list <- read_csv("/Users/jeancjw/Documents/00_Data/Coastal flood maps/Philippines/PH File list.csv")$latlong
# tilename = tile.list[tile]

loss_type <- "WL"
year <- "Y2050"

loss.rasters <- lapply(list.files(path = "/Users/jeancjw/Documents/03_Results/Rasters/Manila", 
                                  pattern = paste0(loss_type, sep2, year), 
                                  full.names = TRUE), 
                       raster)

building_rast <- crop(raster("/Users/jeancjw/Documents/00_Data/Buildings/buildingcount_90m.tif"), loss.rasters[[1]])

#### 1. EAD without buildings ####

# sort out order of loss.rasters
RP1 <- loss.rasters[[1]]  
RP10 <- loss.rasters[[2]]  
RP100 <-loss.rasters[[3]]  
RP1000 <- loss.rasters[[4]]  
RP10000 <- loss.rasters[[5]]  
RP2 <- loss.rasters[[6]]  
RP2000 <- loss.rasters[[7]]  
RP25 <- loss.rasters[[8]]  
RP250 <- loss.rasters[[9]]  
RP5 <- loss.rasters[[10]]  
RP50 <- loss.rasters[[11]]  
RP500 <- loss.rasters[[12]]  

# Create raster stack to apply EAD function later 
ras_stack <- stack(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000)
ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
  r <- ras_stack[[i]]
  r[is.na(r)] <- 0
  return(r)
}))

# Compute average annual losses
loss <- overlay(ras_stack, fun = expected_annual_damage) # returns losses assuming one building per pixel

# Save results
path = "/Users/jeancjw/Documents/03_Results/EAD/Manila/wo_buildings"
writeRaster(loss, filename=paste0(path, sep1, loss_type, sep2, tilename, sep2, year,".tif"), format = "GTiff", overwrite = TRUE)


#### 2. EAD with buildings ####

# sort out order of loss.rasters
RP1 <- loss.rasters[[1]] * building_rast
RP10 <- loss.rasters[[2]] * building_rast  
RP100 <-loss.rasters[[3]] * building_rast 
RP1000 <- loss.rasters[[4]] * building_rast 
RP10000 <- loss.rasters[[5]] * building_rast  
RP2 <- loss.rasters[[6]] * building_rast  
RP2000 <- loss.rasters[[7]] * building_rast  
RP25 <- loss.rasters[[8]] * building_rast 
RP250 <- loss.rasters[[9]] * building_rast  
RP5 <- loss.rasters[[10]] * building_rast 
RP50 <- loss.rasters[[11]] * building_rast
RP500 <- loss.rasters[[12]] * building_rast

# Create raster stack to apply EAD function later 
ras_stack <- stack(RP1,RP2,RP5,RP10,RP25,RP50,RP100,RP250,RP500,RP1000,RP2000,RP10000)
ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) { # remove NA values which will cause issues when integrating diff RPs
  r <- ras_stack[[i]]
  r[is.na(r)] <- 0
  return(r)
}))

# Compute average annual losses
loss <- overlay(ras_stack, fun = expected_annual_damage) # returns total losses for all buildings

# Save results
path = "/Users/jeancjw/Documents/03_Results/EAD/Manila/w_buildings"
writeRaster(loss, filename=paste0(path, sep1, loss_type, sep2, tilename, sep2, year,".tif"), format = "GTiff", overwrite = TRUE)



