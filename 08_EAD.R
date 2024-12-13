#########################################
#### Compute Expected Annual Damages ####
#########################################

# Code description
# After computing asset and well-being losses for each return period, we now integrate losses over all return periods to get the average annual losses/expected annual damages.

library(raster)

#need to read functions.R first
source("~/00_functions.R") # replace with your file path

#Read building rasters
building.list <- list.files(path = "~/00_Data/Buildings/Tiles", # replace with building count raster tiles
                            pattern = ".tif$", 
                            full.names = T)

building.rasters <- lapply(building.list, raster)

## Get tilenames
tilenames <- tools::file_path_sans_ext(basename(building.list))
tilenames

path = "replace with your file path"
year = "2020" # change to 2020 or 2050
loss = "AL" # AL or WL


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
save.path <- "replace with path to save results"
writeRaster(ead_w_buildings, filename = paste0(save.path, loss, "/",loss,"_", tile, "_", year,".tif"), 
            format = "GTiff", 
            overwrite=TRUE)

end <- Sys.time()
cat(tile, "completed", format(end), "\n")

}

## Repeat above for well-being losses

