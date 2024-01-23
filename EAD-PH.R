###################################
### EXPECTED ANNUAL DAMAGE - PH ###
###################################


source("~/Documents/02_Code/functions.R") # read in functions
library(raster)
library(progress)

# Function to apply expected_annual_damage
apply_ead <- function(raster_list, with_buildings, building_rast, loss_type, tilename, year, path) {
  ras_stack <- stack(raster_list)
  ras_stack <- stack(lapply(1:nlayers(ras_stack), function(i) {
    r <- ras_stack[[i]]
    r[is.na(r)] <- 0
    if (with_buildings) {
      r <- r * building_rast
    }
    return(r)
  }))
  loss <- overlay(ras_stack, fun = expected_annual_damage)
  writeRaster(loss, filename = paste0(path, sep1, loss_type, sep2, tilename, sep2, year, ".tif"), format = "GTiff", overwrite = TRUE)
}

loss_type <- "CL"
year <- "Y2050"
tile.list <- read_csv("00_Data/Coastal flood maps/Philippines/PH File list.csv")$latlong

for (tilename in tile.list) {
  cat(paste("<<< Running tile", tilename, ">>>\n"))
  
  RP.list <- c("RP1", "RP10", "RP100", "RP1000", "RP10000", "RP2", "RP2000", "RP25", "RP250", "RP5", "RP50", "RP500")
  loss.rasters <- lapply(
    list.files(
      path = "/Users/jeancjw/Documents/03_Results/Rasters/PH",
      pattern = paste0(loss_type, sep2, tilename, sep2, year),
      full.names = TRUE
    ),
    raster
  )
  
  # Skip the tile if loss.rasters is empty
  if (length(loss.rasters) == 0) {
    cat("Skipping tile", tilename, "because loss.rasters is empty.\n")
    next
  }
  
  # 1. EAD WITHOUT BUILDINGS
  path <- "/Users/jeancjw/Documents/03_Results/EAD/PH/wo_buildings"
  apply_ead(loss.rasters, FALSE, NULL, loss_type, tilename, year, path)
  
  # 2. EAD with buildings
  building_rast <- crop(raster("/Users/jeancjw/Documents/00_Data/Buildings/buildingcount_90m.tif"), loss.rasters[[1]])
  path <- "/Users/jeancjw/Documents/03_Results/EAD/PH/w_buildings"
  apply_ead(loss.rasters, TRUE, building_rast, loss_type, tilename, year, path)

  cat(paste("<<< Processed tile", tilename, ">>>\n"))
}
