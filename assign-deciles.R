######################
### ASSIGN DECILES ###
######################

library(raster)
library(terra)
library(dplyr)

# READ RWI AND POP RASTERS
rwi <- raster("/Users/jeancjw/Documents/00_Data/Relative wealth index/rwi_NCR.tif")
pop <- crop(raster("/Users/jeancjw/Documents/00_Data/Population/phl_ppp_2020.tif"),rwi)


# resample to same resolution

