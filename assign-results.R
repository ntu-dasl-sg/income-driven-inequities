####################################
### ASSIGN RR & LOSSES TO RASTER ###
####################################

library(readr)
library(raster)
library(data.table)
library(dplyr)
library(sp)
library(rlang)

#### 1. SET UP ####
setwd("/Users/jeancjw/Documents")

year <- "Y2050"
# tile <- 78 # 1-102
tile <- 78
tile.list <- read_csv("00_Data/Coastal flood maps/Philippines/PH File list.csv")$latlong
tile.list 



tilename <- tile.list[tile]

print(paste(">>>> Running tile",tilename, "<<<<"))

# Load regions
regions <- c("ARMM", "CAR", "NCR", "I", "II", "III", "IVA", "IVB", "IX", "V", "VI", "VII", "VIII", "X", "XI", "XII", "XIII")
regions_manila <- c("NCR", "III", "IVA")

# Load admin boundaries
admin.list <- list.files(path = "00_Data/Admin boundaries/Philippines/rast_ADM01", # read admin boundary file paths
                         pattern = ".tif$",
                         full.names = TRUE)

admin.rasters <- lapply(admin.list, raster) # apply raster function to read in all the rasters in admin.list


# Load labour income rasters
income.list <- list.files(path = "00_Data/Income expenditure savings 2021/Labour income rasters", # read in lab income file paths
                          pattern = ".tif$", 
                          full.names = TRUE)

income.rasters <- lapply(income.list, raster) # read all labour incomes


##### 1.1 READ IN RESULTS FOR EACH REGION #####
# Read optimised RR csv files
# combined_res <- rbindlist(lapply(regions_manila, function(region) { # for manila bay only
#   fread(paste0("03_Results/results_", region, "_N14E120.csv"))
# }))

combined_res <- rbindlist(lapply(regions, function(region) { # for the entire Philippines
  fread(paste0("03_Results/results_", region, ".csv"))
}))

# Round damage fraction and labour income
combined_res$damage_fraction <- round(combined_res$damage_fraction, 4) 
combined_res$lab_income <- round(combined_res$lab_income, 3) # 3 dp because the values are in 000's


##### 1.2 ADMIN BOUNDARY RASTER #####
admin_boundary <- admin.rasters[[tile]]

##### 1.3 LABOUR INCOME  & DAMAGE FRAC #####
labour_income_rast <- income.rasters[[tile]]

df.list <- lapply(list.files(path = paste0("00_Data/Damage Fraction/RCP45_", year), # list of damage fractions for all 12 return periods
                             pattern = tilename,
                             full.names = TRUE),
                  raster)

#### 2. ASSIGN LOSSES ####
# pathname <- "/Users/jeancjw/Documents/03_Results/Rasters/Manila/" # folder to save rasters for Manila Bay
pathname <- "/Users/jeancjw/Documents/03_Results/Rasters/PH/" # folder to save rasters for PH

vars <- c("AL", "WL")

RPs <- c(1:12)

for (RP in RPs){ # loop through return periods
  return_period <- RP.list[[RP]]
  df_rast <- df.list[[RP]]
  df_rast[is.na(labour_income_rast)] <- NA # to remove pixels in the sea
  
  # Convert df and income rasters to points
  df_pts <- rasterToPoints(df_rast)
  income_pts <- rasterToPoints(labour_income_rast) # only need to load this once cos it's the same for all RPs
  
  # merge both income and df by coordinates
  df_income_join <- merge(df_pts, income_pts, by = c("x","y")) # merge both into one dataframe
  
  
  # Rename column headers for damage frac and lab income (dynamic, based on tilename)
  setDT(df_income_join)[, c("lab_income", "damage_fraction") := .(get(paste0("labour_income_", tilename)), layer)]
  df_income_join[, c("layer", paste0("labour_income_", tilename)) := NULL]
  
  # Apply same rounding as we did for the optimisation results 
  df_income_join$damage_fraction <- round(df_income_join$damage_fraction, 4) # 4dp
  df_income_join$lab_income <- round(df_income_join$lab_income, 3) # 3dp
  
  # join df_income_join which has spatial attributes, with combined_res - results with no spatial attributes
  final_results <-left_join(df_income_join, combined_res, by = c("damage_fraction", "lab_income"))
  final_results <- final_results[!is.na(final_results$region_id),] # remove NAs (rows/pixels that are in the sea)
  
  # Assign losses
  asset_loss_rast <- df_rast
  asset_loss_rast[] <- NA # initialise empty raster w same extent and resolution as df_rast
  asset_loss_rast <- rasterize(cbind(final_results$x, final_results$y), asset_loss_rast, field = final_results$asset_loss) # assign asset loss values to raster
  writeRaster(asset_loss_rast, 
              filename = paste0(pathname, vars[1], sep2, tilename, sep2, year, sep2, return_period, ".tif"),
              format = "GTiff",
              overwrite = TRUE) # results are asset losses assuming each pixel has a building
  
  welfare_loss_rast <- df_rast
  welfare_loss_rast[] <- NA # initialise empty raster w same extent and resolution as df_rast
  welfare_loss_rast <- rasterize(cbind(final_results$x, final_results$y), welfare_loss_rast, field = final_results$welfare_loss) # assign welfare loss values to raster
  writeRaster(welfare_loss_rast, 
              filename = paste0(pathname, vars[2], sep2, tilename, sep2, year, sep2, return_period, ".tif"),
              format = "GTiff",
              overwrite = TRUE) # results are welfare losses assuming each pixel has one building
}



