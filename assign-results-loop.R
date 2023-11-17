library(readr)
library(raster)
library(data.table)
library(dplyr)
library(sp)
library(rlang)

setwd("/Users/jeancjw/Documents")
year <- "Y2020"
tile.list <- read_csv("00_Data/Coastal flood maps/Philippines/PH File list.csv")$latlong
# RP.list <- c("RP1", "RP10", "RP100", "RP1000", "RP10000", "RP2", "RP2000", "RP25", "RP250", "RP5", "RP50", "RP500")

# load regions
regions <- c("ARMM", "CAR", "NCR", "I", "II", "III", "IVA", "IVB", "IX", "V", "VI", "VII", "VIII", "X", "XI", "XII", "XIII")

# load results for each region and combine into one dataframe
combined_res <- rbindlist(lapply(regions, function(region) { # for the entire Philippines
  fread(paste0("03_Results/results_", region, ".csv"))
}))

# combined_res <- rbindlist(lapply(regions_manila, function(region) { # for manila bay only
#   fread(paste0("03_Results/results_", region, "_N14E120.csv"))
# }))

# Round damage fraction and labour income
combined_res$damage_fraction <- round(combined_res$damage_fraction, 4) 
combined_res$lab_income <- round(combined_res$lab_income, 3) # 3 dp because the values are in 000's


for (tile in seq_along(tile.list)){
  tilename <- tile.list[tile]
  
  print(paste("<<< Running tile",tilename, ">>>"))
  
  # load income rasters
  labour_income_rast <- raster(list.files(path = "00_Data/Income expenditure savings 2021/Labour income rasters", # read in lab income file paths
                                          pattern = paste0(tilename, ".*\\.tif$"), # matches tilename & ends w .tif (in case there is an aux file)
                                          full.names = TRUE))
  
  
  # load damage fraction rasters
  df.list <- lapply(list.files(path = paste0("00_Data/Damage Fraction/RCP45_", year), # list of damage fractions for all 12 return periods
                               pattern = tilename,
                               full.names = TRUE),
                    raster)
  
  # pathname <- "/Users/jeancjw/Documents/03_Results/Rasters/Manila/" # folder to save rasters for Manila Bay
  pathname <- "/Users/jeancjw/Documents/03_Results/Rasters/PH/" # folder to save rasters for PH
  
  
  RPs <- c(1:12)
  for (RP in RPs){ # loop through return periods
    return_period <- RP.list[[RP]]
    df_rast <- df.list[[RP]]
    df_rast[is.na(labour_income_rast)] <- NA # to remove pixels in the sea
    
    # Convert df and income rasters to points
    df_pts <- rasterToPoints(df_rast)
    income_pts <- rasterToPoints(labour_income_rast) # only need to load this once cos it's the same for all RPs
    
    # Check if the order of coordinates are the same in both matrices before binding 
    ## (since we're not using merge - merge gave a lot of issues w missing cells)
    print(paste("Coordinates are in the same order for", tilename,sep2,return_period, ":",  all.equal(df_pts[, c("x", "y")], income_pts[, c("x", "y")])))
    
    # Skip the return period if either df_pts or income_pts is empty
    if (nrow(df_pts) == 0 || nrow(income_pts) == 0) {
      cat("Skipping tile", tilename, "and return period", return_period, "because df_pts or income_pts is empty.\n")
      next  # Move on to the next iteration of the return period loop
    }
    
    # Combine coordinates and variables from both data frames
    df_income_join <- cbind(df_pts[, c("x", "y")], df_pts[, "layer"], income_pts[, paste0("labour_income_", tilename)])
    
    # Rename columns for clarity
    colnames(df_income_join) <- c("x", "y", "damage_fraction", "lab_income")
    
    # Change df_income_join to data frame
    df_income_join <- as.data.frame(df_income_join)
    
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
                filename = paste0(pathname, "AL", sep2, tilename, sep2, year, sep2, return_period, ".tif"),
                format = "GTiff",
                overwrite = TRUE) # results are asset losses assuming each pixel has a building
    
    welfare_loss_rast <- df_rast
    welfare_loss_rast[] <- NA # initialise empty raster w same extent and resolution as df_rast
    welfare_loss_rast <- rasterize(cbind(final_results$x, final_results$y), welfare_loss_rast, field = final_results$welfare_loss) # assign welfare loss values to raster
    writeRaster(welfare_loss_rast, 
                filename = paste0(pathname, "WL", sep2, tilename, sep2, year, sep2, return_period, ".tif"),
                format = "GTiff",
                overwrite = TRUE) # results are welfare losses assuming each pixel has one building
  }
}



