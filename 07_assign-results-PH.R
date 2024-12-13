
##################################
### Assign results to rasters ####
##################################

# Code description: Assign calculated asset and well-being losses to rasters.

library(readr)
library(raster)
library(dplyr)

# Assign results from optimisation to raster based on damage fraction and income pairs
# Run entire script, just change year to 2020 or 2050


# read in results from RR optimisation (provinces)
filepaths <- list.files(path = "path to results for provinces (assigned in optimisation step)", 
                        pattern = ".csv$",
                        full.names = T)
results <- lapply(filepaths, read_csv)


# read in results for RR optimisation (municipalities/HUCs)
filepaths_mun <- list.files(path = "path to results for HUCs (assigned in optimisation step)", 
                        pattern = ".csv$",
                        full.names = T)
results_mun <- lapply(filepaths_mun, read_csv)

# combine both results for province and municipality
results_all <- c(results, results_mun)

# combine all results 
combined_results <- do.call(rbind, results_all) #unlists results_all and combines rows

# round values so we can match them
combined_results$damage_fraction <- round(combined_results$damage_fraction,4)
combined_results$lab_income <- round(combined_results$lab_income,3)

# compute consumption post-disaster (optional)
combined_results <- combined_results %>% 
  mutate(new_consumption = consumption0 - consumption_loss)


# define tilenames
tilenames <- tools::file_path_sans_ext(list.files(path = "~/00_Data/Damage Fraction/diff_vuln/2020/", # to get tilenames 
           pattern = "RP1.tif"))

tilenames <- gsub("_Y2020_RP1", "", tilenames)
tilenames <- gsub(".tif","", tilenames)
tilenames # check tilenames

# define return periods
RP.list <- c("RP1","RP10", "RP100", "RP1000", "RP10000", "RP2", "RP2000","RP25", "RP250", "RP5", "RP50", "RP500")


# Read labour income raster
income_PH <- raster("replace with labour income raster path for the Philippines") 


for (i in 1:length(tilenames)){ # tile 2,9 empty
    tile <- tilenames[[i]]

  df_list <- list.files(path = "replace with path to damage fraction rasters", # CHANGE YEAR
                        pattern = tile,
                        full.names = T)
  df_rasters <- lapply(df_list, raster) # list of 12 return periods
  
  income_rast <- crop(income_PH, df_rasters[[1]]) # income for that tile
  # compareRaster(income_rast,df_rasters[[1]]) # rasters not same extent, need to resample
  income_rast <- resample(income_rast, df_rasters[[1]], method = "ngb")
  
  # initialise empty list to save rasters
  AL_list <- list()
  WL_list <- list()
  
  for (j in 1:length(RP.list)){ # now we loop through each return period
    return_period <- RP.list[[j]]
    df_rast <- df_rasters[[j]] # df rast for that return period
    income_rast[df_rast == 0] <- NA # we only want areas where there is damage
    
    # convert df and income to pts
    df_pts <- as.data.frame(df_rast, xy=TRUE)
    income_pts <- as.data.frame(income_rast, xy = TRUE)
    
    colnames(df_pts) <- c("x", "y", "damage_fraction")
    colnames(income_pts) <- c("x", "y", "lab_income")
    
    df_pts <- df_pts %>% 
      filter(damage_fraction >0)
    income_pts <- income_pts %>% 
      filter(!is.na(lab_income))
    
    df_income <- merge(df_pts, income_pts, by = c("x", "y"))
    head(df_income)
    
    # rename column headers
    df_income <- df_income %>% 
      filter(!is.na(lab_income)) # remove NAs
    
    df_income$damage_fraction <- round(df_income$damage_fraction, 4)
    df_income$lab_income <- round(df_income$lab_income, 3)
    
    final_results <- left_join(df_income, combined_results, by=c("damage_fraction", "lab_income"))
    
    # Assign losses
    ## Asset losses
    asset_loss_df <-  final_results %>% 
      dplyr::select(x,y, damage_fraction, lab_income, asset_loss) %>% 
      filter(asset_loss > 0)
    
    asset_loss_rast <- df_rast 
    asset_loss_rast[] <- NA # initialise empty raster
    
    asset_loss_rast <- rasterize(cbind(asset_loss_df$x, asset_loss_df$y), asset_loss_rast, field = asset_loss_df$asset_loss)
    AL_list[[j]] <- asset_loss_rast
    
    ## Welfare losses
    welfare_loss_df <-  final_results %>% 
      dplyr::select(x,y, damage_fraction, lab_income, welfare_loss) %>% 
      filter(welfare_loss > 0)
    
    welfare_loss_rast <- df_rast 
    welfare_loss_rast[] <- NA # initialise empty raster
    
    welfare_loss_rast <- rasterize(cbind(welfare_loss_df$x, welfare_loss_df$y), welfare_loss_rast, field = welfare_loss_df$welfare_loss)
    WL_list[[j]] <- welfare_loss_rast
    
    cat("Completed", tile, return_period, "\n")
  }
  
  # save rasters
  for (RP in 1:length(AL_list)) {
    return_period <- RP.list[[RP]]
    asset_loss_rast <- AL_list[[RP]]
    writeRaster(asset_loss_rast, filename = paste0("output filepath to save asset loss rasters", tile,"_Y2020_", return_period, ".tif"), format = "GTiff", overwrite = TRUE)
  }
  
  # Save each individual raster from WL_list
  for (RP in 1:length(WL_list)) {
    return_period <- RP.list[[RP]]
    welfare_loss_rast <- WL_list[[RP]]
    writeRaster(welfare_loss_rast, filename = paste0("output filepath to save well-being loss rasters", tile,"_Y2020_", return_period, ".tif"), format = "GTiff", overwrite = TRUE)
    
  }
  
  end <- Sys.time()
  cat(tile, "completed: ", format(end), "\n")
}









