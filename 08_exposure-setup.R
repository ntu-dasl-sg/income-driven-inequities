#### EXPOSURE ####
# This code sets up the data for our exposure analysis.
# The resulting data frame contains flood depths and labour income for each building in the Philippines situated within 1km of the coast.

library(raster)
library(ggplot2)
library(dplyr)

#### 1. Read flood maps ####
flood20.tiles <- lapply(list.files(path = "D:/Jeanette/00_Data/PH Coastal Flood Maps/Philippines_30m/2020",
                                   pattern = "RP100.tif",
                                   full.names = T),
                        raster)
flood20.tiles <- flood20.tiles[-c(2,76)] # remove empty tiles

flood50.tiles <- lapply(list.files(path = "D:/Jeanette/00_Data/PH Coastal Flood Maps/Philippines_30m/2050",
                                   pattern = "RP100.tif",
                                   full.names = T),
                        raster)

flood50.tiles <- flood50.tiles[-c(2,76)] # remove empty tiles

#### 2. Read building rasters ####
buildings_1km <- raster("D:/Jeanette/00_Data/Buildings/buildingcount_30m_1km_buffer.tif") # 1km buffer pre-processed in QGIS
building.tiles <- lapply(list.files(path = "D:/Jeanette/00_Data/Buildings/Tiles",
                                    pattern =".tif",
                                    full.names = T),
                         raster)

#### 3. Read labour income rasters ####
income.tiles <- lapply(list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/Labour income/tiles",
                                  pattern = ".tif",
                                  full.names = T),
                       raster)

# We use only labour income and not income from all sources because the poverty threshold is concerned with disposable income.

# hh_income <- raster("D:/Jeanette/00_Data/FIES 2021/rasters/hh_income_PH.tif")

tilenames <- tools::file_path_sans_ext(basename(list.files(path = "D:/Jeanette/00_Data/FIES 2021/rasters/Labour income/tiles",
                                                           pattern = ".tif")))

tilenames
df20_res <- list()
df50_res <-list()
for (i in 1:length(tilenames)){
  tile <- tilenames[[i]]
  tile
  flood20 <- flood20.tiles[[i]]
  flood50 <- flood50.tiles[[i]]
  buildings <- crop(buildings_1km, building.tiles[[i]]) # diff ext from others
  income <- crop(hh_income,income.tiles[[i]])

  # Resample buildings and income
  buildings <- resample(buildings, flood20, method = "ngb")
  income <- resample(income, flood20, method="ngb")
  
  # Convert Raster to Points
  flood20_pts <- rasterToPoints(flood20)
  flood50_pts <- rasterToPoints(flood50)
  income_pts <- rasterToPoints(income)
  building_pts <- rasterToPoints(buildings)
  
  # Merge df
  df20 <- merge(merge(building_pts, income_pts, by = c("x", "y"), all.x=T), flood20_pts, by = c("x", "y"), all.x=T)
  df50 <- merge(merge(building_pts, income_pts, by = c("x", "y"), all.x=T), flood50_pts, by = c("x", "y"), all.x=T)
  colnames(df20) <- c("x", "y", "buildings", "income", "depth")
  colnames(df50) <- c("x", "y", "buildings", "income", "depth")
  
  # replicate rows based on num buildings
  df20 <- df20 %>% 
    slice(rep(row_number(), buildings)) %>% 
    dplyr::select(-buildings)
  df50 <- df50 %>% 
    slice(rep(row_number(), buildings)) %>% 
    dplyr::select(-buildings)
  
  # change NA depths to 0
  df20$depth[is.na(df20$depth)] <- 0
  df50$depth[is.na(df50$depth)] <- 0 
  
  # add year column
  df20$year <- "2020"
  df50$year <- "2050"
 
  # save to list
  df20_res[[i]] <- df20
  df50_res[[i]] <- df50
  
  cat(tile, "completed.\n")
}


saveRDS(df20_res, file = "D:/Jeanette/02_Code/08_DATA20_buffer_1k.rds")
saveRDS(df50_res, file = "D:/Jeanette/02_Code/08_DATA50_buffer_1k.rds")
