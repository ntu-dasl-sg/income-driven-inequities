######################
### ASSIGN INCOMES ###
######################

# Loops through all tiles in the Philippines
## same code used for other income (col 7) & total savings (col 5)

library(raster)
library(readr)
library(plyr)

#### 1. SET UP + LOAD DATA ####
tile.list <- read_csv("/Users/jeancjw/Documents/00_Data/Coastal flood maps/Philippines/PH File list.csv")$latlong
regions <- c("ARMM", "CAR", "NCR", "I", "II", "III", "IVA", "IVB", "IX", "V", "VI", "VII", "VIII", "X", "XI", "XII", "XIII")

##### 1.1 ADMIN BOUNDARY #####
admin.list <- list.files(path = "00_Data/Admin boundaries/Philippines/rast_ADM01", pattern = ".tif$", full.names = TRUE)


# Read FIES data (only need to read in once)
filenames <- list.files(path = "/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021",
                        pattern = "IES.csv", 
                        full.names = TRUE) 

FIES.data <- lapply(filenames, read_csv)

# Iterate over tiles
for (tile in 1:102) {
  tilename <- tile.list[tile]
  
  admin.rasters <- lapply(admin.list, raster)
  admin_boundary <- admin.rasters[[tile]]
  
  ##### 1.2 INCOME DECILE & INCOME DATA #####
  income_decile_rast <- crop(raster("/Users/jeancjw/Documents/00_Data/Income decile/decile_stitched_region.tif"), admin_boundary)
  
  # Convert income tables (FIES.data) to matrix
  convert_to_matrix <- function(df) {
    return(data.matrix(df[, c(1,7)], rownames.force = NA))
  } 
  
  income_matrix_list <- lapply(FIES.data, convert_to_matrix) 
  
  decile_order <- c(1:10) 
  
  # Use lapply to add the new column to each matrix in income_mat_list
  income_matrix_list <- lapply(income_matrix_list, function(mat) {
    return(cbind(income_decile = decile_order, labour_income = mat[, 2]))
  })
  
  
  #### 2. RECLASSIFY RASTERS ####
  
  reclassified_rasters <- lapply(income_matrix_list, function(income_matrix) {
    reclassified_raster <- reclassify(income_decile_rast, income_matrix)
    return(reclassified_raster)
  })
  
  # Convert rasters to the same extent as admin boundary rast
  reclassified_rasters <- lapply(reclassified_rasters, function(r) resample(r, admin_boundary, method = "ngb"))
  
  
  #### 3. ASSIGN INCOMES BASED ON ADMIN BOUNDARY ####
  income_rast <- admin_boundary
  
  for (region in 1:length(reclassified_rasters)) {
    income_rast[admin_boundary == region] <- reclassified_rasters[[region]][admin_boundary == region]
  }
  
  # Plot or save the income_rast for the current tile
  plot(income_rast)
  
  writeRaster(income_rast, filename = paste0("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/Other income rasters/other_income_", tilename, ".tif"), format = "GTiff", overwrite = TRUE)
}
