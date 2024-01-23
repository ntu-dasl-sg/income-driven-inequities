library(raster)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(data.table)
library(stats)

# Read raster data
flood_rast_2020 <- raster("/Users/jeancjw/Documents/00_Data/Coastal flood maps/Philippines/RCP45/Y2020/merged/merge_Y2020_RP100.tif") # year 2020
flood_rast_2050 <- raster("/Users/jeancjw/Documents/00_Data/Coastal flood maps/Philippines/RCP45/Y2050/merged/merge_Y2050_RP100.tif") # year 2050
labour_income_rast <- raster("/Users/jeancjw/Documents/00_Data/Income expenditure savings 2021/Labour income rasters/labour_income_merged.tif")
building_rast <- raster("/Users/jeancjw/Documents/00_Data/Buildings/buildingcount_90m.tif")


AL_rast_20 <- raster("/Users/jeancjw/Documents/03_Results/EAD/PH/wo_buildings/AL_PH_Y2020.tif")
WL_rast_20 <- raster("/Users/jeancjw/Documents/03_Results/EAD/PH/wo_buildings/WL_PH_Y2020.tif")


# Get raster values and convert to df
data_2020 <- data.frame(
  flood_depths = getValues(flood_rast_2020),
  income = getValues(labour_income_rast ),
  welfare_loss = getValues(WL_rast_20),
  asset_loss = getValues(AL_rast_20),
  num_households = getValues(building_rast)
#### 1. FLOOD EXPOSURE ####

# Filter data for households that experience flooding, num_households is not NA, and income is not 0
flooded_data_2020 <- subset(data_2020, !is.na(num_households) & income > 0)

# Filter only households that are flooded
flooded_data_2020 <- flooded_data_2020 %>% filter(flood_depths>0) 

# Replicate rows based on num_households
flooded_data_2020 <- flooded_data_2020 %>%
  slice(rep(row_number(), num_households)) %>%
  select(-num_households) 
# flooded_data_2050 <- flooded_data_2050[, .(flood_depths = rep(flood_depths, num_households),
#                                            income = rep(income, num_households)),
#                                        by = 1:nrow(flooded_data_2050)]


# Sort the data by income
flooded_data_2020 <- flooded_data_2020[order(flooded_data_2020$flood_depths), ]
flooded_data_2050 <- flooded_data_2050[order(flooded_data_2050$flood_depths), ]



# Create Lorenz data
# length(cumulative_flood_20)
# length(flooded_data_2020$cumulative_percent_flood_depths)
## Since the flood data is arranged according to decreasing income and each row is a household, we can ignore the income column and just create the cumulative % of households column.
lorenz_20 <-  data.frame(
  cum_perc_households = seq(0, 100, length.out = length(flooded_data_2020$income)),
  # cum_asset_loss = cumsum(flooded_data_2020$asset_loss) / sum(flooded_data_2020$asset_loss)*100,
  # cum_welfare_loss = cumsum(flooded_data_2020$welfare_loss) / sum(flooded_data_2020$welfare_loss)*100,
  cum_flood = cumsum(flooded_data_2020$flood_depths) / sum(flooded_data_2020$flood_depths)*100
)



# Plot the Lorenz curve for 2020
ggplot(lorenz_20, aes(x = cum_perc_households , y = cum_flood)) +
  geom_ribbon(aes(ymin = pmin(cum_perc_households, cum_flood), 
                  ymax = pmax(cum_perc_households, cum_flood)), 
              fill = "lightblue", 
              alpha = 0.5) +
  # geom_ribbon(aes(ymin = pmin(cum_perc_households, cum_welfare_loss), 
  #                 ymax = pmax(cum_perc_households, cum_welfare_loss)), 
  #             fill = "orange", 
  #             alpha = 0.6) +
  geom_line(color = "#294475", linewidth = 1) +
  # geom_line(aes(x = cum_perc_households, y = cum_welfare_loss),color = "darkorange", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curve for Flooded Households in 2020",
       x = "Cumulative % of Households (Descending income)",
       y = "Cumulative % of Losses") + 
  theme_bw() + 
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

# Plot the Lorenz curve for 2050



# Calculate gini
gini_20 <- calculate_gini(lorenz_20$cum_perc_households, lorenz_20$cum_perc_flood)
print(gini_20)

gini_50 <- calculate_gini(lorenz_50$cum_perc_households, lorenz_50$cum_perc_flood)
print(gini_50)

# Combine data for both years
combined_data <- rbind(cbind(flooded_data_2020, year = "2020"), cbind(flooded_data_2050, year = "2050"))
head(combined_data)

# Plot the Lorenz curves for both years on the same plot
ggplot(combined_data, aes(x = cumulative_percent_households, y = cumulative_percent_flood_depths, color = year, group = year)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(title = "Lorenz Curves for Flooded Households in 2020 and 2050",
       x = "Cumulative % of Households (by income)",
       y = "Cumulative % of Flood Depths")
