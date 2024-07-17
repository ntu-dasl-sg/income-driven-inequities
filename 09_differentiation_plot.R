library(readr)
library(dplyr)
library(tidyverse)


# Read in results from recovery rate optimisation
results.list_prov <- lapply(list.files(path="D:/Jeanette/03_Results/diff_vuln/PROVINCE",
                                       pattern=".csv$",
                                       full.names=T),read_csv)

results.list_mun <- lapply(list.files(path="D:/Jeanette/03_Results/diff_vuln/MUNICIPALITY",
                                      pattern=".csv$",
                                      full.names=T),read_csv)
# combine both lists - these are results for differentiated vulnerability
combined.list <- c(results.list_prov, results.list_mun)
df_vuln <- bind_rows(combined.list)



# Read in income-differentiated damage functions
df_low <- read_csv("D:/Jeanette/00_Data/Damage-depth function/dmg_fn_low.csv")
df_mid <- read_csv("D:/Jeanette/00_Data/Damage-depth function/dmg_fn_mid.csv")
df_high <- read_csv("D:/Jeanette/00_Data/Damage-depth function/dmg_fn_high.csv")

# Function to open damage function & interpolate values
open_damage_function <- function(df_file){ # opens the damage function file and interpolates between values
  df <- read.csv(file = df_file)
  df_interp <- approx(unlist(df[1]), unlist(df[2]), seq(0, 10, by = 0.01)) # df[1]=wd, #df[2]=damagefrac
  return(as.data.frame(df_interp)) # returns damage fractions
}

df_JRC <- open_damage_function("D:/Jeanette/00_Data/Damage-depth function/global_dmgfn_res.csv")
df_JRC$cat <- rep("JRC", nrow(df_JRC))
df_JRC

#### No diff in exposure or vulnerability ####
fd = 0.50 # assume 50cm flood depth
v = 0.3300 # assume same vulnerability -> damage fraction corresponding to 50cm of flooding from JRC vulnerability curve
bldgs = 1 # 1 building per income group
# maxd = 1562.14423 # converted from JRC max damage
income_L = 312  # assume same income for all (here we use the mean consumption level as labour income, but can be any value)
maxd = 2.08*income_L - 0.000056 * 0.9 # replacement cost based on labour income

res_allsame = v*maxd*1 # get risk/losses
res_allsame # 515.5076

# create data frame of results
df1 <- data.frame(HH=c("Low","Middle", "High"),
           Losses = rep(res_allsame,3),
           cat= rep("No differentiation", 3))

#### Accounting for asset value exposed ####

fd = 0.50 # assume 50cm flood depth
v = 0.3300 # assume JRC damage-depth function
bldgs = 1 # 1 building per income group
income_L = c(106.88544,351.54315,888.8834) # corresponding labour incomes of each income group
maxd = 2.08*income_L - 0.000056 * 0.9 # replacement cost based on labour income

res_asset = v*maxd*1
res_asset 

df2 <- data.frame(HH=c("Low","Middle", "High"),
                  Losses = res_asset,
                  cat= rep("Differentiated asset value", 3))
df2

#### Accounting for differential exposure ####
fd = c(1,0.50,0.15) # i.e. low income more exposed
v =  c(0.49,0.264,0.0990)# assume JRC function
# income_L = c(312.54315,312.54315,312.54315)
income_L = c(106.88544,351.54315,888.8834)
maxd = 2.08*income_L - 0.000056 * 0.9 # replacement cost based on income

res_diffExp = v*maxd

# create data frame of results for differentiated exposure
df3 <- data.frame(HH=c("Low","Middle", "High"),
                  Losses = res_diffExp,
                  cat= rep("Differentiated exposure", 3))
df3


#### Accounting for differential exposure + physical vulnerability ####
fd = c(1,0.50,0.15) # i.e. low income more exposed
v =  c(0.60789474,0.306376815,0.073527273)# assume diff vulnerability

income_L = c(106.88544,351.54315,888.8834) 
maxd = 2.08*income_L - 0.000056 * 0.9 # replacement cost based on income

res_diffExpVul <- maxd*v
res_diffExpVul

df4 <-  data.frame(HH=c("Low","Middle", "High"),
                   Losses = res_diffExpVul,
                   cat= rep("Differentiated exposure & vulnerability", 3))


df4
#### Well-being losses ####
# constants 
avg_prod_cap = 0.23
elasticity =1.5
discount_rate=0.1
t=0:20
mean_consumption <- 312.8738
building_count=1

fd = c(1,0.50,0.15) # i.e. low income more exposed
v =  c(0.40736842,0.306376815,0.073527273)# assume diff vulnerability
labour_income = c(106.88544,351.54315,888.8834) 
other_income = c(32.2758,79.7895,367.66809)
total_savings = c(27.51473,438.7233,1636.1378)

WL = c(803.5650, 293.4928, 72.00007) # from df_vuln

df5 <-  data.frame(HH=c("Low","Middle", "High"),
                   Losses = WL,
                   cat= rep("Well-being losses", 3))


### Combine all dfs ###

df <- rbind(df1,df2,df3,df4,df5)

df$cat <- factor(df$cat,
                 levels = c("No differentiation",
                            "Differentiated asset value",
                            "Differentiated exposure",
                            "Differentiated exposure & vulnerability",
                            "Well-being losses"))
df$HH <- factor(df$HH, 
                levels = c("Low", "Middle", "High"))


# Define fill colors
fill_colors <- c(
    "No differentiation" = "grey90",
    "Differentiated asset value" = "#cbebf7",
    "Differentiated exposure" ="#8dc6e0",
    "Differentiated exposure & vulnerability" = "#5aa8d3",
    "Well-being losses" = "#3c7aa0"
  )
  

# Plot losses based on level of differentiation
ggplot(df, aes(x = HH, y = Losses, fill = cat)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = fill_colors) +
    labs(y = "Risk / Losses",
         x = "Income group",
         fill="") +
    theme_bw() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_line(color="grey90",size=0.1),
      panel.grid.minor = element_blank()) +
  facet_wrap(~ factor(str_wrap(cat, width = 40), levels = unique(str_wrap(df$cat, width = 40))), scales = "fixed")

# ggsave(filename="D:/Jeanette/03_Results/Figures/Paper/Rel_diff_risk2.png", plot=last_plot(), dpi=300, width=10, height=5.5)


