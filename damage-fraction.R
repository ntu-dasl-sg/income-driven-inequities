##########################
#### DAMAGE FRACTIONS ####
##########################
library(raster)
library(tidyverse)
library(ggplot2)
library(janitor)
library(terra)
library(dplyr)
library(sp)
library(rgdal)
library(readr)

source("~/Documents/02_Code/functions.R")
setwd("/Users/jeancjw/Documents") # set working dir

# read flood maps 
filelist <- list.files(path = "00_Data/Coastal flood maps/Philippines/RCP45/Y2020/merged",
                       pattern = '.tif$',
                       full.names = TRUE)
flood.rasters <- lapply(filelist, raster) # lists flood maps for each RP, water depths in cm

# read damage function & interpolate values
df <- open_damage_function("00_Data/Damage-depth function/global_dmgfn_res.csv") 

# apply damage function to get damage fraction map
df1 <- damage_fraction_map(flood.rasters[[1]])
df10 <- damage_fraction_map(flood.rasters[[2]])
df100 <- damage_fraction_map(flood.rasters[[3]])
df1000 <- damage_fraction_map(flood.rasters[[4]])
df10000 <- damage_fraction_map(flood.rasters[[5]])
df2 <- damage_fraction_map(flood.rasters[[6]])
df2000 <- damage_fraction_map(flood.rasters[[7]])
df25 <- damage_fraction_map(flood.rasters[[8]])
df250 <- damage_fraction_map(flood.rasters[[9]])
df5 <- damage_fraction_map(flood.rasters[[10]])
df50 <- damage_fraction_map(flood.rasters[[11]]) 
df500 <- damage_fraction_map(flood.rasters[[12]])


