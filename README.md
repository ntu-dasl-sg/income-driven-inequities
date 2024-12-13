# Shared Hazards, Unequal Outcomes: Income-Driven Inequities in Disaster Risk

Code is available for the analyses conducted in the above study. 

**00_functions.R** contains all the functions required for conducting the standard asset loss and well-being loss calculations

**01_assign-deciles.R** assigns income deciles based on relative wealth indices from Chi et al. (2022)

**02_assign_income_savings.R** assigns income and savings distributions by province to deciles (from previous step)

**03_mask_province_w_mun.R** masks income/savings rasters for provinces where municipality/HUC-level data is available

**04_damage-fraction-maps.R** makes damage fraction maps from income-differentiated vulnerability curves.

**05_calc-mean-consump.R** calculates the mean consumption of the entire Philippines required for calculating well-being losses

**06_rr-optimisation.R** optimises recovery rates and computes asset and well-being losses

**07_assign-results-PH.R** assigns results from the previous step to rasters.

**08_EAD.R** integrates across all return periods to calculate the average annual asset and well-being losses