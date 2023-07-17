
#Load libraries
library(terra)
library(tidyverse)


#Specify Drive Path
drive_path <- "//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/Working/NGA/Ortis/"
pop_path <- paste0(drive_path, "NGA_population_v1_gridded/")
building_v1_path <- paste0(drive_path, "NGA_buildings_v1_1/")
building_v2_path <- paste0(drive_path, "NGA_buidlings_v2/")
data_path <- paste0(drive_path, "Data/")
output_path <- paste0(drive_path, "NGA_population_v2_gridded/")


# Import Rasters and Stack ----------------------------------------------

mgrid <- rast(paste0(data_path, "Grid_ID.tif"))
ward <-  rast(paste0(data_path, "NGA_Ward.tif"))
pop <- rast(paste0(pop_path, "NGA_population_v2_0_gridded.tif"))
b_count_v1 <- rast(paste0(building_v1_path, "NGA_buildings_v1_1_count.tif"))
b_count_v2 <- rast(paste0(building_v2_path, "NGA_building_count_v2.tif"))

#Stack all rasters
stack_rasters<- c(mgrid, ward, pop, b_count_v1, b_count_v2)


#get raster values and rename rasters
raster_df <- terra::values(stack_rasters, dataframe = T) %>% 
  rename(B_Count_v1 = NGA_buildings_v1_1_count, 
         B_Count_v2 = NGA_buildings_v2_count,
         NGA_Pop = NGA_population_v2_0_gridded) 


#Remove rasters from memory
rm(mgrid, ward, pop, b_count_v1, b_count_v2, stack_rasters); gc()

#sum v1 population
sum(raster_df$NGA_Pop, na.rm = T)

#difference between v2 and v1 building count
diff<- sum(raster_df$B_Count_v2, na.rm = T) - sum(raster_df$B_Count_v1, na.rm = T)
diff

#Percent increase
diff/sum(raster_df$B_Count_v1, na.rm = T)*100


# Ward Population Checks --------------------------------------------------

#Checks indicate that some population totals are outside the country extent(ward extent)
#Those have been labelled ward 0
#Filter ward 0 and check total population
ward_0 <- raster_df %>% 
  filter(NGA_Ward ==0)

#Find ward_0 population totals
sum(ward_0$NGA_Pop, na.rm = T)

#get total population for each ward and total v2 building count
original_ward_population <- raster_df %>% 
  group_by(NGA_Ward) %>% 
  summarise(original_ward_pop = sum(NGA_Pop, na.rm = T),
            Total_v2_Count = sum(B_Count_v2, na.rm = T))


#Sum ward population to see if its tally with overall population
sum(original_ward_population$original_ward_pop)

#join ward population to raster_df
raster_df <- raster_df %>% 
  full_join(original_ward_population, by = "NGA_Ward")

#Estimate new population base on new building footprint

raster_df <- raster_df %>% 
  mutate(new_pop = B_Count_v2/Total_v2_Count * original_ward_pop)


#Check if new national population totals sum to original national total population
sum(raster_df$new_pop, na.rm = T) == sum(raster_df$NGA_Pop, na.rm = T) 
sum(raster_df$new_pop, na.rm = T)
sum(raster_df$NGA_Pop, na.rm = T)
#NB: why is it false when values are same? 

#What is the difference?
diff<- sum(raster_df$new_pop, na.rm = T) - sum(raster_df$NGA_Pop, na.rm = T)
diff

#The difference is 0.000000089 (very insignificant)

#Calculate new ward population totals
ward_df <- raster_df %>% 
  group_by(NGA_Ward) %>% 
  summarise(new_ward_pop = sum(new_pop, na.rm = T)) 

#join original ward population totals to new ward population
ward_df <- ward_df %>% 
  inner_join(original_ward_population, by = "NGA_Ward")

#Check if new ward population totals tallys with original ward population
all(ward_df$new_ward_pop == ward_df$original_ward_pop)

#Find wards with different populations
ward_df <- ward_df %>% 
  mutate(Same_Pop = if_else(new_ward_pop == original_ward_pop, "True", "False"))

#check correlation between new and old population at the ward level
cor(ward_df$new_ward_pop, ward_df$original_ward_pop)

#ward level differences
ward_df <- ward_df %>% 
  mutate(ward_diff = new_ward_pop - original_ward_pop)

#filter wards with different pop
diff_ward<- ward_df %>% 
  filter(Same_Pop == "False")

# Rasterize new population count ---------------------------------------------------------

#Get pixel values
mgrid <- rast(paste0(data_path, "Grid_ID.tif"))

#Assign predictions to Grid Raster

mgrid[]<- raster_df$new_pop

plot(mgrid)

writeRaster(mgrid, paste0(output_path, "NGA_Gridded_Pop_v2_Simple_Disag.tif"), 
            names = "Population", overwrite = T)

###############End of script##################################################










