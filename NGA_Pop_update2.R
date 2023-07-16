
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

#get total population for each ward
original_ward_population <- raster_df %>% 
  group_by(NGA_Ward) %>% 
  summarise(original_ward_pop = sum(NGA_Pop, na.rm = T)) 
          

#Sum ward population to see if its tally with overall population
sum(original_ward_population$original_ward_pop)

# Some logical conditions checks -----------------------------------------------------

#1. Find observations where B_Count_v2 is na but NGA_Pop is not na
b2 <- raster_df %>% 
  filter((is.nan(B_Count_v2) & !is.nan(NGA_Pop)))
#based on the result we would assign na to the population in this grid cell since B_Count_v2 is na

#2. Find observations where B_Count_v2 is not na but B_Count_v1 is na
b1 <- raster_df %>% 
  filter(!is.nan(B_Count_v2) & is.nan(B_Count_v1))
#based on this logical conditions because B_Count_v2 is not na but B_Count_v1 is na,
#we will assign 1 building to B_Count_v1 and then assign 1 person to the population

#Apply the following logical conditions based on the conditions above
#if B_Count_v2 == NA, NGA_Pop must be NA
#Conversely if B_Count_v2 is not NA and B_Count_v1 == NA we
#assign 1 to B_Count_v1 and NGA_Pop==1

raster_df <- raster_df %>% 
  mutate(NGA_Pop = if_else(is.nan(B_Count_v2), NaN, NGA_Pop)) %>% 
  mutate(NGA_Pop = if_else(!is.nan(B_Count_v2) & is.nan(B_Count_v1), 1, NGA_Pop),
         B_Count_v1 = if_else(!is.nan(B_Count_v2) & is.nan(B_Count_v1), 1, B_Count_v1))

#check if logical condition worked perfectly
raster_df %>% 
  filter((is.nan(B_Count_v2) & !is.nan(NGA_Pop))) #if it worked you should get no observation

raster_df %>% 
  filter(!is.nan(B_Count_v2) & is.nan(B_Count_v1)) #if it worked you should get no observation


# Adjusting population totals using B_count_v2 ----------------------------

#Estimate new population base on new building footprint

raster_df <- raster_df %>% 
  mutate(new_pop = B_Count_v2/B_Count_v1 * NGA_Pop)


#Check if new national population totals sum to original national total population
sum(raster_df$new_pop, na.rm = T) == sum(raster_df$NGA_Pop, na.rm = T)

#What is the difference?
diff<- sum(raster_df$new_pop, na.rm = T) - sum(raster_df$NGA_Pop, na.rm = T)
diff

#What is the percent increase(7.3% increase above old population)
diff/sum(raster_df$NGA_Pop, na.rm = T)*100

#Calculate new ward population totals
ward_df <- raster_df %>% 
  group_by(NGA_Ward) %>% 
  summarise(new_ward_pop = sum(new_pop, na.rm = T)) 

#join original ward population totals to new ward population
ward_df <- ward_df %>% 
  inner_join(original_ward_population, by = "NGA_Ward")

#Check if new ward population totals tallys with original ward population
all(ward_df$new_ward_pop == ward_df$original_ward_pop)

#check correlation between new and old population at the ward level
cor(ward_df$new_ward_pop, ward_df$original_ward_pop)


# Rasterize new population count ---------------------------------------------------------

#Get pixel values
mgrid <- rast(paste0(data_path, "Grid_ID.tif"))

#Assign predictions to Grid Raster

mgrid[]<- raster_df$new_pop

plot(mgrid)

writeRaster(mgrid, paste0(output_path, "NGA_Gridded_Pop_v2.tif"), overwrite = T)

###############End of script##################################################

