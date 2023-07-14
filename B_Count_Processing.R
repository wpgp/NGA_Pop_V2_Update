# Load required libraries
library(sf)
library(tictoc)
library(tidyverse)
library(terra)
library(groupdata2)
library(feather)

#Specify Drive Path
drive_path <- "//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/Working/NGA/Ortis/"
footprint_path <- paste0(drive_path, "AFRICA_NIGERIA_building_year2-001/")
mgrid_path <- paste0(drive_path, "Data/")
output_path <- paste0(drive_path, "NGA_buidlings_v2/")


# Load dataset ------------------------------------------------------------

#read mastergrid
mgrid <- rast(paste0(mgrid_path, "Grid_ID.tif"))

#get pixel values and see if there are NAs in the grid cell ids
mgrid_id <- terra::values(mgrid, dataframe = T) 

#check for NA values
any(is.na(mgrid_id))

#check for Grid_ID duplicates
any(duplicated(mgrid_id))

#Remove mgrid_id
rm(mgrid_id); gc()

#Load footprint shapefiles and rbind them all as one dataset

#specify pattern for file names
pattern = "year2.*\\.shp$"

#list all files that match the pattern
all_files <-dir(footprint_path,pattern=pattern)
all_files

# Remove 3 files labeled year2_deleted
myfiles <- all_files[!grepl("year2_deleted\\.shp$", all_files)]
myfiles

#read files and rbind them

tic()

B_footprint <- myfiles %>% 
  map(function(x) st_read(file.path(footprint_path, x))) %>% 
  map(~ st_transform(., st_crs(mgrid))) %>%  # Project each dataset to the same CRS as mgrid
  map(~ select(., geometry)) %>%         #Select only geometry to reduce data size
  reduce(rbind) 

toc() #takes 3hrs to read-in data


#Create a grouping variable for subsetting data in chunks
B_count_df <- B_footprint %>% 
  group(n = 2000000, method = "greedy", col_name = "Group_ID") %>% 
  ungroup() 

rm(B_footprint); gc()

# split the data into chunks based on the Group_ID
B_count_df <- B_count_df %>% 
  group_split(Group_ID)


# Script for processing footprint -----------------------------------------

tic() 

# Function to process the footprint

for(dd in B_count_df){
  # get the ID of the current chunk being processed
  typro <- unique(dd$Group_ID)
  print(typro)
  
  #correct invalid geometries
  dd <- st_make_valid(dd)
  
  # Calculate the centroid of the polygon
  centroid <- st_centroid(dd)
  
  # Convert the centroid to a point
  centroid_point <- st_cast(centroid, "POINT")
  
  # Extract values from raster under each point to get grid_id
  values <- terra::extract(mgrid, centroid_point) 
  
  #Write each group to file
  
  write_feather(values, paste0(output_path, "Group_", unique(dd$Group_ID), ".feather"))
  
} 

toc() #Takes 16 hours to process

rm(B_count_df,  centroid, centroid_point, dd, values); gc()


# Read files back to memory -----------------------------------------------


#Read all files back to memory and rbind them
tic()

myfiles <-dir(output_path,pattern="*.feather")

B_count <- myfiles %>% 
  map(function(x) read_feather(file.path(output_path, x))) %>% 
  reduce(rbind) 

toc()

#Group observations by Grid_ID and count observations falling in each Grid_ID 
B_count <- B_count %>% 
  group_by(Grid_ID) %>% 
  summarise(count = n()) %>% 
  ungroup()


# Rasterize count ---------------------------------------------------------

#Get pixel values
mgrid_id <- terra::values(mgrid, dataframe = T) 

#Remove grid
rm(mgrid); gc()

#Join building count to the right grid id

mgrid_id <- mgrid_id %>% 
  full_join(B_count, by = "Grid_ID")

#Read mastergrid again
mgrid <- rast(paste0(mgrid_path, "Grid_ID.tif"))

#Assign predictions to Grid Raster

mgrid[]<- mgrid_id$count


plot(mgrid)

writeRaster(mgrid, paste0(output_path, "NGA_building_count_v2.tif"), 
            overwrite = T, names = "NGA_buildings_v2_count")

###############End of script##################################################

