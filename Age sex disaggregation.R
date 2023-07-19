
#Load libraries

library(tidyverse)
library(raster)
library(sf)
library(tictoc)


#Specify Drive Path
drive_path <- "//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_GRID3/Working/NGA/Ortis/"
agesex_path <- paste0(drive_path, "Age-Sex/")
pop_path <-  paste0(drive_path, "NGA_population_v2_gridded/")
shapefile_path <- paste0(drive_path, "Data/")
output_path <- paste0(drive_path, "Age-Sex/")


# Load dataset ------------------------------------------------------------

agesex_table <- read.csv(paste0(agesex_path,'NGA_US_Census_Bureau_2020_agesex.csv'))  #Age-Sex proportions
totpop <- raster(paste0(pop_path, "NGA_Gridded_Pop_v2_Weight_Disag.tif"))   #Adjusted total population
lga <- read_sf(paste0(shapefile_path, "Nigeria_adm2_uscb_2016.shp"))
agesexid <- raster(paste0(agesex_path, "lga_id.tif"))
male_female_prop <- read.csv(paste0(agesex_path, "Male_Female_Prop.csv"))   #male and female proportions 

#merge male_female_prop with lga boundaries
lga_male_female_prop <- lga %>% 
  full_join(male_female_prop, by = "GEO_MATCH")

#Project lga to pop raster
lga_male_female_prop<- st_transform(lga_male_female_prop, crs = st_crs(totpop))

#use total population as a base raster for rasterization
baseRaster <- totpop

MaleProp_raster <- rasterize(lga_male_female_prop, baseRaster, field = "MaleProp")
#plot(MaleProp_raster)

FemaleProp_raster <- rasterize(lga_male_female_prop, baseRaster, field = "FemaleProp")
#plot(FemaleProp_raster)

#multiply male and female proportion raster with total population and write to file
male_total_pop <- totpop * MaleProp_raster
female_total_pop <- totpop * FemaleProp_raster

writeRaster(male_total_pop, paste0(output_path,"totMale.tif"), format = 'GTiff', overwrite = TRUE)
writeRaster(female_total_pop, paste0(output_path,"totFemale.tif"), format = 'GTiff' , overwrite = TRUE)


# Disagregating Population Totals -----------------------------------------

names(agesex_table)[1] <- 'id'
row.names(agesex_table) <- agesex_table$id

crs1 <- crs(totpop)
ex1 <- xmin(totpop); ex2 <- xmax(totpop); ex3 <- ymin(totpop); ex4 <- ymax(totpop)
agesexid_m <- as.matrix(agesexid)

# primary age-sex groups
male_groups <- names(agesex_table)[grepl('m_', names(agesex_table))]
female_groups <- names(agesex_table)[grepl('f_', names(agesex_table))]

#Create folder called proportions
dir.create(paste0(output_path,'proportions/'))  


# Rasterize primary age-sex proportions -----------------------------------
# Male Proportions 

tic()

# males
for(g in male_groups){
  print(g)
  
  g_m <- agesexid_m
  g_m[] <- NA
  
  for(id in agesex_table$id){
    g_m[which(agesexid_m==id)] <- agesex_table[as.character(id), g]
  }
  
  g_raster <- raster(g_m, crs=crs1, xmn=ex1, xmx=ex2, ymn=ex3, ymx=ex4)
  g2 <- gsub("_","",g) # remove 'underscore' from names
  writeRaster(g_raster, paste0(output_path,'proportions/',g2,'.tif'), overwrite = T)
}

toc()

# Female Proportions ------------------------------------------------------

tic()

# females
for(g in female_groups){
  print(g)
  
  g_m <- agesexid_m
  g_m[] <- NA
  
  for(id in agesex_table$id){
    g_m[which(agesexid_m==id)] <- agesex_table[as.character(id), g]
  }
  
  g_raster <- raster(g_m, crs=crs1, xmn=ex1, xmx=ex2, ymn=ex3, ymx=ex4)
  g2 <- gsub("_","",g) # remove 'underscore' from names
  writeRaster(g_raster, paste0(output_path,'proportions/',g2,'.tif'), overwrite = T)
}

toc()


# Population Raster for Each Primary Age-Sex Group ------------------------

dir.create(paste0(output_path,'pop_count/'))

#Male

tic()

male_tot   <- raster(paste0(output_path,"totMale.tif"))
for(g in c(male_groups)){
  print(g)
  g2 <- gsub("_","",g) # remove 'underscore' from names
  prop <- raster(paste0(output_path,'proportions/',g2,'.tif'))
  ras <- male_tot * prop
  
  writeRaster(ras, paste0(output_path,'pop_count/',g2,'.tif'),overwrite=T)
}
rm(male_tot, prop, ras, g, g2)

toc()

female_tot <- raster(paste0(output_path,"totFemale.tif"))
for(g in c(female_groups)){
  print(g)
  g2 <- gsub("_","",g) # remove 'underscore' from names
  prop <- raster(paste0(output_path,'proportions/',g2,'.tif')) 
  ras <- female_tot * prop
  writeRaster(ras, paste0(output_path,'pop_count/',g2,'.tif'),overwrite=T)
}
rm(female_tot, prop, ras, g)


