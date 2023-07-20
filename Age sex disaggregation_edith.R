# Create standard set of age-sex rasters from source files in W:/Projects/WP517763_WOPR_DataRelease/woprUtils
# Source files:  1) gridded population raster, 2) agesex region ID raster, 3) agesex table .csv

# cleanup
rm(list=ls()); gc(); cat("/014"); try(dev.off(), silent=T);

# packages
library(woprUtils)
library(peanutButter)

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'../wd'))

# settings
country <- 'NGA'
version <- 'v2.1'
woprname <- paste0(country,'_population_',gsub('.','_',version, fixed=T),'_')

outdir <- file.path('//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_WOPR_DataRelease/DataReview/NGA/population/v2.1/NGA_population_v2_1_agesex/')

# inputs
srcdir <- '//worldpop.files.soton.ac.uk/worldpop/Projects/WP517763_WOPR_DataRelease/woprUtils'

population <- raster::raster(file.path(srcdir,'pop',paste0(woprname,'gridded.tif')))
regions <- raster::raster(file.path(srcdir,'agesexid',paste0(woprname,'agesex_regions.tif')))
proportions <- read.csv(file.path(srcdir,'agesex',paste0(woprname,'agesex_table.csv')), stringsAsFactors=F)

# create rasters
woprAgesexRasters(country = country, 
                  version = version, 
                  outdir = outdir, 
                  population = population, 
                  regions = regions, 
                  proportions = proportions)

