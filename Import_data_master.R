
#https://cran.r-project.org/web/packages/progressr/vignettes/progressr-intro.html

#daymet climate data information
#https://daymet.ornl.gov/overview

#MODIS GPP data information
#https://lpdaac.usgs.gov/products/myd17a2hv061/

#import daymet precipitation data -----
library(daymetr)

#two ecoregions

#northern_mixed_prairies
#shortgrass_steppe
region_name = 'nebraska_sandhills' 
sandhills=F

#set year

#2003-2020
#year_value = '2003'


#runs script to access, format, and save data
years <- seq(2004,2020,1)
for(j in years){
  
  year_value = j
  
  #run script to access, format, and save data
  source('daymet_ppt_import_script.R')
  
}

#run script to access, format, and save data
source('daymet_ppt_import_script.R')

look <- raster(filename)
plot(look)


look.2 <- raster("./../../Data/Climate/Ecoregion/northern_mixed_prairies/Precipitation/Period/14/Precip_2012_14.tif")
plot(look.2)


#-------------------------------------------------------------------------------
#import MODIS GPP data -----

#load library for API:
library(MODISTools)

#two ecoregions:
#northern_mixed_prairies
#shortgrass_steppe
#nebraska_sandhills
sandhills=T
region_name = 'shortgrass_steppe'

#set year
#2003-2020

year_value = '2010'

#runs script to access, format, and save data
years <- seq(2011,2020,1)
for(j in years){
  
  year_value = j
  
  #run script to access, format, and save data
  source('gpp_import_script.R')
  
}

source('gpp_import_script.R')

look <- raster(filename)
summary(look)
plot(look)

look.2 <- raster("./../../Data/GPP/Ecoregion/northern_mixed_prairies/MODIS_GPP/Period/8/GPP_2019_8.tif")
plot(look.2)

#-------------------------------------------------------------------------------
# import MODIS NDVI data ------

#load library for API:
library(MODISTools)
region_name <- 'shortgrass_steppe'
#region_name <- 'northern_mixed_prairies'
#products <- mt_products()
year_value <- '2020'
source('ndvi_import_script.R')

#-------------------------------------------------------------------------------
#import daymet temp data -----
library(daymetr)

#two ecoregions

#northern_mixed_prairies
#region_name = 'shortgrass_steppe'
#nebraska_sandhills
sandhills=T
region_name = 'nebraska_sandhills'

#set year

#2003-2020 is length of GPP data
#lengthen data temp data so it starts in 1990 to get a full 30 years.

years <- seq(2001,2002,1)

for(j in years){

year_value = j
  
#run script to access, format, and save data
source('daymet_temp_import_script.R')
  
}


#-------------------------------------------------------------------------------
#import MODIS NDVI data

library(MODISTools)

#two ecoregions:
#northern_mixed_prairies
#shortgrass_steppe

#region_name = 'shortgrass_steppe'
region_name = 'northern_mixed_prairies'

#set year
#2003-2020

year_value = '2005'

source('ndvi_import_script.R')

years <- seq(2006,2010,1)

for(j in years){
  
  year_value = j
  
  #run script to access, format, and save data
  source('ndvi_import_script.R')
  
}






#import seasonal precip and temperature -----

library(daymetr)

#two ecoregions

#northern_mixed_prairies
#shortgrass_steppe
region_name = 'northern_mixed_prairies' 
#region_name = 'shortgrass_steppe' 
#sandhills=T

#set year

#2003-2020
year_value = '2012'

years <- seq(2014,2020,1)
for(j in years){
  
  year_value = j
  
  #run script to access, format, and save data
  source('seasonal_climate_import.R')
  
}

source('seasonal_climate_import.R')







#-------------------------------------------------------------------------------
#import 1 km subset of modis GPP and daymet ppt ------

library(MODISTools)
library(daymetr)
sandhills=F
region_name <- 'shortgrass_steppe'
region_name <- 'northern_mixed_prairies'

source('ppt_gpp_1km_subset_import.R')



