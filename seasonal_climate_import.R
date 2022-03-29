

# get seasonal precipitation and temperature 

#Author: A Felton
#This script imports and formats seasonal precipitation and temperature 

#set parallel processing
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
#?plan

#import and subset
rangeland_npp_covariates <- readRDS('./../../Data/Herbaceous_NPP_1986_2015_V2.rds')
head(rangeland_npp_covariates)
mean_production<-aggregate(npp~ x + y + region,mean,data=rangeland_npp_covariates)
sgs<-subset(mean_production,region==c(region_name))
#head(sgs)
sgs_raster <- rasterFromXYZ(sgs[c(1,2,4)])
plot(sgs_raster)

#increase pixel resolution
#check math
#https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
# 1/7 #want to pixel size to be 1/7 of original
# .0625*0.145
# 0.0090625*111 #concert to km. This equals about 1 km
# 1/0.145 #convert so its a factor
sgs_raster <- raster::disaggregate(sgs_raster,fact=7) #convert to ~1 km resolution
sgs_raster <- raster::aggregate(sgs_raster,fact=5.1) # 5 km resolution
#111*0.04464286 = ~5km
#convert back to df
sgs <- data.frame(rasterToPoints(sgs_raster))


#import daymet temp data ------

#https://daymet.ornl.gov
#https://cran.r-project.org/web/packages/daymetr/index.html


# summed_precip_list <- list()
# year_list <- list()
# period_values <- c(1:15)

#set this up first
#sgs.1000<-sgs[(1:50),]
sgs.1000 <- sgs

#test_function <- get_rainfall_period(period_id=2)

# id_values <- c(1:14)
# year_values <- c(2000:2020)
# year_id_list_2 <- list()

Ecoregion = region_name
#Ecoregion='NMP'


aea.proj <- "+proj=longlat +datum=WGS84"

# test_function<-get_rainfall_period_2(year_start = year_value,
#                     year_end = year_value)

handlers("txtprogressbar")

#spring temperature
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  spring_temp_list <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_spring_temp(i)
  })
})

spring_temp_list <- list_to_df(spring_temp_list)

spring_temp_list <- rasterFromXYZ(spring_temp_list)
crs(spring_temp_list) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/spring/spring_temp_',year_value,'_',Ecoregion,'.tif')
writeRaster(spring_temp_list,filename,overwrite=TRUE)
rm(spring_temp_list)

#summer temperature
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  summer_temp_list <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_summer_temp(i)
  })
})

summer_temp_list <- list_to_df(summer_temp_list)

summer_temp_list <- rasterFromXYZ(summer_temp_list)
crs(summer_temp_list) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/summer/summer_temp_',year_value,'_',Ecoregion,'.tif')
writeRaster(summer_temp_list,filename,overwrite=TRUE)
rm(summer_temp_list)

#spring precipitation (stopped here)
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  spring_precip_list <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_spring_precip(i)
  })
})

spring_precip_list <- list_to_df(spring_precip_list)

spring_precip_list <- rasterFromXYZ(spring_precip_list)
crs(spring_precip_list) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/spring/spring_precip_',year_value,'_',Ecoregion,'.tif')
writeRaster(spring_precip_list,filename,overwrite=TRUE)
rm(spring_precip_list)

#summer precipitation
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  summer_precip_list <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_summer_precip(i)
  })
})

summer_precip_list <- list_to_df(summer_precip_list)

summer_precip_list <- rasterFromXYZ(summer_precip_list)
crs(summer_precip_list) <- aea.proj
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/summer/summer_precip_',year_value,'_',Ecoregion,'.tif')
writeRaster(summer_precip_list,filename,overwrite=TRUE)
rm(summer_precip_list)


#get a randomized subset of ~500 sites for PRISM analysis ------

# library("splitstackshape")
# head(sgs.1000)
# ?stratified
# test_strat<-stratified(sgs.1000, c("y"), 0.015)
# #0.04 for sgs, 0.015 for nmp
# test_strat <- test_strat %>%
#   select(y,x) %>%
#   rename(latitude = y,
#          longitude = x)
# test_strat$site <- Ecoregion
# 
# write.csv(test_strat, 
#           paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/PRISM/',Ecoregion,'_vpd.csv'))
# 
# head(test_strat)

