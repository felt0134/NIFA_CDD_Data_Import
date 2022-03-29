#Author: A Felton
#This script imports 16-day summed estimates of growing season rainfall
#across the two rangeland ecoregions

#set parallel processing
plan(multisession, workers = 10)
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

#batch 1 (1st half of data)
# midpoint <- round(nrow(sgs)/2)
# sgs.1000 <- sgs[1:midpoint,]

#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_temp(i)
  })
})

test_function <- list_to_df(test_function)
# head(test_function)

#batch 2 (2nd half of data)
# sgs.1000 <- sgs[(midpoint +1):nrow(sgs),]
# 
# #get data and track progress
# with_progress({
#   p <- progressor(along = 1:nrow(sgs.1000))
#   test_function_2 <- future_lapply(1:nrow(sgs.1000), function(i) {
#     Sys.sleep(0.1)
#     p(sprintf("i=%g", i))
#     get_daymet(i)
#   })
# })
# 
# test_function_2 <- list_to_df(test_function_2)
# 
# test_function_3 <- rbind(test_function,test_function_2)
# 
# 
#   #year_id_df <- data.frame(year_id_list_2[[i]])
#   year_id_df <- test_function_3[c(4,5,1)]
  
  #tmax
  tmax <- rasterFromXYZ(test_function[c(5,6,1)])
  crs(tmax) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/tmax/Tmax_',year_value,'_',Ecoregion,'.tif')
  writeRaster(tmax,filename,overwrite=TRUE)
  
  #tmean
  tmean <- rasterFromXYZ(test_function[c(5,6,3)])
  crs(tmean) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/tmean/Tmean_',year_value,'_',Ecoregion,'.tif')
  writeRaster(tmean,filename,overwrite=TRUE)
  
  #tmin
  tmin <- rasterFromXYZ(test_function[c(5,6,2)])
  crs(tmin) <- aea.proj
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Temperature/tmin/Tmin_',year_value,'_',Ecoregion,'.tif')
  writeRaster(tmin,filename,overwrite=TRUE)
  

#