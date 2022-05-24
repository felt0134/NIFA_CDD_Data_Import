#GPP import
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
#plot(sgs_raster)

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

#set this up first, the function uses this (originally I varied # of sites for testing)
sgs.1000<-sgs
#sgs.1000 <- sgs[1:50,]


#get dates
start_date = paste0(year_value,"-02-18")
end_date = paste0(year_value,"-11-24")

#set up ecoregion nae to save to file
Ecoregion = region_name

aea.proj <- "+proj=longlat +datum=WGS84"

#test_function<-get_modis_gpp_period_2(start_val = start_date,end_val = end_date)

handlers("txtprogressbar")

#batch 1
midpoint <- round(nrow(sgs)/2)
sgs.1000 <- sgs[1:midpoint,]

#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_modis_ndvi(i)
  })
})

test_function <- do.call('rbind',test_function)
# plot(gpp_mean~period,data=test_function)

#batch 2
sgs.1000 <- sgs[(midpoint +1):nrow(sgs),]

#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function_2 <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_modis_ndvi(i)
  })
})

test_function_2 <- do.call('rbind',test_function_2)

test_function_3 <- rbind(test_function,test_function_2)
rm(test_function,test_function_2)
#head(test_function_3)
#summary(test_function_3)

period_list <- c(1:15)
for(i in period_list){
  
  #test_function$year <- as.numeric(year_value)
  test_function_4 <- subset(test_function_3,period==i)
  test_function_4 <- test_function_4 %>%
    dplyr::filter(ndvi_mean < 1000) %>% #remove high values
    dplyr::filter(ndvi_mean > -2000)
  
  #get into an XYZ
  year_id_df <- test_function_4[c(1,2,3)] 
  colnames(year_id_df) <- c('x','y','ndvi')
  year_id_raster <- rasterFromXYZ(year_id_df)
  crs(year_id_raster) <- aea.proj
  filename <- paste0('./../../Data/NDVI/Ecoregion/',Ecoregion,'/MODIS_NDVI/Period/',i,'/NDVI_',year_value,'_',i,'.tif')
  writeRaster(year_id_raster,filename,overwrite=TRUE)
  
}


