#Author: A Felton
#This script imports 16-day summed estimates of growing season rainfall
#across the two rangeland ecoregions

#set parallel processing
plan(multisession, workers = 8)
options(future.globals.maxSize = 8000 * 1024^2)
#?plan

if(sandhills==F){
  
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
  #sgs_raster <- raster::aggregate(sgs_raster,fact=3.1) # 5 km resolution
  #110.567*0.008928571 = ~1km
  #convert back to df
  sgs <- data.frame(rasterToPoints(sgs_raster))
  
}else{
  
  
  sgs.raster <- raster("./../../Data/sandhills_biomass.tif")
  #plot(sgs_raster)
  sgs <- data.frame(rasterToPoints(sgs.raster))
  
  
}
?stratified
#take a stratified (by latitude)/randomized subset of the data
library("splitstackshape")
#head(sgs.1000)

# if(region_name=='northern_mixed_prairies'){
# 
# sgs <- stratified(sgs, c("y"), 0.015)}else{
#   
#   sgs <- stratified(sgs, c("y"), 1.1)
#   
# }

#randomly sample 200 sites
set.seed(100)
sgs <- sgs %>%
  dplyr::sample_n(size=200)



#import daymet precipitation data ------

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

#loop this sequences through all years
period_list_2 <- seq(2003,2020,1)

for(j in period_list_2){
  year_value = j

#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function_ppt <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet(i)
  })
})

test_function_ppt <- do.call('rbind',test_function_ppt)


period_list <- c(1:15)
for(i in period_list){
  #i <- 1
  #test_function$year <- as.numeric(test_function$year)
  #str(test_function)
  test_function_4_ppt <-subset(test_function_ppt,period==i)  
  
  #year_id_list_2[[i]] <- test_function
  
  #year_id_df <- data.frame(year_id_list_2[[i]])
  year_id_df_ppt <- test_function_4_ppt[c(4,5,1)]
  
  year_id_raster_ppt <- rasterFromXYZ(year_id_df_ppt)
  crs(year_id_raster_ppt) <- aea.proj
  #plot(year_id_raster_ppt)
  filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation_1km/Period/',i,'/Precip_',year_value,'_',i,'.tif')
  writeRaster(year_id_raster_ppt,filename,overwrite=TRUE)
  
}


#now import GPP data

#get dates
start_date = paste0(year_value,"-02-18")
end_date = paste0(year_value,"-11-24")

with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function_gpp <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_modis_gpp_period_1km(i)
  })
})

test_function_gpp <- do.call('rbind',test_function_gpp)

for(i in period_list){
  
  #test_function$year <- as.numeric(year_value)
  test_function_gpp_4 <-subset(test_function_gpp,period==i)
  test_function_gpp_4  <- test_function_gpp_4  %>%
    dplyr::filter(gpp_mean < 6000) #remove 6552.4 values
  
  #fix lat/lon
  year_id_df_gpp <- test_function_gpp_4[c(1,2,3)] 
  #year_id_df <- test_function_4[c(1,2,3)] 
  colnames(year_id_df_gpp) <- c('x','y','gpp')
  
  year_id_raster_gpp <- rasterFromXYZ(year_id_df_gpp)
  crs(year_id_raster_gpp) <- aea.proj
  #plot(year_id_raster_gpp)
  filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP_1km/Period/',i,'/GPP_',year_value,'_',i,'.tif')
  writeRaster(year_id_raster_gpp,filename,overwrite=TRUE)
  
}

}

# 110.567*.044
# 110.567*0.008928571

#