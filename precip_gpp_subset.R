
# get 100 random points to look at 1) total annual precip and 2) GPP throughout
# the year (% of GPP due to shoulder seasons)

#Author: A Felton
#This script imports 16-day summed estimates of growing season rainfall
#across the two rangeland ecoregions

#set parallel processing
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
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
  sgs_raster <- raster::aggregate(sgs_raster,fact=5.1) # 5 km resolution
  #111*0.04464286 = ~5km
  #convert back to df
  sgs <- data.frame(rasterToPoints(sgs_raster))
  
}else{
  
  
  sgs.raster <- raster("./../../Data/sandhills_biomass.tif")
  #plot(sgs.raster)
  sgs <- data.frame(rasterToPoints(sgs.raster))
  
  
}


#import daymet precipitation data ------

#https://daymet.ornl.gov
#https://cran.r-project.org/web/packages/daymetr/index.html


sgs.1000 <- sgs
nrow(sgs.1000)

#take a random sample of 100
sgs.1000 <- sgs.1000 %>%
  sample_n(100)

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

year_value_list <- seq(2003,2020,1)

#get growing season precip
gs_precip_list <- list()
for(year_value in year_value_list){

#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet(i)
  })
})
  
gs_precip_list[[year_value]] <- do.call('rbind',test_function)
  
}

gs_precip_df <- do.call('rbind',gs_precip_list)
gs_precip_df$window <- 'growing_season'
gs_precip_df <- aggregate(prcp..mm.day. ~ x+y+year+window,sum,data=gs_precip_df)
head(gs_precip_df,1)

gs_precip_df <- gs_precip_df %>%
  rename('gs_precip' = 'prcp..mm.day.')

#annual precip
annual_precip_list <- list()
for(year_value in year_value_list){
  
#get data and track progress
with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function_2 <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_daymet_annual(i)
  })
})
  
  annual_precip_list[[year_value]] <- do.call('rbind',test_function_2)
  
}

annual_precip_df <- do.call('rbind',annual_precip_list)
annual_precip_df$window <- 'annual'
annual_precip_df <- aggregate(prcp..mm.day. ~ x+y+year+window,sum,data=annual_precip_df)
head(annual_precip_df,1)

annual_precip_df <- annual_precip_df %>%
  rename('annual_precip' = 'prcp..mm.day.')

#merge
annual_gs_precip <- merge(annual_precip_df,gs_precip_df,by=c('x','y','year'))
head(annual_gs_precip)
plot(gs_precip ~ annual_precip,data=annual_gs_precip,
     xlab='Annual precipitation (mm)',ylab='March-October Precipitation')

annual_gs_precip <- annual_gs_precip %>%
  select(x,y,year,annual_precip,gs_precip)

#save file
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/annual_gs_subset.csv')
write_csv(annual_gs_precip,filename)

#cleanup memory
rm(gs_precip_list,annual_precip_list)

#now do the GPP subset
test_function_gpp_list <- list()

for(year_value in year_value_list){

  with_progress({
  p <- progressor(along = 1:nrow(sgs.1000))
  test_function_gpp <- future_lapply(1:nrow(sgs.1000), function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    get_modis_gpp_period_annual(i)
  })
})

  test_function_gpp_list[[year_value]] <-  do.call('rbind',test_function_gpp)
  
}
  
test_function_gpp_df <- data.frame(do.call('rbind',test_function_gpp_list))
head(test_function_gpp_df)

#save file
filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/full_year_subset.csv')
write_csv(test_function_gpp_df ,filename)
rm(test_function_gpp_list,test_function_gpp_df)
