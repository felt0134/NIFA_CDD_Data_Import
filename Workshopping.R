


#workshopping

#importing modis NDVI -----
get_modis_ndvi <- function(i) {
  
  temp_lat <- sgs.1000[i,] %>% pull(y)
  temp_lon <- sgs.1000[i,] %>% pull(x)
  
  #get GPP data
  site_ndvi <- mt_subset(
    product = "MYD13Q1",
    lat = temp_lat,
    lon =  temp_lon,
    band = '250m_16_days_NDVI',
    start = start_date,
    end = end_date,
    km_lr = 5,
    km_ab = 5,
    site_name = Ecoregion,
    internal = TRUE,
    progress = TRUE
  )
  
  #filter out bad values, get day of year, take median value for coordinate, and rescale 
  site_ndvi_2  <- site_ndvi  %>%
    #filter(value <= X) %>% if there is a threshold value to filter by
    group_by(calendar_date) %>%
    summarize(doy = as.numeric(format(as.Date(calendar_date)[1], "%j")),
              ndvi_mean = median(value * as.double(scale))) %>%
    filter(doy > 60) %>%
    filter(doy < 300)
  
  #plot(ndvi_mean~doy, data=site_ndvi_2)
  
  #get year column
  site_ndvi_2$year <- substr(site_ndvi_2$calendar_date, 1, 4)
  
  #filter out years with incomplete data
  site_length = aggregate(doy ~ year, length, data = site_ndvi_2)
  colnames(site_length) = c('year', 'length')
  site_ndvi_2 = merge(site_ndvi_2, site_length, by = 'year')
  
  
  site_ndvi_2$period <- as.numeric(rownames(site_ndvi_2))
  
  site_ndvi_2$y <- temp_lat
  site_ndvi_2$x <- temp_lon
  
  site_ndvi_2 <- site_ndvi_2 %>%
    select(x,y, ndvi_mean,period)
  
  
  return(site_ndvi_2)
  
}
#importing finer resolution gpp/ppt -----

Ecoregion <- 'shortgrass_steppe'
year_value <- 2003
i <- 10
filename <- paste0('./../../Data/Climate/Ecoregion/',Ecoregion,'/Precipitation/Period/',i,'/Precip_',year_value,'_',i,'.tif')
test <- raster(filename)
plot(test)

filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/MODIS_GPP/Period/',i,'/GPP_',year_value,'_',i,'.tif')
gpp_test <- raster(filename)
plot(gpp_test)


#get % reduction in GPP throughout the growing season

# setup----
library(plotrix)
plan(multisession, workers = 10)
options(future.globals.maxSize = 8000 * 1024^2) #https://github.com/satijalab/seurat/issues/1845
period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# first do the GPP import----

#loop through each year and period combination

#list to store outputs in
gpp_list <- list()

#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/GPP/Ecoregion/',
        Ecoregion,
        '/MODIS_GPP_1km/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test <- lapply(filepath, format_gpp_df)
  test <- data.frame(do.call('rbind', test))
  #test <- lapply(test,rasterToPoints)
  gpp_list[[i]] <- test
  
}

#convert list of dataframes to a single dataframe
gpp_df <- do.call('rbind', gpp_list)
rm(gpp_list, test) #get rid of excess stuff

#make unique id for each site
gpp_df_mean <- aggregate(gpp ~ x + y, mean, data = gpp_df)
gpp_df_mean$id_value <- seq.int(nrow(gpp_df_mean))
#head(gpp_df_mean)

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/GPP/period_day_match.csv')

#add on day of year and ID columns
gpp_df <- merge(gpp_df, doy_conversion[c(2, 3)], by = c('period'))
gpp_df <- merge(gpp_df, gpp_df_mean[c(1, 2, 4)], by = c('x', 'y'))

#create a vector of unique sites IDs
id_list <- unique(gpp_df$id_value)

# import ppt -----
ppt_list <- list()

#loop through each year and period
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/Climate/Ecoregion/',
        Ecoregion,
        '/Precipitation_1km/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test_ppt <- lapply(filepath, format_ppt_df)
  test_ppt <- data.frame(do.call('rbind', test_ppt))
  ppt_list[[i]] <- test_ppt
  
}

#convert to dataframe
ppt_df <- do.call('rbind', ppt_list)
rm(ppt_list, test_ppt) #remove excess data
#head(ppt_df)

#merge the two dataframes by location, year, and period within each year
ppt_gpp <- merge(gpp_df, ppt_df, by = c('x', 'y', 'year', 'period'))
#head(ppt_gpp)
rm(ppt_df)


# get splines -----

#get average growth 
with_progress({
  p <- progressor(along = id_list)
  growth_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline(i)
  })
})


#now do drought
with_progress({
  p <- progressor(along = id_list)
  growth_drought_spline_list <- future_lapply(id_list, function(i) {
    Sys.sleep(0.1)
    p(sprintf("i=%g", i))
    gpp_spline_drought(i)
  })
})

#get each 95% CI for each day of the prediction
doy_list <- c(65:297)

#loop
gpp_predicted_list_average <- list()
gpp_predicted_list_drought <- list()
gpp_reduction_list <- list()
gpp_reduction_list_2 <- list()
#test_list <- c(1:10)

for(i in doy_list){
  
  for(j in id_list){
    
    #average
    gpp_predicted_average <- data.frame(predict(growth_spline_list[[j]], i))
    gpp_predicted_average$id_val <- j
    gpp_predicted_list_average[[j]] <- gpp_predicted_average
    
    #drought
    gpp_predicted_drought <- data.frame(predict(growth_drought_spline_list[[j]], i))
    gpp_predicted_drought$id_val <- j
    gpp_predicted_list_drought[[j]] <- gpp_predicted_drought
    
  }
  
  #convert to dataframe and remove values below zero
  gpp_predicted_list_average_df <- list_to_df(gpp_predicted_list_average)
  colnames(gpp_predicted_list_average_df) <- c('doy','gpp_average','id_val')
  gpp_predicted_list_average_df <- gpp_predicted_list_average_df %>%
    filter(gpp_average > 0)
  
  gpp_predicted_list_drought_df <- list_to_df(gpp_predicted_list_drought)
  colnames(gpp_predicted_list_drought_df) <- c('doy','gpp_drought','id_val')
  gpp_predicted_list_drought_df <- gpp_predicted_list_drought_df %>%
    filter(gpp_drought > 0)
  
  # hist(gpp_predicted_list_average_df$gpp_average)
  # summary(gpp_predicted_list_average_df)
  # hist(gpp_predicted_list_drought_df$gpp_drought)
  # summary(gpp_predicted_list_drought_df)
  
  gpp_predicted_drought_average <- merge(gpp_predicted_list_drought_df,gpp_predicted_list_average_df,
                                         by=c('doy','id_val'))
  
  ss <- nrow(gpp_predicted_drought_average)
  
  gpp_predicted_drought_average_3 <- gpp_predicted_drought_average #use for absolute
  
  #relative
  gpp_predicted_drought_average$perc_change <- ((gpp_predicted_drought_average$gpp_drought -
    gpp_predicted_drought_average$gpp_average)/gpp_predicted_drought_average$gpp_average)*100

  #get median
  gpp_predicted_drought_average_2 <- aggregate(perc_change~doy,median,data=gpp_predicted_drought_average)

  #get and add 99% CI
  gpp_predicted_drought_average_2$ci_99 <- std.error(gpp_predicted_drought_average$perc_change)*2.576
  gpp_predicted_drought_average_2$sample_size <- ss
  gpp_reduction_list[[i]] <- gpp_predicted_drought_average_2
  
  #absolute
  gpp_predicted_drought_average_3$abs_change <- gpp_predicted_drought_average_3$gpp_drought -
    gpp_predicted_drought_average$gpp_average
  
  #get median
  gpp_predicted_drought_average_4 <- aggregate(abs_change~doy,median,data=gpp_predicted_drought_average_3)
  
  #get and add 99% CI
  gpp_predicted_drought_average_4$ci_99 <- std.error(gpp_predicted_drought_average_3$abs_change)*2.576
  gpp_predicted_drought_average_4$sample_size <- ss
  gpp_reduction_list_2[[i]] <- gpp_predicted_drought_average_4
  
}

gpp_reduction_list_df <- list_to_df(gpp_reduction_list)
head(gpp_reduction_list_df,1)

filename <- paste0('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_1km_',Ecoregion,'.csv')
write.csv(gpp_reduction_list_df,filename)

gpp_reduction_list_df_2 <- list_to_df(gpp_reduction_list_2)
head(gpp_reduction_list_df_2,1)

filename <- paste0('./../../Data/growth_dynamics/one_km_subset/drought_gpp_reduction_absolute_1km_',Ecoregion,'.csv')
write.csv(gpp_reduction_list_df_2,filename)

rm(gpp_df_mean,gpp_predicted_average,gpp_predicted_drought,gpp_predicted_drought_average,
   gpp_predicted_drought_average_2,gpp_predicted_list_average,gpp_predicted_list_average_df,
   gpp_predicted_list_drought,gpp_predicted_list_drought_df,gpp_reduction_list,
   growth_drought_spline_list,growth_spline_list,gpp_reduction_list_2,gpp_predicted_drought_average_3,
   gpp_predicted_drought_average_4)


#plot this out ------
# str(gpp_reduction_list_df)
# gpp_reduction_list_df$upper <- gpp_reduction_list_df$perc_change + gpp_reduction_list_df$ci_99
# gpp_reduction_list_df$lower <- gpp_reduction_list_df$perc_change - gpp_reduction_list_df$ci_99
# plot(perc_change~doy,data=gpp_reduction_list_df,cex=0.1,
#      xlab='Julian day',ylab='Drought impact (% change in GPP)')
# lines(perc_change~doy,data=gpp_reduction_list_df)
# lines(upper~as.numeric(as.integer(doy)),gpp_reduction_list_df)
# lines(lower~doy,gpp_reduction_list_df)
# abline(h=0)
# 
# gpp.doy.spl <-
#   with(gpp_reduction_list_df, smooth.spline(doy, perc_change))
#lines(gpp.doy.spl, col = "blue")

# #import and merge
# gpp_reduction_list_df <- read.csv(paste0('./../../Data/growth_dynamics/drought_gpp_reduction_',Ecoregion,'.csv'))
# gpp_mean_list_df <- read.csv(paste0('./../../Data/growth_dynamics/average_gpp_',Ecoregion,'.csv'))
# 
# normal_drought_df <- merge(gpp_mean_list_df_2[c(2,3,5)],gpp_mean_list_df[c(2,3,5)],by='doy')
# head(normal_drought_df)
# 
# normal_drought_df$perc_change <- 
#   ((normal_drought_df$mean.x - normal_drought_df$mean.y)/normal_drought_df$mean.y)*100
# 
# head(normal_drought_df)
# plot(perc_change ~ doy,data=normal_drought_df,xlab='Julian day',ylab='Drought impact (% change in GPP)')
# abline(h=0)


# gpp subset ------


test_function_gpp_list <- list()

year_value_list = 2003

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




i=100
#get dates
start_date = paste0(year_value,"-01-01")
end_date = paste0(year_value,"-12-20")

temp_lat <- sgs.1000[i,] %>% pull(y)
temp_lon <- sgs.1000[i,] %>% pull(x)

#get GPP data
site_gpp <- mt_subset(
  product = "MYD17A2H",
  lat = temp_lat,
  lon =  temp_lon,
  band = 'Gpp_500m',
  start = start_date,
  end = end_date,
  km_lr = 5,
  km_ab = 5,
  site_name = Ecoregion,
  internal = TRUE,
  progress = TRUE
)

#filter out bad values, get day of year, take median value for coordinate, and rescale GPP units to g/m^2
site_gpp_2  <- site_gpp  %>%
  #filter(value <= X) %>% if there is a threshold value to filter by
  group_by(calendar_date) %>%
  summarize(doy = as.numeric(format(as.Date(calendar_date)[1], "%j")),
            gpp_mean = median(value * as.double(scale))) %>%
  filter(doy > 0) %>%
  filter(doy < 365)

#get gpp in grams
site_gpp_2$gpp_mean <- site_gpp_2$gpp_mean * 1000

#get year column
site_gpp_2$year <- substr(site_gpp_2$calendar_date, 1, 4)

#filter out years with incomplete data
# site_length = aggregate(doy ~ year, length, data = site_gpp_2)
# colnames(site_length) = c('year', 'length')
# site_gpp_2 = merge(site_gpp_2, site_length, by = 'year')
# site_gpp_2 = site_gpp_2 %>%
#   dplyr::filter(length > 29)

# site_gpp_3 <- get_16_day_sums_gpp(site_gpp_2)
# site_gpp_3$period <- as.numeric(rownames(site_gpp_3))
# site_gpp_3$period = (site_gpp_3$period + 1)

site_gpp_2$x <- temp_lat
site_gpp_2$y <- temp_lon
site_gpp_2 <- site_gpp_2 %>%
  select(x,y,year,doy,calendar_date,gpp_mean)



return(site_gpp_2)

}