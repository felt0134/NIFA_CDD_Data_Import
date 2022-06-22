
#create a combined NDVI and precipitation dataset for each ecoregion for analysis

period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# import GPP and Precip and combine (run once)
ecoregion_list <- c('shortgrass_steppe','northern_mixed_prairies')

for(Ecoregion in ecoregion_list){
  
  print(Ecoregion)
  
#list to store outputs in
ndvi_list <- list()  

#run the loop
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/ndvi/Ecoregion/',
        Ecoregion,
        '/MODIS_ndvi/Period/',
        i,
        '/'
      ),
      full.names = T
    )
  test <- lapply(filepath, format_ndvi_df)
  test <- data.frame(do.call('rbind', test))
  #test <- lapply(test,rasterToPoints)
  ndvi_list[[i]] <- test
  
}

#convert list of dataframes to a single dataframe
ndvi_df <- do.call('rbind', ndvi_list)
rm(ndvi_list, test) #get rid of excess stuff

#make unique id for each site
ndvi_df_mean <- aggregate(ndvi ~ x + y, mean, data = ndvi_df)
ndvi_df_mean$id_value <- seq.int(nrow(ndvi_df_mean))
#head(ndvi_df_mean,50)

#import conversion of period to day of year to map period on to DOY
doy_conversion <- read.csv('./../../Data/ndvi/period_day_match.csv')

#add on day of year and ID columns
ndvi_df <- merge(ndvi_df, doy_conversion[c(2, 3)], by = c('period'))
ndvi_df <- merge(ndvi_df, ndvi_df_mean[c(1, 2, 4)], by = c('x', 'y'))

#create a vector of unique sites IDs
id_list <- unique(ndvi_df$id_value)

# import ppt -----
ppt_list <- list()

#loop through each year and period
for (i in period_list) {
  filepath <-
    dir(
      paste0(
        './../../Data/Climate/Ecoregion/',
        Ecoregion,
        '/Precipitation/Period/',
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
ppt_ndvi <- merge(ndvi_df, ppt_df, by = c('x', 'y', 'year', 'period'))
#head(ppt_ndvi)
rm(ppt_df)


#save this file so can just pull it out when re-running this code
filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_ndvi_combined.csv')
write.csv(ppt_ndvi,filename)



}




