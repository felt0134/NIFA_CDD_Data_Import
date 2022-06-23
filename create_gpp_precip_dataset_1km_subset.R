
# setup

period_list <- seq(1, 15, 1) #set periods
period_list <-
  as.character(period_list) #easier when they are characters
year_list <- seq(2003, 2020, 1) #set years
year_list <-
  as.character(year_list) #easier when they are characters


# import GPP and Precip and combine (run once)
ecoregion_list <- c('shortgrass_steppe','northern_mixed_prairies')

for(Ecoregion in ecoregion_list){

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

#save this file so can just pull it out when re-running this code
filename <- paste0('./../../Data/GPP/Ecoregion/',Ecoregion,'/ppt_gpp_combined_1km.csv')
write.csv(ppt_gpp,filename)

}

