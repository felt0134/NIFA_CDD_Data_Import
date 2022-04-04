


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