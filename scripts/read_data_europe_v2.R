library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(raster)
library(geoR)
library(sf)
library(spData)

spain_bbox = c(-10.59,35.61,4.48,43.99)
france_bbox = c(-5.49,41.94,8.57,51.36)

csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"

data <- read.csv(csv_loc)
data <- data %>% dplyr::select(-X)
names(data) <- c('latitude', 'longitude', 'temp')


data(world)
spain_shp <- world %>% filter(name_long == 'Spain') %>% dplyr::select(geom)
france_shp <- world %>% filter(name_long == 'Spain') %>% dplyr::select(geom)


ggplot() +
    geom_sf(data = spain_shp$geom) +
    geom_point(data = data,
               aes(x = longitude,
                   y = latitude,
                   color = temp
               ),
               alpha = .5) +
    coord_sf(datum=st_crs(4326))


sp_data <- data %>% dplyr::filter(longitude >= spain_bbox[1],
                                  longitude <= spain_bbox[3],
                                  latitude >= spain_bbox[2],
                                  latitude <= spain_bbox[4])

fr_data <- data %>% dplyr::filter(longitude >= france_bbox[1],
                                      longitude <= france_bbox[3],
                                      latitude >= france_bbox[2],
                                      latitude <= france_bbox[4])

sp_data <- sp_data %>% filter(temp > -20)
fr_data <- fr_data %>% filter(temp > -20)

sp_data <- sp_data %>% filter(!between(longitude, -1.5, 4) | !between(latitude, 35.9, 38.1))
sp_data <- sp_data %>% filter(!between(longitude, -10.1, -7.9) | !between(latitude, 36, 41.9))
sp_data <- sp_data %>% filter(!between(longitude, -1.9, 4) | !between(latitude, 42.9, 44))
sp_data <- sp_data %>% filter(!between(longitude, 0, 4) | !between(latitude, 8.9, 40.6))

ggplot() +
    geom_sf(data = spain_shp$geom) +
    geom_point(data = sp_data,
               aes(x = longitude,
                   y = latitude,
                   color = temp
               ),
               alpha = .5) +
    coord_sf(datum=st_crs(4326))




data_geo <-cbind(sp_data$longitude,sp_data$latitude,sp_data$temp)
vg<-as.geodata(data_geo)
plot(vg)
