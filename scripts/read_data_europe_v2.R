library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(raster)
library(geoR)
library(sf)
library(spData)
library(viridis)

spain_bbox = c(-10.59,35.61,4.48,43.99)
france_bbox = c(-5.49,41.94,8.57,51.36)

csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"

data <- read.csv(csv_loc)
data <- data %>% dplyr::select(-X) %>% dplyr::select(Longitude, Latitude, X1989_Primavera)
names(data) <- c('longitude', 'latitude', 'temp')


data(world)
spain_shp <- world %>% filter(name_long == 'Spain') %>% dplyr::select(geom)
france_shp <- world %>% filter(name_long == 'France') %>% dplyr::select(geom)


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
    geom_sf(data = spain_shp$geom,
            fill = NA,
            size = 1.5) +
    geom_point(data = sp_data,
               aes(x = longitude,
                   y = latitude,
                   color = temp
               ),
               alpha = .5) +
    scale_color_viridis(option = "magma", direction = -1) +
    coord_sf(datum=st_crs(4326))


## 75% of the sample size
smp_size <- floor(0.7 * nrow(sp_data))

## set the seed to make your partition reproducible
set.seed(42)
train_ind <- sample(seq_len(nrow(sp_data)), size = smp_size)


ggplot() +
    geom_sf(data = spain_shp$geom,
            fill = NA,
            size = 1.5) +
    geom_point(data = sp_data %>% mutate(is_train = if_else(row_number() %in% train_ind,
                                                            'Train',
                                                            'Testing')
                                         ),
               aes(x = longitude,
                   y = latitude,
                   colour = as.factor(is_train)
               ),
               alpha = 1,
               size = 2,
               shape = 16) +
    # scale_colour_discrete(name = "Data Type",
    #                     labels = c("Training", "Testing")) +
    facet_wrap(vars(is_train))+
    labs(x = 'Longitud',
         y = 'Latitud',
         colour = "Data type")+
    coord_sf(datum=st_crs(4326))

sp_data_final <- sp_data[train_ind, ]
sp_data_bin <- sp_data[-train_ind, ]

plot(as.geodata(cbind(sp_data_final$longitude,sp_data_final$latitude,sp_data_final$temp)))
ggplot() +
    geom_sf(data = spain_shp$geom) +
    geom_point(data = sp_data_final,
               aes(x = longitude,
                   y = latitude,
                   color = temp
               ),
               alpha = .5) +
    scale_color_gradient2(low = "blue",
                          high = "red",
                          space = "Lab") +
    coord_sf(datum = st_crs(4326))


sp_geo_data <- as.geodata(cbind(sp_data$longitude,sp_data$latitude,sp_data$temp))
plot(sp_geo_data)


save(sp_data, sp_data_final, sp_data_bin, spain_shp,
     file = "spain_geodata_temperature_1989_spring.RData")


fr_data <- data %>% dplyr::filter(longitude >= france_bbox[1],
                                  longitude <= france_bbox[3],
                                  latitude >= france_bbox[2],
                                  latitude <= france_bbox[4])
fr_data <- fr_data %>% filter(temp > -20)

ggplot() +
    geom_sf(data = france_shp$geom) +
    xlim(-6, 9) +
    ylim(41.5, 51.5) +
    geom_point(data = fr_data,
               aes(x = longitude,
                   y = latitude,
                   color = temp
               ),
               alpha = .5) +
    scale_color_gradient2(low = "blue",
                          midpoint = 5,
                          mid = "yellow",
                          high = "red",
                          space = "Lab") +
    coord_sf(datum = st_crs(4326))


fr_data <- fr_data %>% filter(!between(longitude, -6, 1) | !between(latitude, 49.9, 52))
fr_data <- fr_data %>% filter(!between(longitude, -6, -2) | !between(latitude, 42, 44))
fr_data <- fr_data %>% filter(!between(longitude, 4, 9) | !between(latitude, 50, 52))
fr_data <- fr_data %>% filter(!between(longitude, 7.5, 9) | !between(latitude, 42, 52))
fr_data <- fr_data %>% filter(!between(longitude, -2, 4) | !between(latitude, 42, 42.5))

fr_geo <- as.geodata(cbind(fr_data$longitude,fr_data$latitude,fr_data$temp))
plot(fr_geo)

## sampling france dataset
smp_size <- floor(0.6 * nrow(fr_data))

## set the seed to make your partition reproducible
set.seed(42)
train_ind <- sample(seq_len(nrow(fr_data)), size = smp_size)

fr_data_final <- fr_data[train_ind, ]
fr_data_bin <- fr_data[-train_ind, ]

sp_geo_data <- as.geodata(cbind(fr_data_final$longitude,fr_data_final$latitude,fr_data_final$temp))
plot(sp_geo_data)

save(fr_data, fr_data_final, fr_data_bin, france_shp,
     file = "france_geodata_temperature_1989_spring.RData")
