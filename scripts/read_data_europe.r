library(dplyr)
library(readr)
library(ggplot2)
library(ggmap)
library(raster)
library(geoR)
library(sf)
library(spData)
#devtools::install_github("paleolimbot/ggspatial")

## Leemos el dataset inicial
# -6.7604, 2.2474,
# 49.7665, 54.5731


lat_range = c(-6 , 2)
lon_range = c(50 , 54.0)


csv_loc = "data/europe-temp/europe_temp_spring_1989.csv"

data <- read.csv(csv_loc)
data <- data %>% dplyr::select(-X)

names(data) <- c('longitude', 'latitude', 'temp' )

sliced_data <- data %>% dplyr::filter(longitude >= lon_range[1],
                               longitude <= lon_range[2],
                               latitude >= lat_range[1],
                               latitude <= lat_range[2])

sliced_data %>% filter(temp > -10) %>%
    ggplot(aes(x=latitude, y= longitude, col = temp)) + geom_point()
min(sliced_data$longitude)
max(sliced_data$longitude)
min(sliced_data$latitude)
max(sliced_data$latitude)

## Eliminamos una parte de francia que se coló

# 1.1213155 , 1.8903584687
# 49.950032124 , 51.0125932806

sliced_data <- sliced_data %>% filter(!between(latitude, -0.5, 2.5) | !between(longitude, 49.5, 50.9))
sliced_data <- sliced_data %>% filter(!between(latitude, -4, 0) | !between(longitude, 49.5, 50.4))
sliced_data <- sliced_data %>% filter(!between(latitude, 1.4, 2.1) | !between(longitude, 50.9, 51.6))

sliced_data <- sliced_data %>% filter(temp > -10)
sliced_data %>% ggplot(aes(x=latitude, y= longitude, col = temp)) + geom_point()

gb_temp_data <- sliced_data

gb_temp_data <- gb_temp_data %>% transform(pato = rnorm(temp))

coordinates(gb_temp_data) <- ~longitude+latitude
proj4string(gb_temp_data) <- CRS("+init=epsg:4267 +proj=longlat +ellps=clrk66 +datum=NAD27")

plot(gb_temp_data)

save(gb_temp_data, file = "england_geodata_temperature_1989_spring.RData")


## otros testings para visualizar!

data(world)

uk_map <- world %>% filter(name_long == 'United Kingdom') %>% dplyr::select(geom)

ggplot() +
    geom_sf(data = uk_map$geom) +
    geom_point(data = sliced_data,
                    aes(x = latitude,
                        y = longitude,
                        color = temp
                    ),
                    alpha = .5) +
    coord_sf(datum=st_crs(4326))

## Hay un problema con la forma de datos entre el mapa y las temp (hay q pasar la longitud
## a sexagesimal que tenga en vez de 49.5 a 49° 30' 00''N)


## genero grilla de datos para geodata


data_geo <-cbind(sliced_data$lon,sliced_data$lat,sliced_data$temp)
vg<-as.geodata(data_geo)
plot(vg)
